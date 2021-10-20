-- local show do
--   local inspect = require'inspect'
--   function show(...)
--     return print(inspect(...))
--   end
-- end

local readu8 = fio.readcardinal1
local readu16 = fio.readcardinal2
local readu32 = fio.readcardinal4
local reads32 = fio.readinteger4
local readcardinaltable = fio.readcardinaltable
local readintegertable = fio.readintegertable
local getposition = fio.getposition
local setposition = fio.setposition
local skipposition = fio.skipposition

local function from_utf16be(s)
  local codepoints = {}
  local i = 0
  local cached
  for high, low in string.bytepairs(s) do
    if not low then return nil, "Invalid number of bytes for UTF-16 string" end
    if high & 0xF8 == 0xD8 then
      if high & 4 == 4 then
        if cached then return nil, "Invalid surrogate in UTF-16 string" end
        cached = ((high & 3) << 18) | (low << 10)
      else
        if not cached then return nil, "Invalid surrogate in UTF-16 string" end
        i = i+1
        codepoints[i] = 0x10000 + (cached | ((high & 3) << 2) | low)
        cached = nil
      end
    else
      if cached then return nil, "Invalid surrogate in UTF-16 string" end
      i = i+1
      codepoints[i] = (high << 8) + low
    end
  end
  if cached then return nil, "Invalid surrogate in UTF-16 string" end
  return utf8.char(table.unpack(codepoints))
end

local function read_date(f)
  local year = readu16(f)
  local month = readu16(f) -- 1 to 12
  local day = readu16(f) -- 1 to 31
  local hour = readu16(f)
  local minute = readu16(f)
  local second = readu16(f) -- 0-59 (no leap seconds)
  return {
    year = year,
    month = month,
    day = day,
    hour = hour,
    min = minute,
    sec = second
  } -- os.time compatible except for the time zone
end

local function read_position_number(f)
  local offset = readu32(f)
  local size = readu32(f)
  return offset, size
end

local function read_response16(f)
  local device = readu16(f)/65535.
  readu16(f) -- reserved, shall be zero
  local measurement = readu32(f)
  return device, measurement
end

local function read_fixeds16(f) return reads32(f)/65536. end

local function read_fixeds16table(f, n)
  local ret = readintegertable(f, n, 4)
  for i = 1, n do ret[i] = ret[i]/65536. end
  return ret
end


local function read_fixedu16(f) return readu32(f)/65536. end

local function read_fixedu15(f) return readu16(f)/32768. end

local function read_fixedu8(f)  return readu16(f)/256.   end

local function read_xyz(f)
  local x = read_fixeds16(f)
  local y = read_fixeds16(f)
  local z = read_fixeds16(f)
  return x, y, z
end

local function read_tag(f)
  local tag = f:read(4)
  if tag == '\0\0\0\0' then return nil end
  return tag:gsub(" *$", "")
end

local function read_localized_unicode(f, size, pos0)
  readu32(f)
  local count = readu32(f)
  local record_size = readu32(f)
  local records = {}
  for i=1, count do
    local lang = f:read(2)
    local country = f:read(2)
    local size = readu32(f)
    local offset = readu32(f)
    records[#records+1] = {lang, country, size, offset}
    skipposition(f, record_size-12)
  end

  local strings = {}
  for i=1, count do
    local record = records[i]
    setposition(f, pos0 + record[4])
    local str, err = from_utf16be(f:read(record[3]))
    if not str then return nil, err end
    local lang = strings[record[1]]
    if lang then
      lang[record[2]] = str
    else
      if i == 1 then strings[''] = str end
      strings[record[1]] = {[''] = str, [record[2]] = str}
    end
  end
  return strings
end

local pcs_illuminant_x = (0.9642 * 65536 + .5) // 1 / 65536
local pcs_illuminant_y = (1 * 65536 + .5) // 1 / 65536
local pcs_illuminant_z = (0.8249 * 65536 + .5) // 1 / 65536

local function read_header(f)
  local info = {}
  info.size = readu32(f)
  info.preferred_cmm = read_tag(f)
  info.version = readu32(f)
  if info.version >= 0x05000000 then
    return nil, [[Unsupported version]]
  end
  info.profile_class = read_tag(f)
  info.colorspace_a = read_tag(f)
  info.colorspace_pcs = read_tag(f)
  info.creation_date = read_date(f)
  if read_tag(f) ~= "acsp" then
    return nil, [[Not a profile file]]
  end
  info.primary_platform = read_tag(f)
  info.flags = readu32(f)
  info.device_manuf = read_tag(f)
  info.device_model = read_tag(f)
  info.device_flags_vendor = readu32(f)
  info.device_flags_icc = readu32(f)
  info.rendering_intent = readu32(f) & 0xFFFF
  do
    local x, y, z = read_xyz(f)
    if math.max(math.abs(x - pcs_illuminant_x), math.abs(y - pcs_illuminant_y), math.abs(z - pcs_illuminant_z)) > 0.001 then
      -- print(x, pcs_illuminant_x, y, pcs_illuminant_y, z, pcs_illuminant_z)
      return nil, [[Invalid illuminant]]
    end
  end
  info.creator = read_tag(f)
  info.id = f:read(16)
  if info.id == '\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0' then
    info.id = nil
  end
  skipposition(f, 28)
  return info
end

local function read_tags(f)
  local count = readu32(f)
  local tag_map = {}
  for i = 1, count do
    local tag = read_tag(f)
    if tag_map[tag] then
      return nil, [[Duplicate tag]]
    end
    tag_map[tag] = {read_position_number(f)}
  end
  return tag_map
end

local function clip(x)
  return x < 0 and 0 or x > 1 and 1 or x
end

local map_para = { [0] =
  function(para, value)
    return clip(value^para[1])
  end,
  function(para, value)
    if value >= -para[3]/para[2] then
      return clip((para[2]*value+para[3])^para[1])
    else
      return 0
    end
  end,
  function(para, value)
    if value >= -para[3]/para[2] then
      return clip((para[2]*value+para[3])^para[1]+para[4])
    else
      return clip(para[4])
    end
  end,
  function(para, value)
    if value >= para[5] then
      return clip((para[2]*value+para[3])^para[1])
    else
      return clip(para[4]*value)
    end
  end,
  function(para, value)
    if value >= para[5] then
      return clip((para[2]*value+para[3])^para[1]+para[6])
    else
      return clip(para[4]*value+para[7])
    end
  end,
}

local map_para_inverse = { [0] =
  function(para, value)
    return clip(value^(1/para[1]))
  end,
  function(para, value)
    if value < 0 then return clip(-para[3]/para[2]) end
    return clip((value^(1/para[1])-para[3])/para[2])
  end,
  function(para, value)
    value = value - para[4]
    if value < 0 then return clip(-para[3]/para[2]) end
    return clip((value^(1/para[1])-para[3])/para[2])
  end,
  function(para, value)
    if para[4] < 0 then return nil end
    if value >= para[4]*para[5] then
      return clip((value^(1/para[1])-para[3])/para[2])
    else
      return clip(value/para[4])
    end
  end,
  function(para, value)
    if para[4] < 0 then return nil end
    if value >= para[4]*para[5]+para[7] then
      return clip(((value-para[6])^(1/para[1])-para[3])/para[2])
    else
      return clip((value-para[7])/para[4])
    end
  end,
}

-- read_para does not take a size and guaratees to en with the reading pointer 4-byte aligned after the curve (if called on a 4-byte ligned position)
local function read_para(f)
  skipposition(f, 4)
  local kind = readu16(f)
  skipposition(f, 2)
  local num_param = kind == 0 and 1
                 or kind == 1 and 3
                 or kind == 2 and 4
                 or kind == 3 and 5
                 or kind == 4 and 7
  if not num_param then
    return nil, "Unknown function type"
  end
  local parameters = read_fixeds16table(f, num_param)
  parameters.kind = kind
  parameters.map = map_para[kind]
  parameters.inverse = map_para_inverse[kind]
  return parameters
end

local function map_curv(curv, value)
  local points = #curv
  if points == 0 then
    return value
  elseif points == 1 then
    return value^(curv[1]/256)
  else
    if value < 0 then
      value = 0
    elseif value > 1 then
      value = 1
    end
    local val_int, val_float = math.modf(value * (points-1))
    if val_float > 0.000000001 then
      return ((1-val_float)*curv[val_int+1] + val_float*curv[val_int+2] + .5)//1/((1<<(8*curv.precision))-1)
    else -- The second case is not just slightly faster in easy cases,
         --it avoids an invalid lookup if value==1
      return curv[val_int+1]/((1<<(8*curv.precision))-1)
    end
  end
end
local function map_curv_inverse(curv, value)
  local points = #curv
  if points == 0 then
    return value
  elseif points == 1 then
    return value^(256/curv[1])
  else
    if value < 0 then
      value = 0
    elseif value > 1 then
      value = 1
    end
    value = value * ((1<<(8*curv.precision))-1)
    local i, j = 1, points
    if curv[1] < curv[points] then
      while j-i > 1 do
        local k = i + (j-i)//2
        if curv[k] > value then
          j = k
        else
          i = k
        end
      end
    else
      while j-i > 1 do
        local k = i + (j-i)//2
        if curv[k] < value then
          j = k
        else
          i = k
        end
      end
    end
    if curv[i] == curv[j] then
      value = (i+j)/2
    else
      value = i + (value - curv[i])/(curv[j]-curv[i])
    end
    return (value-1) / (points-1)
  end
end
-- read_curv does not take a size and guaratees to en with the reading pointer 4-byte aligned after the curve (if called on a 4-byte ligned position)
local function read_curv(f)
  skipposition(f, 4)
  local points = readu32(f)
  local values = readcardinaltable(f, points, 2)
  values.kind = 'curv'
  values.map = map_curv
  values.inverse = map_curv_inverse
  values.precision = 2
  if points % 2 == 1 then
    skipposition(f, 2)
  end
  return values
end

local function read_curve(f)
  local tag = read_tag(f)
  if tag == 'para' then
    return read_para(f)
  elseif tag == 'curv' then
    return read_curv(f)
  else
    return nil, "Unknown curve type"
  end
end

local function map_curves(curves, values)
  -- if #curves ~= #values then
  --   show{#curves, #values, values}
  --   print(debug.traceback())
  -- end
  assert(#curves == #values)
  for i=1, #curves do
    values[i] = curves[i]:map(values[i])
  end
  return values
end

local function map_curves_inverse(curves, values)
  assert(#curves == #values)
  for i=1, #curves do
    local mapped, err = curves[i]:inverse(values[i])
    if not mapped then return nil, err or "Curve does not support reverse mapping" end
    values[i] = mapped
  end
  return values
end

-- PCSXYZ encoding for 8bit is undefined, so we can assume that it acts like s1Fixed7Number,
-- comptible with the u1Fixed15 number used for 16bit. Thi is most likely a terrible idea,
-- but 8-bit PCSXYZ shouldn't be used anyway.
local encode_xyz, decode_xyz do
  local factor = 0xFFFF/0x8000 -- This can be represented exactly in a float, in contrast to 1/factor
  encode_xyz = { map = function(_, values)
    values[1], values[2], values[3] = values[1]/factor, values[2]/factor, values[3]/factor
    return values
  end }
  decode_xyz = { map = function(_, values)
    values[1], values[2], values[3] = values[1]*factor, values[2]*factor, values[3]*factor
    return values
  end }
end

local encode_lab, decode_lab do
  local factor = 0xFFFF/0x8000 -- This can be represented exactly in a float, in contrast to 1/factor
  encode_lab = { map = function(_, values)
    values[1], values[2], values[3] = values[1]/100, (values[2]+128)/255, (values[3]+128)/255
    return values
  end }
  decode_lab = { map = function(_, values)
    values[1], values[2], values[3] = values[1]*100, values[2]*255 - 128, values[3]*255 - 128
    return values
  end }
end

local encode_lab_legacy, decode_lab_legacy do
  local factorL = (0xFFFF*100)/0xFF00 -- =100*(1+1/256), so the denominator is a power-of-2
  local factor_ab = 256-1/256
  encode_lab_legacy = { map = function(_, values)
    values[1], values[2], values[3] = values[1]/factorL, (values[2]+128)/factor_ab, (values[3]+128)/factor_ab
    return values
  end }
  decode_lab_legacy = { map = function(_, values)
    values[1], values[2], values[3] = values[1]*factorL, values[2]*factor_ab - 128, values[3]*factor_ab - 128
    return values
  end }
end

local function apply_matrix(matrix, x, y, z)
  x, y, z = x * matrix[1] + y * matrix[2] + z * matrix[3],
            x * matrix[4] + y * matrix[5] + z * matrix[6],
            x * matrix[7] + y * matrix[8] + z * matrix[9]
  if matrix[10] then
    x, y, z = x + matrix[10],  y + matrix[11], z + matrix[12]
  end
  return x, y, z
end

local function map_matrix(matrix, values)
  assert(#values == 3)
  values[1], values[2], values[3] = apply_matrix(matrix, values[1], values[2], values[3])
  values[1], values[2], values[3] = clip(values[1]), clip(values[2]), clip(values[3])
  return values
end

local function map_clut_recurse(clut, values, offset, stride, i)
  if i == 0 then return clut[offset] end
  local points = clut.dimensions[i]
  local value = values[i]
  if value < 0 then
    value = 0
  elseif value > 1 then
    value = 1
  end
  local val_int, val_float = math.modf(value * (points-1))
  local mapped = map_clut_recurse(clut, values, offset + stride * val_int, stride * points, i-1)
  if val_float > 0.000000001 then
    local other_mapped = map_clut_recurse(clut, values, offset + stride * (val_int+1), stride * points, i-1)
    mapped = ((1-val_float)*mapped + val_float*other_mapped + .5) // 1
  end
  return mapped
end
local function map_clut(clut, values)
  local result = {}
  local scaling_factor = (1<<(8*clut.precision)) - 1.
  local in_channels = #values
  local out_channels = clut.out_channels
  for i=1, out_channels do
    result[i] = map_clut_recurse(clut, values, i, out_channels, in_channels) / scaling_factor
  end
  return result
end

local function map_clut_float_recurse(clut, values, offset, stride, i)
  if i == 0 then return clut[offset] end
  local points = clut.dimensions[i]
  local value = values[i]
  if value < 0 then
    value = 0
  elseif value > 1 then
    value = 1
  end
  local val_int, val_float = math.modf(value * (points-1))
  local mapped = map_clut_float_recurse(clut, values, offset + stride * val_int, stride * points, i-1)
  if val_float > 0.000000001 then
    local other_mapped = map_clut_float_recurse(clut, values, offset + stride * (val_int+1), stride * points, i-1)
    mapped = (1-val_float)*mapped + val_float*other_mapped
  end
  return mapped
end
local function map_clut_float(clut, values)
  local result = {}
  local in_channels = #values
  if in_channels ~= clut.in_channels then
    error'Input channel mismatch'
  end
  local out_channels = clut.out_channels
  for i=1, out_channels do
    result[i] = map_clut_float_recurse(clut, values, i, out_channels, in_channels)
  end
  return result
end

local function map_pipeline(pipeline, values)
  for i=1, #pipeline do
    local err
    values, err = pipeline[i]:map(values)
    if not values then return nil, err end
  end
  return values
end

-- tag might be mAB or mBA
local function read_mABA(f, size, off0, tag, encode, decode)
  skipposition(f, 4)
  local in_channels = readu8(f)
  local out_channels = readu8(f)
  local a_channels, b_channels
  if tag == 'mAB' then
    a_channels, b_channels = in_channels, out_channels
  else
    a_channels, b_channels = out_channels, in_channels
  end
  skipposition(f, 2)
  local b_off = readu32(f)
  local matrix_off = readu32(f)
  local m_off = readu32(f)
  local clut_off = readu32(f)
  local a_off = readu32(f)

  local b, matrix, m, clut, a
  if b_off ~= 0 then
    setposition(f, off0 + b_off)
    b = {}
    for i=1, b_channels do
      local curve, err = read_curve(f)
      if err then return nil, err end
      b[i] = curve
    end
    b.map = map_curves
  end
  if m_off ~= 0 then
    setposition(f, off0 + m_off)
    m = {}
    for i=1, b_channels do
      local curve, err = read_curve(f)
      if err then return nil, err end
      m[i] = curve
    end
    m.map = map_curves
  end
  if a_off ~= 0 then
    setposition(f, off0 + a_off)
    a = {}
    for i=1, a_channels do
      local curve, err = read_curve(f)
      if err then return nil, err end
      a[i] = curve
    end
    a.map = map_curves
  end
  if matrix_off ~= 0 then
    setposition(f, off0 + matrix_off)
    matrix = read_fixeds16table(f, 12)
    matrix.map = map_matrix
  end
  if clut_off ~= 0 then
    setposition(f, off0 + clut_off)
    local grid_points = readcardinaltable(f, 16, 1)
    local precision = readu8(f)
    skipposition(f, 3)
    local total_points = out_channels
    for i = 1, in_channels do
      total_points = total_points * grid_points[i]
    end
    clut = readcardinaltable(f, total_points, precision)
    clut.dimensions = grid_points
    clut.precision = precision
    clut.out_channels = out_channels
    clut.map = map_clut
  end
  local mapping = {
    kind = tag,
    precision = precision,
    matrix = matrix,
    in_channels = in_channels,
    a = a,
    m = m,
    b = b,
    clut = clut,
    out_channels = out_channels,
    encode = encode == 'XYZ' and encode_xyz or encode == 'Lab' and encode_lab or nil,
    decode = decode == 'XYZ' and decode_xyz or decode == 'Lab' and decode_lab or nil,
    map = map_pipeline,
  }
  mapping[#mapping+1] = mapping.encode
  if tag == 'mAB' then
    mapping[#mapping+1] = a
    mapping[#mapping+1] = clut
    mapping[#mapping+1] = m
    mapping[#mapping+1] = matrix
    mapping[#mapping+1] = b
  elseif tag == 'mBA' then
    mapping[#mapping+1] = b
    mapping[#mapping+1] = matrix
    mapping[#mapping+1] = m
    mapping[#mapping+1] = clut
    mapping[#mapping+1] = a
  end
  mapping[#mapping+1] = mapping.decode
  return mapping
end

local function read_mft(f, size, precision, tag, header)
  local encode, decode
  local subtag = tag:sub(1,3)
  if subtag == 'A2B' then
    decode = header.colorspace_pcs
  elseif subtag == 'B2A' or tag == 'gamt' then
    encode = header.colorspace_pcs
  elseif subtag == 'pre' then
    encode = header.colorspace_pcs
    decode = encode
  end
  readu32(f)
  local in_channels = readu8(f)
  local out_channels = readu8(f)
  local grid_points = readu8(f)
  readu8(f)
  local matrix = read_fixeds16table(f, 9)
  matrix.map = map_matrix
  local in_entries = precision == 1 and 256 or readu16(f)
  local out_entries = precision == 1 and 256 or readu16(f)
  local in_table = {}
  for i = 1, in_channels do
    local curve = readcardinaltable(f, in_entries, precision)
    curve.map = map_curv
    curve.precision = precision
    in_table[i] = curve
  end
  in_table.map = map_curves
  local clut = readcardinaltable(f, grid_points^in_channels*out_channels, precision)
  clut.dimensions = {}
  for i=1, in_channels do clut.dimensions[i] = grid_points end
  clut.precision = precision
  clut.out_channels = out_channels
  clut.map = map_clut
  local out_table = {}
  for i = 1, out_channels do
    local curve = readcardinaltable(f, out_entries, precision)
    curve.map = map_curv
    curve.precision = precision
    out_table[i] = curve
  end
  out_table.map = map_curves

  -- 52 = 4 + 4 + 4 + 9*4 + 2 + 2
  if size ~= (precision == 1 and 48 or 52)
    + precision * (in_channels*in_entries + #clut + out_channels*out_entries) then
    return nil, "Size mismatch"
  end

  local mapping = {
    kind = "mft",
    precision = precision,
    matrix = matrix,
    in_table = in_table,
    clut = clut,
    out_table = out_table,
    map = map_pipeline,
    encode = encode == 'XYZ' and encode_xyz or encode == 'Lab' and (precision == 2 and encode_lab_legacy or encode_lab) or nil,
    decode = decode == 'XYZ' and decode_xyz or decode == 'Lab' and (precision == 2 and decode_lab_legacy or decode_lab) or nil,
  }
  mapping[#mapping+1] = mapping.encode
  if matrix[1] ~= 1 or matrix[2] ~= 0 or matrix[3] ~= 0 or
     matrix[4] ~= 0 or matrix[5] ~= 1 or matrix[6] ~= 0 or
     matrix[7] ~= 0 or matrix[8] ~= 0 or matrix[9] ~= 1 then
    mapping[#mapping+1] = matrix
  end
  mapping[#mapping+1] = in_table
  mapping[#mapping+1] = clut
  mapping[#mapping+1] = out_table
  mapping[#mapping+1] = mapping.decode
  return mapping
end

local read_mpet do
  local function readfloattable(f, count)
    local t = {string.unpack('>' .. string.rep("f", count), f:read(4*count))}
    t[#t] = nil -- Remove final offset
    assert(#t == count) -- TODO: Drop after testing
    return t
  end

  local function map_ident(_, values)
    return values
  end

  local function map_big_matrix(matrix, values)
    local val = {}
    local in_channels = matrix.in_channels
    local out_channels = matrix.out_channels
    local constants_offset = in_channels * out_channels
    for i=1, out_channels do
      local value = matrix[constants_offset+i]
      for j=1, in_channels do
        value = value + values[j]*matrix[in_channels*(i-1)+j]
      end
      val[i] = value
    end
    return val
  end

  local function map_curf(curf, value)
    local breakpoints = curf.breakpoints
    local i, j = 1, #breakpoints
    if j == 0 or value <= breakpoints[1] then
      i, j = 0, 1
    elseif value > breakpoints[j] then
      i, j = j, j+1
    else
      while j-i > 1 do
        local k = i + (j-i)//2
        if value > curv[k] then
          i = k
        else
          j = k
        end
      end
    end
    return curf[j]:map(value, breakpoints[i], breakpoints[j])
  end

  local function map_samf(samf, value, prev_break, next_break)
    local count = #samf
    value = (value-prev_break) / (next_break-prev_break)
    local val_int, val_float = math.modf(value * count)
    if val_float > 0.000000001 then
      return ((1-val_float)*samf[val_int] + val_float*samf[val_int+1])
    else -- The second case is not just slightly faster in easy cases,
         --it avoids an invalid lookup if value==1
      return samf[val_int]
    end
  end

  local log = math.log
  local map_parf = { [0] =
    function(parf, value)
      return (parf[2]*value+parf[3])^parf[1]+parf[4]
    end,
    function(parf, value)
      return parf[2]*log(parf[3]*value^parf[1]+parf[4], 10) + parf[5]
    end,
    function(parf, value)
      return parf[1]*parf[2]^(parf[3]*value+parf[4])+parf[5]
    end,
  }

  local function read_segment(f, carry, prev_break, next_break)
    local tag = read_tag(f)
    if tag == 'parf' then
      skipposition(f, 4)
      local kind = readu16(f)
      skipposition(f, 2)
      local num = kind == 0 and 4 or kind == 1 or kind == 2 and 5
      if not num then return nil, "Unknown function" end
      local parameters = readfloattable(f, num)
      parameters.map = map_parf[kind]
      return parameters, next_break and parameters:map(next_break, prev_break, next_break)
    elseif tag == 'samf' then
      skipposition(f, 4)
      local count = readu32(f)
      local entries = readfloattable(f, count)
      entries.map = map_samf
      entries[0] = carry
      return entries, entries[count]
    else
      return nil, "Unknown segment"
    end
  end

  local mpet_readers = {
    cvst = function(f, size, offset)
      skipposition(f, 4)
      local channels = readu16(f)
      if channels ~= readu16(f) then
        return nil, "Input and output channels of curve set must match"
      end
      local positions, sizes = {}, {}
      for i=1, channels do
        positions[i], sizes[i] = read_position_number(f)
      end
      local curves = {
        map = map_curves,
        in_channels = channels,
        out_channels = channels,
      }
      for i=1, channels do
        setposition(f, offset + positions[i])
        if read_tag(f) ~= 'curf' then
          return nil, "Curve expected"
        end
        skipposition(f, 4)
        local num_segments = readu16(f)
        skipposition(f, 2)
        local breakpoints = readfloattable(f, num_segments-1)
        local segments = {
          map = map_curf,
          breakpoints = breakpoints,
        }
        local carry
        for j=1, num_segments do
          segments[j], carry = read_segment(f, carry, breakpoints[j-1], breakpoints[j])
          if not segments[j] then return nil, carry end
        end
        curves[i] = segments
      end
      return curves
    end,
    matf = function(f, size)
      skipposition(f, 4)
      local in_channels = readu16(f)
      local out_channels = readu16(f)
      if size ~= 12 + (in_channels+1)*out_channels*4 then
        return nil, "Size mismatch"
      end
      local matrix = readfloattable(f, (in_channels+1)*out_channels)
      matrix.in_channels = in_channels
      matrix.out_channels = out_channels
      matrix.map = map_big_matrix
      return matrix
    end,
    clut = function(f, size)
      skipposition(f, 4)
      local in_channels = readu16(f)
      local out_channels = readu16(f)
      local grid_points = readcardinaltable(f, 16, 1)
      local total_points = out_channels
      for i = 1, in_channels do
        total_points = total_points * grid_points[i]
      end
      if size ~= 28 + 4 * total_points then
        return nil, "Size mismatch"
      end
      local clut = readfloattable(f, total_points)
      clut.dimensions = grid_points
      clut.in_channels = in_channels
      clut.out_channels = out_channels
      clut.map = map_clut_float
      return clut
    end,
    bACS = function(f, size)
      skipposition(f, 4)
      local in_channels = readu16(f)
      local out_channels = readu16(f)
      if out_channels ~= in_channels then
        return nil, "In/out channel mismatch in bACS"
      end
      return {
        in_channels = in_channels,
        out_channels = out_channels,
        map = map_ident,
      }
    end,
    eACS = function(f, size)
      skipposition(f, 4)
      local in_channels = readu16(f)
      local out_channels = readu16(f)
      if out_channels ~= in_channels then
        return nil, "In/out channel mismatch in eACS"
      end
      return {
        in_channels = in_channels,
        out_channels = out_channels,
        map = map_ident,
      }
    end,
  }

  function read_mpet(f, size, offset)
    skipposition(f, 4)
    local channels = readu16(f)
    local final_out_channels = readu16(f)
    local elements = readu32(f)
    local positions, sizes = {}, {}
    for i=1, elements do
      positions[i], sizes[i] = read_position_number(f)
    end
    local steps = {
      map = map_pipeline,
    }
    for i=1, elements do
      local off = offset + positions[i]
      setposition(f, off)
      local tag = read_tag(f)
      local reader = mpet_readers[tag]
      if not reader then
        return -- Unsupported tag ~~> fall back to A2B/B2A
      end
      local step, err = reader(f, sizes[i], off, tag)
      if not step then
        return nil, err
      elseif step.in_channels ~= channels then
        return nil, "Unexpected input channel count"
      end
      steps[i] = step
      channels = step.out_channels
    end

    if final_out_channels ~= channels then
      return nil, "Unexpected output channel count"
    end
    return steps
  end
end

-- Interface for readers: reader(f, size, offset, tag, profile)
-- f is set to a position 4 bytes after offset and size includes these 4 bytes.
-- The return value is set as value for profile[tag]
local tag_readers = {
  mpet = read_mpet,
  mluc = read_localized_unicode,
  mft1 = function(f, size, _offset, tag, profile) return read_mft(f, size, 1, tag, profile.header) end,
  mft2 = function(f, size, _offset, tag, profile) return read_mft(f, size, 2, tag, profile.header) end,
  mAB  = function(f, size, offset, _tag, profile) return read_mABA(f, size, offset, 'mAB', nil, profile.header.colorspace_pcs) end,
  mBA  = function(f, size, offset, _tag, profile) return read_mABA(f, size, offset, 'mBA', profile.header.colorspace_pcs, nil) end,
  curv = read_curv,
  para = read_para,
  sig  = function(f) skipposition(f, 4) return read_tag(f) end,
  sf32 = function(f, size) skipposition(f, 4) if size % 4 == 0 then return read_fixeds16table(f, size/4 - 2) end end,
  XYZ  = function(f, size)
    skipposition(f, 4)
    local entries = {}
    for i = 1, (size-8)/12 do
      entries[i] = {read_xyz(f)}
    end
    return entries
  end,
}

-- M={a, b, c, d, e, f, g, h, i}
-- / a b c \
-- | d e f |
-- \ g h i /
local function inv_matrix(M)
  local inv = {
    M[5]*M[9] - M[6]*M[8], M[3]*M[8] - M[2]*M[9], M[2]*M[6] - M[3]*M[5],
    M[6]*M[7] - M[4]*M[9], M[1]*M[9] - M[3]*M[7], M[3]*M[4] - M[1]*M[6],
    M[4]*M[8] - M[5]*M[7], M[2]*M[7] - M[1]*M[8], M[1]*M[5] - M[2]*M[4]}
  local det = M[1]*inv[1]+M[2]*inv[4]+M[3]*inv[7]
  if det == 0 then
    return nil, 'Not invertible'
  end
  inv[1], inv[2], inv[3] = inv[1]/det, inv[2]/det, inv[3]/det
  inv[4], inv[5], inv[6] = inv[4]/det, inv[5]/det, inv[6]/det
  inv[7], inv[8], inv[9] = inv[7]/det, inv[8]/det, inv[9]/det
  return inv
end

local function synthesize_matrix_transforms(profile)
  if profile.A2B0 or profile.B2A0 then return profile end
  if profile.rTRC or profile.gTRC or profile.bTRC then
    if profile.header.colorspace_pcs ~= 'XYZ' then
      return nil, "Matrix transforms require PCSXYZ"
    end
    if not (profile.rTRC and profile.gTRC and profile.bTRC and profile.rXYZ and profile.gXYZ and profile.bXYZ) then
      return nil, "Required tag for matrix transform missing"
    end
    local curves = {
      profile.rTRC, profile.gTRC, profile.bTRC,
      map = map_curves,
    }
    local matrix = {map = map_matrix}
    local rXYZ, gXYZ, bXYZ = profile.rXYZ[1],
        profile.gXYZ[1], profile.bXYZ[1]
    matrix[1], matrix[2], matrix[3] = rXYZ[1], gXYZ[1], bXYZ[1]
    matrix[4], matrix[5], matrix[6] = rXYZ[2], gXYZ[2], bXYZ[2]
    matrix[7], matrix[8], matrix[9] = rXYZ[3], gXYZ[3], bXYZ[3]
    profile.A2B0 = {
      curves, matrix,
      kind = "matrix/TRC",
      map = map_pipeline,
    }
    matrix = inv_matrix(matrix)
    matrix.map = map_matrix
    curves = table.move(curves, 1, 3, 1, {})
    curves.map = map_curves_inverse
    profile.B2A0 = {
      matrix, curves,
      kind = "matrix/TRC",
      map = map_pipeline,
    }
    return profile
  elseif profile.kTRC then
    error[[TODO]]
  else
    return nil, "Unsupported"
  end
end

local function read_profile(f)
  local header, err = read_header(f)
  if not header then return nil, err end
  local tags tags, err = read_tags(f, header.size)
  if not tags then return nil, err end
  local profile = {header = header}
  for tag, position in next, tags do
    local offset, length = position[1], position[2]
    setposition(f, offset)
    local data_type = read_tag(f)
    local reader = tag_readers[data_type]
    if reader then
      local tag_data tag_data, err = reader(f, length, offset, tag, profile)
      if not tag_data and err then return nil, err end -- read_mpet might return nil, nil. This is not an error but indicates that the tag should be ignored.
      profile[tag] = tag_data
    -- else
    --   print(string.format("Skipping %q since no reader for %q is available", tag, data_type))
    end
  end
  return synthesize_matrix_transforms(profile)
end

local function map_values(profile, ...)
  local mapped, msg = profile:map{...}
  if not mapped then return mapped, msg end
  return table.unpack(mapped)
end

local xyz_to_lab, lab_to_xyz do
  local delta = 6/29
  local delta_cube = delta^3
  local factor_inv = 3*delta^2
  local factor, offset = 1/factor_inv, 4/29
  function f(x)
    if x > delta_cube then
      return x^(1/3)
    else
      return factor * x + offset
    end
  end
  function f_inv(x)
    if x > delta then
      return x^3
    else
      return factor_inv * (x - offset)
    end
  end

  local x_n, y_n, z_n = 0.9642, 1, 0.8249

  function xyz_to_lab(x, y, z)
    x, y, z = f(x/x_n), f(y/y_n), f(z/z_n)
    return 116*y-16, 500*(x-y), 200*(y-z)
  end

  function lab_to_xyz(L, a, b)
    L = (L+16)/116
    return x_n*f_inv(L+a/500), y_n*f_inv(L), z_n*f_inv(L-b/200)
  end
end

local xyz_to_luv, luv_to_xyz do
  local delta = 6/29
  local delta_cube = delta^3
  local factor_inv = delta_cube/8
  local factor = 1/factor_inv

  local x_n, y_n, z_n = 0.9642, 1, 0.8249
  local u_n, v_n = 4*x_n / (x_n + 15*y_n + 3*z_n), 9*y_n / (x_n + 15*y_n + 3*z_n)

  function xyz_to_luv(x, y, z)
    local div = x + 15*y + 3*z
    local u, v = 4*x / div, 9*y / div
    
    L, u, v = y/y_n, u-u_n, v-v_n
    if L > delta_cube then
      L = 116 * L^(1/3) - 16
    else
      L = factor * L
    end
    local L13 = 13 * L
    return L, L13 * u, L13 * v
  end

  function luv_to_xyz(L, u, v)
    local L13 = 13 * L
    u, v = u/L13 + u_n, v/L13 + v_n
    if L > 8 then
      L = ((L+16)/116)^3
    else
      L = L * factor_inv
    end
    L = y_n * L
    return L * (9*u) / (4*v), L, L * (12-3*u-20*v)/(4*v)
  end
end

local xyz_to_xyY, xyY_to_xyz do
  function xyz_to_xyY(x, y, z)
    local sum = x + y + z
    return x / sum, y / sum, y
  end

  function xyY_to_xyz(x, y, Y)
    local sum = Y / y
    return x * sum, Y, (1 - x - y) * sum
  end
end

-- These functions will modify the values input table
local from_lab, from_xyz do
  local function from_pcs(profile, values, intent)
    local mapping = profile['B2D' .. (intent or 0)] or profile['B2A' .. (intent or 0)] or profile['B2A0']
    if not mapping then
      return nil, "Requested conversion not supported by profiles"
    end
    local gamut = profile.gamt
    if gamut then
      gamut = gamut:map{values[1], values[2], values[3]}
      if gamut then
        gamut = gamut[1] == 0
      end
    end
    local err values, err = mapping:map(values)
    if not values then return nil, err end
    return values, gamut
  end

  function from_lab(profile, values, intent)
    local pcs = profile.header.colorspace_pcs
    if pcs == 'XYZ' then
      values[1], values[2], values[3] = lab_to_xyz(values[1], values[2], values[3])
    elseif pcs ~= 'Lab' then
      return nil, "Unexpected PCS"
    end
    return from_pcs(profile, values, intent)
  end

  function from_xyz(profile, values, intent)
    local pcs = profile.header.colorspace_pcs
    if pcs == 'Lab' then
      values[1], values[2], values[3] = xyz_to_lab(values[1], values[2], values[3])
    elseif pcs ~= 'XYZ' then
      return nil, "Unexpected PCS"
    end
    return from_pcs(profile, values, intent)
  end
end

local function to_pcs(profile, values, intent)
  values = table.move(values, 1, #values, 1, {})
  local mapping = profile['D2B' .. (intent or 0)] or profile['A2B' .. (intent or 0)] or profile['A2B0']
  if not mapping then
    return nil, "Requested conversion not supported by profiles"
  end
  local err values, err = mapping:map(values)
  if not values then return nil, err end
  return values, profile.header.colorspace_pcs
end

local function to_lab(profile, values, intent)
  local pcs values, pcs = to_pcs(profile, values, intent)
  if not values then return nil, pcs end
  if pcs == 'XYZ' then
    values[1], values[2], values[3] = xyz_to_lab(values[1], values[2], values[3])
  elseif pcs ~= 'Lab' then
    return nil, "Unexpected PCS"
  end
  return values
end

local function to_xyz(profile, values, intent)
  local pcs values, pcs = to_pcs(profile, values, intent)
  if not values then return nil, pcs end
  if pcs == 'Lab' then
    values[1], values[2], values[3] = lab_to_xyz(values[1], values[2], values[3])
  elseif pcs ~= 'XYZ' then
    return nil, "Unexpected PCS"
  end
  return values
end

local function from_xyY(profile, values, intent)
  values[1], values[2], values[3] = xyY_to_xyz(values[1], values[2], values[3])
  return from_xyz(profile, values, intent)
end

local function to_xyY(profile, values, intent)
  values = to_xyz(profile, values, intent)
  values[1], values[2], values[3] = xyz_to_xyY(values[1], values[2], values[3])
  return values
end

local function from_luv(profile, values, intent)
  values[1], values[2], values[3] = luv_to_xyz(values[1], values[2], values[3])
  return from_xyz(profile, values, intent)
end

local function to_luv(profile, values, intent)
  values = to_xyz(profile, values, intent)
  values[1], values[2], values[3] = xyz_to_luv(values[1], values[2], values[3])
  return values
end

-- Convert can go through XYZ directly and therefore is faster when no Lab is involved.
local function convert(target, intent, source, values)
  local pcs values, pcs = to_pcs(source, values, intent)
  if not values then return nil, pcs end
  local converter = pcs == 'Lab' and from_lab or pcs == 'XYZ' and from_xyz
  if not converter then
    return nil, "Unexpected input PCS"
  end
  return converter(target, values, intent)
end

local function interpolate(from_space, to_space)
  local function interp(target, intent, t, acc, profile, values, factor, ...)
    if not factor and select('#', ...) == 0 then
      factor = 1-t
    end
    local err
    values, err = to_space(profile, values, intent)
    if not values then return nil, err end
    acc[1], acc[2], acc[3] =
        acc[1] + factor*values[1], acc[2] + factor*values[2], acc[3] + factor*values[3]
    t = t + factor
    if select('#', ...) == 0 then
      if t > 1.0001 or t < 0.999 then
        return nil, "Factors do not add to unity"
      end
      return from_space(target, acc, intent)
    end
    return interp(target, intent, t, acc, ...)
  end
  return function(target, intent, ...)
    local values = {0, 0, 0}
    return interp(target, intent, 0, values, ...)
  end
end

local interpolate_polar do
  local function to_polar(abc)
    abc[2], abc[3] = math.sqrt(abc[2]^2 + abc[3]^2), math.atan(abc[3], abc[2])
    return abc
  end
  local function from_polar(abc)
    abc[2], abc[3] = abc[2] * math.cos(abc[3]), abc[2] * math.sin(abc[3])
    return abc
  end
  function interpolate_polar(from_space, to_space)
    return function(target, intent, profile1, values1, factor, profile2, values2, inverse)
      values1 = to_polar(to_space(profile1, values1, intent))
      values2 = to_polar(to_space(profile2, values2, intent))
      if values1[3] < values2[3] then
        if (values2[3] - values1[3] > math.pi) == not inverse then
          values1[3] = values1[3] + 2*math.pi
        end
      else
        if (values1[3] - values2[3] > math.pi) == not inverse then
          values2[3] = values2[3] + 2*math.pi
        end
      end
      local values = from_polar{
        values1[1] * factor + values2[1] * (1-factor),
        values1[2] * factor + values2[2] * (1-factor),
        values1[3] * factor + values2[3] * (1-factor),
      }
      return from_space(target, values, intent)
    end
  end
end

local function load_profile(filename)
  local f, msg = io.open(filename, 'rb')
  if not f then return f, msg end
  local profile, err = read_profile(f)
  f:close()
  return profile, err
end

local input_components_lookup = {
  XYZ = 3,
  Lab = 3,
  Luv = 3,
  YCbr = 3,
  Yxy = 3,
  RGB = 3,
  GRAY = 1,
  HSV = 3,
  HLS = 3,
  CMYK = 4,
  CMY = 3,
  ['2CLR'] = 2,
  ['3CLR'] = 3,
  ['4CLR'] = 4,
  ['5CLR'] = 5,
  ['6CLR'] = 6,
  ['7CLR'] = 7,
  ['8CLR'] = 8,
  ['9CLR'] = 9,
  ['ACLR'] = 10,
  ['BCLR'] = 11,
  ['CCLR'] = 12,
  ['DCLR'] = 13,
  ['ECLR'] = 14,
  ['FCLR'] = 15,
}
local function input_components(profile)
  return input_components_lookup[profile.header.colorspace_a]
end

return {
  load = load_profile,
  map = map_values,
  convert = convert,
  interpolate_lab = interpolate(from_lab, to_lab),
  interpolate_xyz = interpolate(from_xyz, to_xyz),
  interpolate_xyY = interpolate(from_xyY, to_xyY),
  interpolate_luv = interpolate(from_luv, to_luv),
  interpolate_lch = interpolate_polar(from_lab, to_lab),
  interpolate_lchuv = interpolate_polar(from_luv, to_luv),
  input_components = input_components,
}
