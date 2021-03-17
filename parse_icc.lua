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
    if x ~= pcs_illuminant_x or y ~= pcs_illuminant_y or z ~= pcs_illuminant_z then
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
    local t = {string.unpack(f:read(4*count), string.rep("f", count))}
    t[#t] = nil -- Remove final offset
    assert(#t == count) -- TODO: Drop after testing
    return t
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
      return parameters, parameters:map(next_break, prev_break, next_break)
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
        skipposition(4)
        local num_segments = readu16(f)
        skipposition(2)
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
    clut = nil, -- TODO
    bACS = nil, -- TODO
    eACS = nil, -- TODO
  }

  function read_mpet(f, size, offset)
    skipposition(f, 4)
    local channels = readu16(f)
    local final_out_channels = readu16(f)
    local elements = readu16(f)
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
    return mapping
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
    if not (profile.rTRC and profile.gTRC and profile.bTRC and profile.rXYZ and profile.gTRC and profile.bTRC) then
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
      if not tag_data then return nil, err end
      profile[tag] = tag_data
    -- else
    --   print(string.format("Skipping %q since no reader for %q is available", tag, data_type))
    end
  end
  return synthesize_matrix_transforms(profile)
end

local function map_values(profile, ...)
  return table.unpack(assert(profile:map{...}))
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

-- Convert can go through XYZ directly and therefore is faster when no Lab is involved.
local function convert(profile1, profile2, values, intent)
  values = table.move(values, 1, #values, 1, {})
  local mapping1 = profile1['A2B' .. (intent or 0)] or profile1['A2B0']
  local mapping2 = profile1['B2A' .. (intent or 0)] or profile1['B2A0']
  if (not mapping1) or (not mapping2) then
    return nil, "Requested conversion not supported by profiles"
  end
  local err
  values, err = mapping1:map(values)
  if not values then return nil, err end
  if profile1.header.colorspace_pcs ~= profile2.header.colorspace_pcs then
    if profile1.header.colorspace_pcs == 'Lab' and profile2.header.colorspace_pcs == 'XYZ' then
      values[1], values[2], values[3] = lab_to_xyz(values[1], values[2], values[3])
    elseif profile1.header.colorspace_pcs == 'XYZ' and profile2.header.colorspace_pcs == 'Lab' then
      values[1], values[2], values[3] = xyz_to_lab(values[1], values[2], values[3])
    else
      return nil, "Unsupported"
    end
  end
end
local interpolate, interpolate_polar do
  local function to_lab(profile, values, intent)
    values = table.move(values, 1, #values, 1, {})
    local mapping = profile['A2B' .. (intent or 0)] or profile['A2B0']
    if not mapping then return nil, "Unsupported" end
    local err values, err = mapping:map(values)
    if not values then return nil, err end
    if profile.header.colorspace_pcs == 'XYZ' then
      values[1], values[2], values[3] = xyz_to_lab(values[1], values[2], values[3])
    elseif profile.header.colorspace_pcs ~= 'Lab' then
      return nil, 'Unsupported'
    end
    return values
  end
  local function interp(target, intent, t, acc, profile, values, factor, ...)
    if not factor and select('#', ...) == 0 then
      factor = 1-t
    end
    local err
    values, err = to_lab(profile, values, intent)
    if not values then return nil, err end
    acc[1], acc[2], acc[3] =
        acc[1] + factor*values[1], acc[2] + factor*values[2], acc[3] + factor*values[3]
    t = t + factor
    if select('#', ...) == 0 then
      if t > 1.0001 or t < 0.999 then
        return nil, "Factors do not add to unity"
      end
      if target.header.colorspace_pcs == 'XYZ' then
        acc[1], acc[2], acc[3] = lab_to_xyz(acc[1], acc[2], acc[3])
      elseif target.header.colorspace_pcs ~= 'Lab' then
        return nil, 'Unsupported'
      end
      local mapping = target['B2A' .. (intent or 0)] or target['B2A0']
      if not mapping then return nil, "Unsupported" end
      return mapping:map(acc)
    end
    return interp(target, intent, t, acc, ...)
  end
  function interpolate(target, intent, ...)
    local values = {0, 0, 0}
    return interp(target, intent, 0, values, ...)
  end
  local function Lab_to_HLC(Lab)
    Lab[1], Lab[2], Lab[3] = math.atan(Lab[3], Lab[2]), Lab[1], math.sqrt(Lab[2]^2 + Lab[3]^2)
    return Lab
  end
  local function HLC_to_Lab(HLC)
    HLC[1], HLC[2], HLC[3] = HLC[2], HLC[3] * math.cos(HLC[1]), HLC[3] * math.sin(HLC[1])
    return HLC
  end
  function interpolate_polar(target, intent, profile1, values1, factor, profile2, values2, inverse)
    values1 = Lab_to_HLC(to_lab(profile1, values1, intent))
    values2 = Lab_to_HLC(to_lab(profile2, values2, intent))
    if values1[1] < values2[1] then
      if (values2[1] - values1[1] > math.pi) == not inverse then
        values1[1] = values1[1] + 2*math.pi
      end
    else
      if (values1[1] - values2[1] > math.pi) == not inverse then
        values2[1] = values2[1] + 2*math.pi
      end
    end
    local values = HLC_to_Lab{
      values1[1] * factor + values2[1] * (1-factor),
      values1[2] * factor + values2[2] * (1-factor),
      values1[3] * factor + values2[3] * (1-factor),
    }
    if target.header.colorspace_pcs == 'XYZ' then
      values[1], values[2], values[3] = lab_to_xyz(values[1], values[2], values[3])
    elseif target.header.colorspace_pcs ~= 'Lab' then
      return nil, 'Unsupported'
    end
    local mapping = target['B2A' .. (intent or 0)] or target['B2A0']
    if not mapping then return nil, "Unsupported" end
    return mapping:map(values)
  end
end

local function load_profile(filename)
  local f = io.open(filename, 'rb')
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
  interpolate = interpolate,
  interpolate_polar = interpolate_polar,
  input_components = input_components,
}
