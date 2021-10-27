local parse_icc = require'parse_icc'

local loaded_profiles = {}
local functions = lua.get_functions_table()

local luacall = token.command_id'lua_call'
local relax = token.command_id'relax'
local spacer = token.command_id'spacer'

local handler

local function scan_profile()
  local t
  repeat
    t = token.scan_token()
  until t.command ~= relax and t.command ~= spacer
  local index = t.index
  if t.command ~= luacall or functions[index] ~= handler then
    error[[Profile expected]]
  end
  return loaded_profiles[index]
end


local funcid = luatexbase.new_luafunction'LoadProfile'
token.set_lua('LoadProfile', funcid, 'global', 'protected')
functions[funcid] = function()
  token.put_next(token.create'noexpand')
  local csname = token.scan_token().csname
  local filename = token.scan_argument()
  local profile = assert(parse_icc.load(kpse.find_file(filename)))
  local profile_id = luatexbase.new_luafunction('Lua loaded colorprofile ' .. filename)
  token.set_lua(csname, profile_id, 'protected')
  functions[profile_id] = handler
  loaded_profiles[profile_id] = profile
end

local funcid = luatexbase.new_luafunction'UnloadProfile'
token.set_lua('UnloadProfile', funcid, 'global', 'protected')
functions[funcid] = function()
  local t
  repeat
    t = token.scan_token()
  until t.command ~= relax and t.command ~= spacer
  local index = t.index
  if t.command ~= luacall or functions[index] ~= handler then
    error[[Profile expected]]
  end
  loaded_profiles[index] = nil
  functions[index] = nil
  if false then
    luatexbase.free_luafunction(index)
  end
end

funcid = luatexbase.new_luafunction'ApplyProfile'
token.set_lua('ApplyProfile', funcid, 'global')
functions[funcid] = function()
  local delim = token.scan_keyword'delim' and token.get_next() or ' '
  local out_of_gamut_tag = token.scan_keyword'gamut' and token.get_next()
  local intent = token.scan_keyword'perceptual' and 0 or token.scan_keyword'colorimetric' and 1 or token.scan_keyword'saturation' and 2 or token.scan_keyword'absolute' and 3 or 1
  local polar = token.scan_keyword'lchuv' and parse_icc.interpolate_lchuv or token.scan_keyword'lch' and parse_icc.interpolate_lch
  local inverse = polar and token.scan_keyword'inverse'
  local interpolate = polar or token.scan_keyword'lab' and parse_icc.interpolate_lab or token.scan_keyword'luv' and parse_icc.interpolate_luv or token.scan_keyword'xyz' and parse_icc.interpolate_xyz or token.scan_keyword'xyy' and parse_icc.interpolate_xyY or parse_icc.interpolate_lab
  local profile = scan_profile()
  local is_device_link = parse_icc.profile_class(profile) == 'link'
  local num = is_device_link and 1 or token.scan_int()
  if num < 1 then
    tex.error'Invalid number of color components'
    return
  elseif num == 1 then
    interpolate, polar = parse_icc.convert, nil
  elseif polar and num > 2 then
    tex.error'Unable to interpolate more than two colors in polar space'
    return
  end
  local args = {profile, intent}
  for i = 1, num do
    local prof = is_device_link and profile or scan_profile()
    local class = parse_icc.profile_class(prof)
    local components
    if class == 'nmcl' then
      components = token.scan_argument()
    else
      components = {}
      local num_components = assert(parse_icc.input_components(prof))
      for j = 1, num_components do
        components[j] = token.scan_real()
      end
    end
    args[3*i] = prof
    args[3*i+1] = components
    if i ~= num then
      args[3*i+2] = token.scan_int()/1000
    end
  end
  local result, in_gamut
  if polar then
    args[#args+1] = inverse
  end
  if is_device_link then
    result, in_gamut = assert(parse_icc.apply_device_link(args[1], args[4]))
  else
    result, in_gamut = assert(interpolate(table.unpack(args)))
  end
  -- in_gamut has three possible values:
  --  - nil: Indeterminate (No gamt tag or check failed)
  --  - true: In gamut
  --  - false: out of gamut
  if out_of_gamut_tag and in_gamut == false then
    tex.sprint(out_of_gamut_tag)
  end
  tex.sprint(-2, string.format("%.6f", result[1]))
  for i=2, #result do
    tex.sprint(-2, delim)
    tex.sprint(-2, string.format("%.6f", result[i]))
  end
end

funcid = luatexbase.new_luafunction'ProfileInfo'
token.set_lua('ProfileInfo', funcid, 'global')
functions[funcid] = function()
  if token.scan_keyword'components' then
    local profile = scan_profile()
    return tex.write(parse_icc.input_components(profile))
  elseif token.scan_keyword'class' then
    local profile = scan_profile()
    return tex.sprint(-2, parse_icc.profile_class(profile))
  end
  tex.error('Unsupported argument ' .. token.scan_word() .. ' supplied to \\ProfileInfo')
end

function handler()
  tex.error('Color profile identifier misused', {
      'Color profile commands initialized with \\LoadProfile can not be \z
      used on their own but can only be used as arguments \z
      for \\ApplyProfile or \\ProfileInfo.'
  })
end
