local parse_icc = require'parse_icc'

local loaded_profiles = {}
local functions = lua.get_functions_table()

local funcid = luatexbase.new_luafunction'LoadProfile'
token.set_lua('LoadProfile', funcid, 'global', 'protected')
local handler
functions[funcid] = function()
  token.put_next(token.create'noexpand')
  local csname = token.scan_token().csname
  local filename = token.scan_argument()
  local profile = assert(parse_icc.load(kpse.find_file(filename)))
  local profile_id = luatexbase.new_luafunction('Lua loaded colorprofile ' .. filename)
  token.set_lua(csname, profile_id)
  functions[profile_id] = handler
  loaded_profiles[profile_id] = profile
  if profile.A2D0 then
    print(require'inspect'(profile.A2D0))
  end
end

local luacall = token.command_id'lua_expandable_call'
local relax = token.command_id'relax'
local spacer = token.command_id'spacer'
function handler(id)
  local profile = assert(loaded_profiles[id])
  if token.scan_keyword'components' then return tex.write(parse_icc.input_components(profile)) end
  local delim = token.scan_keyword'delim' and token.get_next() or ' '
  local out_of_gamut_tag = token.scan_keyword'gamut' and token.get_next()
  local intent = token.scan_keyword'perceptual' and 0 or token.scan_keyword'colorimetric' and 1 or token.scan_keyword'saturation' and 2 or 0
  local inverse = token.scan_keyword'inv_polar'
  local polar = inverse or token.scan_keyword'polar'
  local num = polar and 2 or token.scan_int()
  local args = {profile, intent}
  for i = 1, num do
    local t repeat
      t = token.get_next()
    until t.command ~= relax and t.command ~= spacer
    local index = t.index
    if t.command ~= luacall or functions[index] ~= handler then
      error[[Profile expected]]
    end
    local prof = loaded_profiles[index]
    local components = {}
    local num_components = assert(parse_icc.input_components(prof))
    for j = 1, num_components do
      components[j] = token.scan_real()
      print(components[j])
    end
    local factor
    args[3*i] = prof
    args[3*i+1] = components
    if i ~= num then
      factor = token.scan_int()/1000
      args[3*i+2] = factor
    end
  end
  local result, in_gamut
  if polar then
    args[#args+1] = inverse
    result, in_gamut = assert(parse_icc.interpolate_polar(table.unpack(args)))
  else
    result, in_gamut = assert(parse_icc.interpolate(table.unpack(args)))
  end
  -- in_gamut has three possible values:
  --  - nil: Indeterminate (No gamt tag or check failed)
  --  - true: In gamut
  --  - false: out of gamut
  if in_gamut ~= nil then
    print(out_of_gamut_tag, in_gamut)
  end
  if out_of_gamut_tag and in_gamut == false then
    tex.sprint(out_of_gamut_tag)
  end
  tex.sprint(-2, string.format("%.6f", result[1]))
  for i=2, #result do
    tex.sprint(-2, delim)
    tex.sprint(-2, string.format("%.6f", result[i]))
  end
end
