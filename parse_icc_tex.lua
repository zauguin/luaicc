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
  local profile = parse_icc.load(kpse.find_file(filename))
  local profile_id = luatexbase.new_luafunction('Lua loaded colorprofile ' .. filename)
  token.set_lua(csname, profile_id)
  functions[profile_id] = handler
  loaded_profiles[profile_id] = profile
end

local luacall = token.command_id'lua_expandable_call'
local relax = token.command_id'relax'
local spacer = token.command_id'spacer'
function handler(id)
  local profile = assert(loaded_profiles[id])
  local delim = token.scan_keyword'delim' and token.get_next() or ' '
  local num = token.scan_int()
  local args = {profile, 0}
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
    local num_components = parse_icc.input_components(prof)
    for j = 1, num_components do
      components[j] = token.scan_real()
    end
    local factor
    args[3*i] = prof
    args[3*i+1] = components
    if i ~= num then
      factor = token.scan_int()/1000
      args[3*i+2] = factor
    end
  end
  local result = parse_icc.interpolate(table.unpack(args))
  tex.sprint(-2, string.format("%.4f", result[1]))
  for i=2, #result do
    tex.sprint(-2, delim)
    tex.sprint(-2, string.format("%.4f", result[i]))
  end
end
