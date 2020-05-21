local err, warn, info = luatexbase.provides_module {
  name        = "simurgh-unibidi",
    version   = 0.01,
    comment   = "unicode bidi",
    author    = "Vafa Khalighi",
    copyright = "Vafa Khalighi",
    license   = "see simurgh package documentation",
    derived   = "derived from import-typo-dir by Philipp Gesang"
}
 
info "unicode bidi"






require "lualibs"  -- we need the extended set

local texsprint     = tex.sprint
local stringformat  = string.format
local tableconcat   = table.concat
local copynode      = node.copy

-----------------------------------------------------------------------
---                             prepare
-----------------------------------------------------------------------

--- 1) wrap Luatexbase attribute handler in a Context style interface

attributes            = attributes or { }
local texsetattribute = tex.setattribute


local hidden = {
  state       = luatexbase.new_attribute ("typo-dir:state",      true),
  directions  = luatexbase.new_attribute ("typo-dir:directions", true),
  mathbidi    = luatexbase.new_attribute ("typo-dir:mathbidi",   true),
}

local a_directions = hidden.directions

attributes.private = attributes.private or function (attr_name)
  local res = hidden [attr_name]
  if not res then
    res = luatexbase.new_attribute (attr_name)
  end
  return res
end

local unsetvalue      = luatexbase.get_unset_value ()
attributes.unsetvalue = unsetvalue

--- 2)  simulate the multilingual interface; safe to purge afterwards
---     since Context uses local copies

interfaces                      = interfaces or { }
interfaces.variables            = interfaces.variables or { }
interfaces.variables.global     = "global"
interfaces.variables["local"]   = "local"
interfaces.variables.default    = "default"
interfaces.variables.on         = "on"
interfaces.variables.yes        = "yes"

--- 3) node tasks; we don’t have real node processors so we will need
---    to set up a makeshift interface

nodes.tasks               = nodes.tasks or { }
nodes.tasks.enableaction  = function () end

--- 4) commands namespace

commands = commands or { } -- already present due to luaotfload-extrablibs

--- 5) typesetters namespace
---

-- require "typo-krn"

typesetters = typesetters or { }

--- 6)  the Context namespace; cannot be cleaned up without breaking
---     things; srsly there needs to be a non-Context equivalent for
---     this

context = context or { }

setmetatable (context, {

  --- this is quite primitive and considerably less functional than
  --- the real deal as defined in cldf-ini.lua, but we already get
  --- quite far with this reduced version

  __index = function (t, k)

    if k == "sprint" then
      return function (...) texsprint (...) end

    elseif type (k) == "string" then
      local command = [[\]] .. k
      return function (...)
        local res = { command }
        for i = 1, select ("#", ...) do
          --- just simple grouped arguments
          res [#res + 1] = "{"
          res [#res + 1] = select (i, ...)
          res [#res + 1] = "}"
        end
        texsprint (tableconcat (res))
      end

    else
      return context
    end
  end,

  __call = function (t, fmt, first, ...)

    if t == nil or fmt == nil then
      return
    end

    local tf = type (fmt)

    if tf == "string" then
      if first then
        texsprint (-1, stringformat (fmt, first, ...))
      else
        texsprint (-1, fmt)
      end
    elseif tf == "function" then
      texsprint (-1, fmt (first, ...))
    elseif first then --- and so on,
      texsprint (-1, tostring (fmt), first, ...)
    else
      texsprint (-1, tostring (fmt))
    end
  end,
})

--- 7)  catcodes namespace

catcodes                      = catcodes or { }
catcodes.numbers              = catcodes.numbers or { }
catcodes.numbers.ctxcatcodes  = -1

--- 8) whatsit prototype; expected to be present in the nodepool

local n_textdir     = node.new (node.id "whatsit", nodes.whatsitcodes.dir)
nodes.pool          = nodes.pool or { }

nodes.pool.textdir  = function (dir)
  local n = copynode (n_textdir)
  n.dir = dir
  return n
end

--- 9) node identifiers

nodes.nodecodes = table.mirrored (nodes.nodecodes) --> convenience

----------------------------------------------------------------------
---                             import
-----------------------------------------------------------------------

if not characters.directions then
  require "simurgh-char-def" --> characters.data (unicode)
  require "simurgh-char-ini" --> characters.*
end
--- the next three used to share “typo-dir.lua” before it was split
require "simurgh-unibidi-ini" --> typesetters.directions
require "simurgh-unibidi-core" --> typesetters.directions
require "simurgh-unibidi-math" --> typesetters.directions

-----------------------------------------------------------------------
---                            wrappers
-----------------------------------------------------------------------

--- we use the *packagedata* namespace which should become canonical
--- anyways; also we keep a copy of typesetters.directions in a
--- subtable

packagedata                     = packagedata or { }
local directions                = typesetters.directions
local typo_dir                  = { directions = directions }
packagedata.typo_dir            = typo_dir
local directionprocessor        = directions.process
local mathdirectionprocessor    = directions.processmath
local processorid               = "typesetters.directions"

--- emulate node tasks capability; we override the original definitions
--- of set() and setmath() with surrogates that work with Luatexbase
--- callback handlers

directions.set                  = nil
directions.setmath              = nil


--- we need to track which callbacks the node processor is hooked
--- into since we lack the combined version Context has

local registered_as = { } --- procname -> callbacks


--- (node_t -> node_t) -> string -> string list -> bool

local add_processor = function (processor, name, ...)

  for i=1, select ("#", ...) do
    local callback = select (i, ...)
    --- *IMPORTANT* the processor must be inserted at the top,
    --- i.e. with a priority higher than any other callback!
    luatexbase.add_to_callback (callback, processor, name, 1)
  end

  registered_as [name] = { ... }
  return true

end


--- string -> bool

local remove_processor = function (name)

  local callbacks = registered_as [name]

  if callbacks then
    for i=1, #callbacks do
      luatexbase.remove_from_callback (callbacks [i], name)
    end
    return true
  end

  return false
end

--- we use the same callbacks as a node processor in Context
--- unit -> bool

local enabledirectionprocessor = function (math)

  local processor

  if math == true then
    processor = function (hd)
      --- different signature from the normal one
      return mathdirectionprocessor (hd)
    end
  else
    processor = function (hd)
      return directionprocessor ("directions", a_directions, hd)
    end
  end

  return add_processor (processor,
                        processorid,
                        "pre_linebreak_filter",
                        "hpack_filter")
end

typo_dir.enable = enabledirectionprocessor


--- unit -> bool
local disabledirectionprocessor = function ( )
  return remove_processor (processorid)
end

typo_dir.disable = disabledirectionprocessor


local active = false --- activation state of direction processor

typo_dir.set = function (n)

  if not n or n == 0 then
    n = unsetvalue
  end

  if not active then
    info ("Installing simurgh direction handler (%d).", n)
    enabledirectionprocessor ()
    active = true
  end

  texsetattribute (a_directions, n)
end


local active = false --- activation state of math direction processor

typo_dir.setmath = function (n)

  if not active and n and n > 0 then
    info ("Installing simurgh math direction handler (%d).", n)
    enabledirectionprocessor (true)
    active = true
  end
end

typo_dir.getbidimode = directions.getbidimode

typo_dir.getbidimode = function (specification)
  context (directions.tomode (specification))
end

-----------------------------------------------------------------------
---                              clean
-----------------------------------------------------------------------

attributes.private    = nil
attributes            = nil
interfaces.variables  = nil
interfaces            = nil
nodes.tasks           = nil

collectgarbage "collect"

