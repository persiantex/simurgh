if not modules then modules = { } end modules ['simurgh-unibidi-ini'] = {
    version   = 0.01,
    comment   = "unicode bidi",
    author    = "Vafa Khalighi",
    copyright = "Vafa Khalighi",
    license   = "see simurgh package documentation",
    derived   = "derived from typo-dir by Hans Hagen"
}

-- When we started with this, there were some issues in luatex so we needed to take care of
-- intereferences. Some has been improved but we stil might end up with each node having a
-- dir property. Now, the biggest problem is that there is an official bidi algorithm but
-- some searching on the web shows that there are many confusing aspects and therefore
-- proposals circulate about (sometimes imcompatible ?) improvements. In the end it all boils
-- down to the lack of willingness to tag an input source. Of course tagging of each number
-- and fenced strip is somewhat over the top, but now it has to be captured in logic. Texies
-- normally have no problem with tagging but we need to handle any input. So, what we have
-- done here (over the years) is starting from what we expect to see happen, especially with
-- respect to punctation, numbers and fences. Eventually alternative algorithms will be provides
-- so that users can choose (the reason why suggestion sfor improvements circulate on the web
-- is that it is non trivial to predict the expected behaviour so one hopes that the ditor
-- and the rest of the machinery match somehow. Anyway, the fun of tex is that it has no hard
-- coded behavior. And ... we also want to have more debugging and extras and ... so we want
-- a flexible approach. 

local next, type = next, type
local format, insert, sub, find, match = string.format, table.insert, string.sub, string.find, string.match
local utfchar = utf.char
local formatters = string.formatters

local nodes, node = nodes, node

local trace_textdirections = false  trackers.register("typesetters.directions.text", function(v) trace_textdirections = v end)
local trace_mathdirections = false  trackers.register("typesetters.directions.math", function(v) trace_mathdirections = v end)
local trace_directions     = false  trackers.register("typesetters.directions",      function(v) trace_textdirections = v trace_mathdirections = v end)

local report_textdirections = logs.reporter("typesetting","text directions")
local report_mathdirections = logs.reporter("typesetting","math directions")




local traverse_id        = node.traverse_id
local insert_node_before = node.insert_before
local insert_node_after  = node.insert_after
local remove_node        = nodes.remove
local end_of_math        = nodes.end_of_math

local texsetattribute    = tex.setattribute
local texsetcount        = tex.setcount
local unsetvalue         = attributes.unsetvalue

local hasbit             = number.hasbit

local nodecodes          = nodes.nodecodes
local whatcodes          = nodes.whatcodes
local mathcodes          = nodes.mathcodes

local tasks              = nodes.tasks

local glyph_code         = nodecodes.glyph
local whatsit_code       = nodecodes.whatsit
local math_code          = nodecodes.math
local penalty_code       = nodecodes.penalty
local kern_code          = nodecodes.kern
local glue_code          = nodecodes.glue
local hlist_code         = nodecodes.hlist
local vlist_code         = nodecodes.vlist

local localpar_code      = whatcodes.localpar
local dir_code           = whatcodes.dir

local nodepool           = nodes.pool

local new_textdir        = nodepool.textdir

local fonthashes         = fonts.hashes
local fontdata           = fonthashes.identifiers
local fontchar           = fonthashes.characters

local chardirections     = characters.directions
local charmirrors        = characters.mirrors
local charclasses        = characters.textclasses

local directions         = typesetters.directions or { }
typesetters.directions   = directions

local a_state            = attributes.private('state')
local a_directions       = attributes.private('directions')
local a_mathbidi         = attributes.private('mathbidi')

local strip              = false

local s_isol             = fonts.analyzers.states.isol

local variables          = interfaces.variables
local v_global           = variables["global"]
local v_local            = variables["local"]
local v_on               = variables.on
local v_yes              = variables.yes

local m_enabled          = 2^6 -- 64
local m_global           = 2^7
local m_fences           = 2^8

local handlers           = { }
local methods            = { }
local lastmethod         = 0

local function installhandler(name,handler)
    local method = methods[name]
    if not method then
        lastmethod    = lastmethod + 1
        method        = lastmethod
        methods[name] = method
    end
    handlers[method] = handler
    return method
end

directions.handlers       = handlers
directions.installhandler = installhandler

local function tomode(specification)
    local scope = specification.scope
    local mode
    if scope == v_global or scope == v_on then
        mode = m_enabled + m_global
    elseif scope == v_local then
        mode = m_enabled
    else
        return 0
    end
    local method = methods[specification.method]
    if method then
        mode = mode + method
    else
        return 0
    end
    if specification.fences == v_yes then
        mode = mode + m_fences
    end
    return mode
end

local function getglobal(a)
    return a and a > 0 and hasbit(a,m_global)
end

local function getfences(a)
    return a and a > 0 and hasbit(a,m_fences)
end

local function getmethod(a)
    return a and a > 0 and a % m_enabled or 0
end

directions.tomode         = tomode
directions.getglobal      = getglobal
directions.getfences      = getfences
directions.getmethod      = getmethod
directions.installhandler = installhandler


function commands.getbidimode(specification)
    context(tomode(specification)) -- hash at tex end
end

local enabled = false

local starttiming = statistics.starttiming
local stoptiming  = statistics.stoptiming

function directions.process(namespace,attribute,head) -- for the moment nodes and not nuts
    if not head.next then
        return head, false
    end
    local attr = head[a_directions]
    if not attr or attr == 0 then
        return head, false
    end
    local method  = getmethod(attr)
    local handler = handlers[method]
    if not handler then
        return head, false
    end
    starttiming(directions)
    local head, done = handler(namespace,attribute,head)
    stoptiming(directions)
    return head, done
end


-- function directions.enable()
--     tasks.enableaction("processors","directions.handler")
-- end

function directions.set(n) -- todo: names and numbers
    if not enabled then
        if trace_textdirections then
            report_textdirections("enabling directions handler")
        end
        tasks.enableaction("processors","typesetters.directions.handler")
        enabled = true
    end
    if not n or n == 0 then
        n = unsetvalue
        -- maybe tracing
    end
    texsetattribute(a_directions,n)
end

commands.setdirection = directions.set

directions.handler = nodes.installattributehandler {
    name      = "directions",
    namespace = directions,
    processor = directions.process,
}
