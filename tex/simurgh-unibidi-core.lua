if not modules then modules = { } end modules ['simurgh-unibidi-core'] = {
    version   = 0.01,
    comment   = "unicode bidi",
    author    = "Vafa Khalighi",
    copyright = "Vafa Khalighi",
    license   = "see simurgh package documentation",
    derived   = "derived from typo-dha by Hans Hagen"
}

-- Some analysis:
--
-- 1. Assuming the reading- vs word-order distinction (bidi-char types) is governing;
-- 2. Assuming that 'PARSI' represents an actual parsi string in raw input order, not word-order;
-- 3. Assuming that 'ISRAP' represent the correct RL word order;
--
-- Then we have, with input: LATIN PARSI
--
-- \textdir TLT LATIN PARSI => LATIN ISRAP
-- \textdir TRT LATIN PARSI => LATIN ISRAP
-- \textdir TRT LRO LATIN PARSI => LATIN PARSI
-- \textdir TLT LRO LATIN PARSI => LATIN PARSI
-- \textdir TLT RLO LATIN PARSI => NITAL PARSI
-- \textdir TRT RLO LATIN PARSI => NITAL PARSI

-- elseif d == "es"  then -- European Number Separator
-- elseif d == "et"  then -- European Number Terminator
-- elseif d == "cs"  then -- Common Number Separator
-- elseif d == "nsm" then -- Non-Spacing Mark
-- elseif d == "bn"  then -- Boundary Neutral
-- elseif d == "b"   then -- Paragraph Separator
-- elseif d == "s"   then -- Segment Separator
-- elseif d == "ws"  then -- Whitespace
-- elseif d == "on"  then -- Other Neutrals

-- todo  : delayed inserts here
-- todo  : get rid of local functions here
-- beware: math adds whatsits afterwards so that will mess things up
-- todo  : use new dir functions
-- todo  : make faster
-- todo  : move dir info into nodes
-- todo  : swappable tables and floats i.e. start-end overloads (probably loop in builders)
-- todo  : check if we still have crashes in luatex when non-matched (used to be the case)

local nodes, node = nodes, node

local trace_directions   = false  trackers.register("typesetters.directions.default", function(v) trace_directions = v end)

local report_directions  = logs.reporter("typesetting","text directions")


local insert_node_before = nodes.insert_before
local insert_node_after  = nodes.insert_after
local remove_node        = nodes.remove
local end_of_math        = nodes.end_of_math

local nodepool           = nodes.pool

local nodecodes          = nodes.nodecodes
local whatcodes          = nodes.whatcodes
local mathcodes          = nodes.mathcodes

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

local new_textdir        = nodepool.textdir

local hasbit             = number.hasbit
local formatters         = string.formatters
local insert             = table.insert

local fonthashes         = fonts.hashes
local fontdata           = fonthashes.identifiers
local fontchar           = fonthashes.characters

local chardirections     = characters.directions
local charmirrors        = characters.mirrors
local charclasses        = characters.textclasses

local directions         = typesetters.directions
local getglobal          = directions.getglobal

local a_state            = attributes.private('state')
local a_directions       = attributes.private('directions')

local strip              = false

local s_isol             = fonts.analyzers.states.isol

local function stopdir(finish)
    return new_textdir(finish == "TRT" and "-TRT" or "-TLT")
end

local function startdir(finish)
    return new_textdir(finish == "TRT" and "+TRT" or "+TLT")
end

local function process(namespace,attribute,start)

    local head     = start

    local current  = head
    local inserted = nil
    local finish   = nil
    local autodir  = 0
    local embedded = 0
    local override = 0
    local pardir   = 1
    local textdir  = 1
    local done     = false
    local finished = nil
    local finidir  = nil
    local stack    = { }
    local top      = 0
    local obsolete = { }
    local lro      = false
    local lro      = false
    local prevattr = false
    local fences   = { }

    local function finish_auto_before()
        head, inserted = insert_node_before(head,current,stopdir(finish))
        finished, finidir, autodir = inserted, finish, 0
        finish, done = nil, true
    end

    local function finish_auto_after()
        head, current = insert_node_after(head,current,stopdir(finish))
        finished, finidir, autodir = current, finish, 0
        finish, done = nil, true
    end

    local function force_auto_left_before(direction)
        if finish then
            head, inserted = insert_node_before(head,current,stopdir(finish))
            finished = inserted
            finidir  = finish
        end
        if embedded >= 0 then
            finish, autodir = "TLT",  1
        else
            finish, autodir = "TRT", -1
        end
        done = true
        if finidir == finish then
            head = remove_node(head,finished,true)
        else
            head, inserted = insert_node_before(head,current,startdir(finish))
        end
    end

    local function force_auto_right_before(direction)
        if finish then
            head, inserted = insert_node_before(head,current,stopdir(finish))
            finished = inserted
            finidir  = finish
        end
        if embedded <= 0 then
            finish, autodir, done = "TRT", -1
        else
            finish, autodir, done = "TLT",  1
        end
        done = true
        if finidir == finish then
            head = remove_node(head,finished,true)
        else
            head, inserted = insert_node_before(head,current,startdir(finish))
        end
    end

    local function nextisright(current)
        current = current.next
        local id = current.id
        if id == glyph_code then
            local character = current.char
            local direction = chardirections[character]
            return direction == "r" or direction == "al" or direction == "an"
        end
    end

    local function previsright(current)
        current = current.prev
        local id = current.id
        if id == glyph_code then
            local char = current.char
            local direction = chardirections[character]
            return direction == "r" or direction == "al" or direction == "an"
        end
    end

    while current do
        local id = current.id
        if id == math_code then
            current = end_of_math(current.next).next
        else
            local attr = current[attribute]
            if attr and attr > 0 and attr ~= prevattr then
                if not getglobal(a) then
                    lro, rlo = false, false
                end
                prevattr = attr
            end
            if id == glyph_code then
                if attr and attr > 0 then
                    local character = current.char
                    local direction = chardirections[character]
                    local reversed  = false
                    if rlo or override > 0 then
                        if direction == "l" then
                            direction = "r"
                            reversed = true
                        end
                    elseif lro or override < 0 then
                        if direction == "r" or direction == "al" then
                            current[a_state] = s_isol
                            direction = "l"
                            reversed = true
                        end
                    end
                    if direction == "on" then
                        local mirror = charmirrors[character]
                        if mirror and fontchar[current.font][mirror] then
                            local class = charclasses[character]
                            if class == "open" then
                                if nextisright(current) then
                                    if autodir >= 0 then
                                        force_auto_right_before(direction)
                                    end
                                    current.char = mirror
                                    done = true
                                elseif autodir < 0 then
                                    current.char = mirror
                                    done = true
                                else
                                    mirror = false
                                end
                                local fencedir = autodir == 0 and textdir or autodir
                                fences[#fences+1] = fencedir
                            elseif class == "close" and #fences > 0 then
                                local fencedir = fences[#fences]
                                fences[#fences] = nil
                                if fencedir < 0 then
                                    current.char = mirror
                                    done = true
                                    force_auto_right_before(direction)
                                else
                                    mirror = false
                                end
                            elseif autodir < 0 then
                                current.char = mirror
                                done = true
                            else
                                mirror = false
                            end
                        end
                    elseif direction == "l" then
                        if autodir <= 0 then -- could be option
                            force_auto_left_before(direction)
                        end
                    elseif direction == "r" then
                        if autodir >= 0 then
                            force_auto_right_before(direction)
                        end
                    elseif direction == "en" then -- european number
                        if autodir <= 0 then -- could be option
                            force_auto_left_before(direction)
                        end
                    elseif direction == "al" then -- arabic number
                        if autodir >= 0 then
                            force_auto_right_before(direction)
                        end
                    elseif direction == "an" then -- arabic number
                        if autodir >= 0 then
                            force_auto_right_before(direction)
                        end
                    elseif direction == "lro" then -- Left-to-Right Override -> right becomes left
                        top = top + 1
                        stack[top] = { override, embedded }
                        override = -1
                        obsolete[#obsolete+1] = current
                    elseif direction == "rlo" then -- Right-to-Left Override -> left becomes right
                        top = top + 1
                        stack[top] = { override, embedded }
                        override = 1
                        obsolete[#obsolete+1] = current
                    elseif direction == "lre" then -- Left-to-Right Embedding -> TLT
                        top = top + 1
                        stack[top] = { override, embedded }
                        embedded = 1
                        obsolete[#obsolete+1] = current
                    elseif direction == "rle" then -- Right-to-Left Embedding -> TRT
                        top = top + 1
                        stack[top] = { override, embedded }
                        embedded = -1
                        obsolete[#obsolete+1] = current
                    elseif direction == "pdf" then -- Pop Directional Format
                        if top > 0 then
                            local s = stack[top]
                            override, embedded = s[1], s[2]
                            top = top - 1
                        end
                        obsolete[#obsolete+1] = current
                    end
                else
                    -- we do nothing
                end
            elseif id == whatsit_code then
                local subtype = current.subtype
                if subtype == localpar_code then
                    local dir = current.dir
                    if dir == 'TRT' then
                        autodir = -1
                    elseif dir == 'TLT' then
                        autodir = 1
                    end
                    pardir = autodir
                    textdir = pardir
                elseif subtype == dir_code then
                    -- todo: also treat as lro|rlo and stack
                    if finish then
                        finish_auto_before()
                    end
                    local dir = current.dir
                    if dir == "+TRT" then
                        finish, autodir = "TRT", -1
                    elseif dir == "-TRT" then
                        finish, autodir = nil, 0
                    elseif dir == "+TLT" then
                        finish, autodir = "TLT", 1
                    elseif dir == "-TLT" then
                        finish, autodir = nil, 0
                    end
                    textdir = autodir
                else
                    if finish then
                        finish_auto_before()
                    end
                end
            elseif finish then
                finish_auto_before()
            end
            local cn = current.next
            if cn then
                -- we're okay
            elseif finish then
                finish_auto_after()
            end
            current = cn
        end
    end

    if done and strip then
        local n = #obsolete
        if n > 0 then
            for i=1,n do
                remove_node(head,obsolete[i],true)
            end
            report_directions("%s character nodes removed",n)
        end
    end

    return head, done

end

directions.installhandler(interfaces.variables.default,process)

