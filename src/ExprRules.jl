__precompile__()

module ExprRules

import TreeView: walk_tree
using StatsBase

export
        Grammar,
        RuleNode,
        NodeLoc,

        ExpressionIterator,

        @grammar,
        @digits,
        max_arity,
        depth,
        isterminal,
        return_type,
        contains_returntype,
        nchildren,
        child_types,
        nonterminals,
        get_executable,
        sample,
        root_node_loc,
        count_expressions

"""
isterminal(rule::Any, types::AbstractVector{Symbol})

Returns true if the rule is terminal, ie does not contain any of the types in the provided vector.
For example, :(x) is terminal, and :(1+1) is terminal, but :(Real + Real) is typically not.
"""
function isterminal(rule::Any, types::AbstractVector{Symbol})
    if isa(rule, Expr)
        for arg in rule.args
            if !isterminal(arg, types)
                return false
            end
        end
    end
    return rule ∉ types
end

"""
iseval(rule::Any)

Returns true if the rule is the special evaluate immediately function, i.e., _()
"""
iseval(rule) = false
iseval(rule::Expr) = (rule.head == :call && rule.args[1] == :_)

function get_childtypes(rule::Any, types::AbstractVector{Symbol})
    retval = Symbol[]
    if isa(rule, Expr)
        for arg in rule.args
            append!(retval, get_childtypes(arg, types))
        end
    elseif rule ∈ types
        push!(retval, rule)
    end
    return retval
end

"""
TODO: docs
- generated via macro
- maybe have an example of use
"""
struct Grammar
    rules::Vector{Any}    # list of RHS of rules (subexpressions)
    types::Vector{Symbol} # list of LHS of rules (types, all symbols)
    isterminal::BitVector # whether rule i is terminal
    iseval::BitVector     # whether rule i is an eval rule
    bytype::Dict{Symbol,Vector{Int}}   # maps type to all rules of said type
    childtypes::Vector{Vector{Symbol}} # list of types of the children for each rule. Empty if terminal
end
macro grammar(ex)
    rules = Any[]
    types = Symbol[]
    bytype = Dict{Symbol,Vector{Int}}()
    for e in ex.args
        if e.head == :(=)
            s = e.args[1] # name of return type
            rule = e.args[2] # expression?
            rvec = Any[]
            _parse_rule!(rvec, rule)
            for r in rvec
                push!(rules, r)
                push!(types, s)
                bytype[s] = push!(get(bytype, s, Int[]), length(rules))
            end
        end
    end
    alltypes = collect(keys(bytype))
    is_terminal = [isterminal(rule, alltypes) for rule in rules]
    is_eval = [iseval(rule) for rule in rules]
    childtypes = [get_childtypes(rule, alltypes) for rule in rules]
    return Grammar(rules, types, is_terminal, is_eval, bytype, childtypes)
end
_parse_rule!(v::Vector{Any}, r) = push!(v, r)
function _parse_rule!(v::Vector{Any}, ex::Expr)
    if ex.head == :call && ex.args[1] == :|
         terms = length(ex.args) == 2 ?
            collect(eval(Main,ex.args[2])) :    #|(a:c) case
            ex.args[2:end]                      #a|b|c case
        for t in terms
            _parse_rule!(v, t)
        end
    else
        push!(v, ex)
    end
end

Base.getindex(grammar::Grammar, typ::Symbol) = grammar.bytype[typ]

nonterminals(grammar::Grammar) = collect(keys(grammar.bytype))
return_type(grammar::Grammar, rule_index::Int) = grammar.types[rule_index]
child_types(grammar::Grammar, rule_index::Int) = grammar.childtypes[rule_index]
isterminal(grammar::Grammar, rule_index::Int) = grammar.isterminal[rule_index]
nchildren(grammar::Grammar, rule_index::Int) = length(grammar.childtypes[rule_index])
max_arity(grammar::Grammar) = maximum(length(cs) for cs in grammar.childtypes)


"""
    RuleNode

INSERT DOCS HERE
"""
struct RuleNode
    ind::Int # index in grammar
    _val::Nullable{Any} #value of _() evals
    children::Vector{RuleNode}
end
RuleNode(ind::Int) = RuleNode(ind, Nullable{Any}(), RuleNode[])
RuleNode(ind::Int, children::Vector{RuleNode}) = RuleNode(ind, Nullable{Any}(), children)
RuleNode(ind::Int, _val::Any) = RuleNode(ind, Nullable{Any}(_val), RuleNode[])

return_type(grammar::Grammar, node::RuleNode) = grammar.types[node.ind]
child_types(grammar::Grammar, node::RuleNode) = grammar.childtypes[node.ind]
isterminal(grammar::Grammar, node::RuleNode) = grammar.isterminal[node.ind]
nchildren(grammar::Grammar, node::RuleNode) = length(child_types(grammar, node))

"""
    contains_returntype(node::RuleNode, sym::Symbol)

Returns true if the tree rooted at node contains at least one node with the given return type.
"""
function contains_returntype(node::RuleNode, grammar::Grammar, sym::Symbol)
    if return_type(grammar, node) == sym
        return true
    end
    for c in node.children
        if contains_returntype(c, grammar, sym)
            return true
        end
    end
    return false
end

function Base.:(==)(A::RuleNode, B::RuleNode)
    if A.ind != B.ind ||
      (!isnull(A._val) && isnull(B._val)) ||
      ( isnull(A._val) && !isnull(B._val)) ||
      (!isnull(A._val) && !isnull(B._val) && A._val == B._val)

        return false
    else
        for (a,b) in zip(A.children, B.children)
            if !isequal(a, b)
                return false
            end
        end
        return true
    end
end
function Base.hash(node::RuleNode, h::UInt=zero(UInt))
    retval = hash(node.ind, h)
    for child in node.children
        retval = hash(child, retval)
    end
    return retval
end

function Base.show(io::IO, node::RuleNode; separator=",", last_child::Bool=false)
    print(io, node.ind)
    if !isempty(node.children)
        print(io, "{")
        for (i,c) in enumerate(node.children)
            show(io, c, separator=separator, last_child=(i == length(node.children)))
        end
        print(io, "}")
    elseif !last_child
        print(io, separator)
    end
end

"""
Return the number of vertices in the tree rooted at root.
"""
function Base.length(root::RuleNode)
    retval = 1
    for c in root.children
        retval += length(c)
    end
    return retval
end
function depth(root::RuleNode)
    retval = 1
    for c in root.children
        retval = max(retval, depth(c)+1)
    end
    return retval
end


"""
    get_executable(rulenode::RuleNode, grammar::Grammar)

INSERT DOCS HERE
"""
function _get_executable(expr::Expr, rulenode::RuleNode, grammar::Grammar)
    j = 0
    stack = Expr[expr]
    while !isempty(stack)
        ex = pop!(stack)
        for (k,arg) in enumerate(ex.args)
            if haskey(grammar.bytype, arg)
                child = rulenode.children[j+=1]
                ex.args[k] = !isnull(child._val) ?
                    get(child._val) : deepcopy(grammar.rules[child.ind])
                if !isterminal(grammar, child)
                    ex.args[k] = _get_executable(ex.args[k], child, grammar)
                end
            elseif isa(arg, Expr)
                push!(stack, arg)
            end
        end
    end
    return expr
end
function _get_executable(typ::Symbol, rulenode::RuleNode, grammar::Grammar)
    retval = typ
    if haskey(grammar.bytype, typ)
        child = rulenode.children[1]
        retval = !isnull(child._val) ?
            get(child._val) : deepcopy(grammar.rules[child.ind])
        if !grammar.isterminal[child.ind]
            retval = _get_executable(retval, child, grammar)
        end
    end
    retval
end
function get_executable(rulenode::RuleNode, grammar::Grammar)
    root = !isnull(rulenode._val) ?
        get(rulenode._val) : deepcopy(grammar.rules[rulenode.ind])
    if !grammar.isterminal[rulenode.ind] # not terminal
        root = _get_executable(root, rulenode, grammar)
    end
    return root
end

Core.eval(rulenode::RuleNode, grammar::Grammar) = eval(Main, get_executable(rulenode, grammar))
function Base.display(rulenode::RuleNode, grammar::Grammar)
    root = get_executable(rulenode, grammar)
    if isa(root, Expr)
        walk_tree(root)
    else
        root
    end
end

"""
    rand(::Type{RuleNode}, grammar::Grammar, typ::Symbol, max_depth::Int=10)

Generates a random RuleNode of return type typ and maximum depth max_depth.
"""
function Base.rand(::Type{RuleNode}, grammar::Grammar, typ::Symbol, max_depth::Int=10)
    rules = grammar[typ]
    rule_index = max_depth > 1 ?
        StatsBase.sample(rules) :
        StatsBase.sample([r for r in rules if isterminal(grammar, r)])

    rulenode = grammar.iseval[rule_index] ?
        RuleNode(rule_index, eval(Main, grammar.rules[rule_index].args[2])) :
        RuleNode(rule_index)

    if !grammar.isterminal[rule_index]
        for ch in child_types(grammar, rule_index)
            push!(rulenode.children, rand(RuleNode, grammar, ch, max_depth-1))
        end
    end
    return rulenode
end

"""
    sample(root::RuleNode, typ::Symbol, grammar::Grammar)

Selects a uniformly random node from the tree.
"""
function StatsBase.sample(root::RuleNode)
    i = 0
    selected = root
    stack = RuleNode[root]
    while !isempty(stack)
        node = pop!(stack)
        i += 1
        if rand() ≤ 1/i
            selected = node
        end
        append!(stack, node.children)
    end
    return selected
end

"""
    sample(rulenode::RuleNode, typ::Symbol, grammar::Grammar)

Selects a uniformly random node of the given return type, typ.
"""
function StatsBase.sample(root::RuleNode, typ::Symbol, grammar::Grammar)
    i = 0
    selected = root
    stack = RuleNode[root]
    while !isempty(stack)
        node = pop!(stack)
        if grammar.types[node.ind] == typ
            i += 1
            if rand() ≤ 1/i
                selected = node
            end
        end
        append!(stack, node.children)
    end
    grammar.types[selected.ind] == typ || error("type $typ not found in RuleNode")
    return selected
end

"""
    NodeLoc

A helper struct that points to a node in the tree via its parent such that the child can be easily
swapped out.
If i is 0 the node pointed to is the root node and parent is the node itself.
"""
struct NodeLoc
    parent::RuleNode
    i::Int
end
root_node_loc(root::RuleNode) = NodeLoc(root, 0)

"""
    get(root::RuleNode, loc::NodeLoc)

Obtain the node pointed to by loc.
"""
function Base.get(root::RuleNode, loc::NodeLoc)
    parent, i = loc.parent, loc.i
    if loc.i > 0
        return parent.children[i]
    else
        return root
    end
end


"""
    insert!(loc::NodeLoc, rulenode::RuleNode)

Replaces the subtree pointed to by loc with the given rulenode.
"""
function Base.insert!(root::RuleNode, loc::NodeLoc, rulenode::RuleNode)
    parent, i = loc.parent, loc.i
    if loc.i > 0
        parent.children[i] = rulenode
    else
        root = rulenode
    end
    return root
end

"""
    sample(::Type{NodeLoc}, root::RuleNode)

Selects a uniformly random node in the tree, specified using its parent such that the subtree can be replaced.
Returns a tuple (rulenode::RuleNode, i::Int) where rulenode is the parent RuleNode and i is the index
in rulenode.children corresponding to the selected node.
If the root node is selected, rulenode is the root node and i is 0.
"""
function StatsBase.sample(::Type{NodeLoc}, root::RuleNode)
    i = 1
    selected = root_node_loc(root)
    stack = RuleNode[root]
    while !isempty(stack)
        node = pop!(stack)
        for (j,child) in enumerate(node.children)
            i += 1
            if rand() ≤ 1/i
                selected = NodeLoc(node, j)
            end
            append!(stack, child.children)
        end
    end
    return selected
end

"""
    sample(::Type{NodeLoc}, root::RuleNode, typ::Symbol, grammar::Grammar)

Selects a uniformly random node in the tree of a given type, specified using its parent such that the subtree can be replaced.
Returns a tuple (rulenode::RuleNode, i::Int) where rulenode is the parent RuleNode and i is the index
in rulenode.children corresponding to the selected node.
If the root node is selected, rulenode is the root node and i is 0.
"""
function StatsBase.sample(::Type{NodeLoc}, root::RuleNode, typ::Symbol, grammar::Grammar)
    i = 1
    selected = root_node_loc(root)
    stack = RuleNode[root]
    while !isempty(stack)
        node = pop!(stack)
        if grammar.types[node.ind] == typ
            for (j,child) in enumerate(node.children)
                i += 1
                if rand() ≤ 1/i
                    selected = NodeLoc(node, j)
                end
                append!(stack, child.children)
            end
        end
    end
    grammar.types[get(root,selected).ind] == typ || error("type $typ not found in RuleNode")
    return selected
end

###

function _next_state!(node::RuleNode, grammar::Grammar, max_depth::Int)

    if max_depth < 1
        return (node, false) # did not work
    elseif isterminal(grammar, node)
        # do nothing
        return (node, false) # cannot change leaves
    else # !isterminal
        if isempty(node.children)
            if max_depth ≤ 1
                return (node,false) # cannot expand
            end

            # build out the node
            for c in child_types(grammar, node)

                worked = false
                i = 0
                child = RuleNode(0)
                child_rules = grammar[c]
                while !worked && i < length(child_rules)
                    i += 1
                    child = RuleNode(child_rules[i])
                    worked = true
                    if !isterminal(grammar, child)
                        child, worked = _next_state!(child, grammar, max_depth-1)
                    end
                end
                if !worked
                    return (node, false) # did not work
                end
                push!(node.children, child)
            end

            return (node, true)
        else # not empty
            # make one change, starting with rightmost child
            worked = false
            child_index = length(node.children) + 1
            while !worked && child_index > 1
                child_index -= 1
                child = node.children[child_index]

                child, child_worked = _next_state!(child, grammar, max_depth-1)
                while !child_worked
                    child_type = return_type(grammar, child)
                    child_rules = grammar[child_type]
                    i = findfirst(child_rules, child.ind)
                    if i < length(child_rules)
                        child_worked = true
                        child = RuleNode(child_rules[i+1])
                        if !isterminal(grammar, child)
                            child, child_worked = _next_state!(child, grammar, max_depth-1)
                        end
                        node.children[child_index] = child
                    else
                        break
                    end
                end

                if child_worked
                    worked = true

                    # reset remaining children
                    for child_index2 in child_index+1 : length(node.children)
                        c = child_types(grammar, node)[child_index2]
                        worked = false
                        i = 0
                        child = RuleNode(0)
                        child_rules = grammar[c]
                        while !worked && i < length(child_rules)
                            i += 1
                            child = RuleNode(child_rules[i])
                            worked = true
                            if !isterminal(grammar, child)
                                child, worked = _next_state!(child, grammar, max_depth-1)
                            end
                        end
                        if !worked
                            break
                        end
                        node.children[child_index2] = child
                    end
                end
            end

            return (node, worked)
        end
    end
end

mutable struct ExpressionIterator
    grammar::Grammar
    max_depth::Int
    sym::Symbol
end
Base.iteratorsize(::ExpressionIterator) = Base.SizeUnknown()
Base.eltype(::ExpressionIterator) = RuleNode
Base.done(iter::ExpressionIterator, state::Tuple{RuleNode,Bool}) = !state[2]
function Base.start(iter::ExpressionIterator)
    node = RuleNode(iter.grammar[iter.sym][1])
    if isterminal(iter.grammar, node)
        return (node, true)
    else
        return _next_state!(node, iter.grammar, iter.max_depth)
    end
end
function Base.next(iter::ExpressionIterator, state::Tuple{RuleNode,Bool})
    grammar, max_depth = iter.grammar, iter.max_depth
    item = deepcopy(state[1])
    node, worked = _next_state!(state[1], grammar, max_depth)

    while !worked
        # increment root's rule
        rules = grammar[iter.sym]
        i = findfirst(rules, node.ind)
        if i < length(rules)
            node, worked = RuleNode(rules[i+1]), true
            if !isterminal(grammar, node)
                node, worked = _next_state!(node, grammar, max_depth)
            end
        else
            break
        end
    end

    state = (node, worked)
    return (item, state)
end
function count_expressions(grammar::Grammar, max_depth::Int, sym::Symbol)
    retval = 0
    for root_rule in grammar[sym]
        node = RuleNode(root_rule)
        if isterminal(grammar, node)
            retval += 1
        else
            node, worked = _next_state!(node, grammar, max_depth)
            while worked
                retval += 1
                node, worked = _next_state!(node, grammar, max_depth)
            end
        end
    end
    return retval
end
count_expressions(iter::ExpressionIterator) = count_expressions(iter.grammar, iter.max_depth, iter.sym)


end # module
