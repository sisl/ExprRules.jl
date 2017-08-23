__precompile__()

module ExprRules

import TreeView: walk_tree
using StatsBase

export
        RuleSet,
        RuleNode,
        NodeLoc,

        ExpressionIterator,

        @ruleset,
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
struct RuleSet
    rules::Vector{Any}    # list of RHS of rules (subexpressions)
    types::Vector{Symbol} # list of LHS of rules (types, all symbols)
    isterminal::BitVector # whether rule i is terminal
    iseval::BitVector     # whether rule i is an eval rule
    bytype::Dict{Symbol,Vector{Int}}   # maps type to all rules of said type
    childtypes::Vector{Vector{Symbol}} # list of types of the children for each rule. Empty if terminal
end
macro ruleset(ex)
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
    return RuleSet(rules, types, is_terminal, is_eval, bytype, childtypes)
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

Base.getindex(ruleset::RuleSet, typ::Symbol) = ruleset.bytype[typ]

nonterminals(ruleset::RuleSet) = collect(keys(ruleset.bytype))
return_type(ruleset::RuleSet, rule_index::Int) = ruleset.types[rule_index]
child_types(ruleset::RuleSet, rule_index::Int) = ruleset.childtypes[rule_index]
isterminal(ruleset::RuleSet, rule_index::Int) = ruleset.isterminal[rule_index]
nchildren(ruleset::RuleSet, rule_index::Int) = length(ruleset.childtypes[rule_index])
max_arity(ruleset::RuleSet) = maximum(length(cs) for cs in ruleset.childtypes)


"""
    RuleNode

INSERT DOCS HERE
"""
struct RuleNode
    ind::Int # index in ruleset
    _val::Nullable{Any} #value of _() evals
    children::Vector{RuleNode}
end
RuleNode(ind::Int) = RuleNode(ind, Nullable{Any}(), RuleNode[])
RuleNode(ind::Int, children::Vector{RuleNode}) = RuleNode(ind, Nullable{Any}(), children)
RuleNode(ind::Int, _val::Any) = RuleNode(ind, Nullable{Any}(_val), RuleNode[])

return_type(ruleset::RuleSet, node::RuleNode) = ruleset.types[node.ind]
child_types(ruleset::RuleSet, node::RuleNode) = ruleset.childtypes[node.ind]
isterminal(ruleset::RuleSet, node::RuleNode) = ruleset.isterminal[node.ind]
nchildren(ruleset::RuleSet, node::RuleNode) = length(child_types(ruleset, node))

"""
    contains_returntype(node::RuleNode, sym::Symbol)

Returns true if the tree rooted at node contains at least one node with the given return type.
"""
function contains_returntype(node::RuleNode, ruleset::RuleSet, sym::Symbol)
    if return_type(ruleset, node) == sym
        return true
    end
    for c in node.children
        if contains_returntype(c, ruleset, sym)
            return true
        end
    end
    return false
end

function Base.isequal(A::RuleNode, B::RuleNode)
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
    get_executable(rulenode::RuleNode, ruleset::RuleSet)

INSERT DOCS HERE
"""
function _get_executable(expr::Expr, rulenode::RuleNode, ruleset::RuleSet)
    j = 0
    stack = Expr[expr]
    while !isempty(stack)
        ex = pop!(stack)
        for (k,arg) in enumerate(ex.args)
            if haskey(ruleset.bytype, arg)
                child = rulenode.children[j+=1]
                ex.args[k] = !isnull(child._val) ?
                    get(child._val) : deepcopy(ruleset.rules[child.ind])
                if !isterminal(ruleset, child)
                    ex.args[k] = _get_executable(ex.args[k], child, ruleset)
                end
            elseif isa(arg, Expr)
                push!(stack, arg)
            end
        end
    end
    return expr
end
function _get_executable(typ::Symbol, rulenode::RuleNode, ruleset::RuleSet)
    retval = typ
    if haskey(ruleset.bytype, typ)
        child = rulenode.children[1]
        retval = !isnull(child._val) ?
            get(child._val) : deepcopy(ruleset.rules[child.ind])
        if !ruleset.isterminal[child.ind]
            retval = _get_executable(retval, child, ruleset)
        end
    end
    retval
end
function get_executable(rulenode::RuleNode, ruleset::RuleSet)
    root = !isnull(rulenode._val) ?
        get(rulenode._val) : deepcopy(ruleset.rules[rulenode.ind])
    if !ruleset.isterminal[rulenode.ind] # not terminal
        root = _get_executable(root, rulenode, ruleset)
    end
    return root
end

Core.eval(rulenode::RuleNode, ruleset::RuleSet) = eval(Main, get_executable(rulenode, ruleset))
function Base.display(rulenode::RuleNode, ruleset::RuleSet)
    root = get_executable(rulenode, ruleset)
    if isa(root, Expr)
        walk_tree(root)
    else
        root
    end
end

"""
    rand(::Type{RuleNode}, ruleset::RuleSet, typ::Symbol, max_depth::Int=10)

Generates a random RuleNode of return type typ and maximum depth max_depth.
"""
function Base.rand(::Type{RuleNode}, ruleset::RuleSet, typ::Symbol, max_depth::Int=10)
    rules = ruleset[typ]
    rule_index = max_depth > 1 ?
        StatsBase.sample(rules) :
        StatsBase.sample([r for r in rules if isterminal(ruleset, r)])

    rulenode = ruleset.iseval[rule_index] ?
        RuleNode(rule_index, eval(Main, ruleset.rules[rule_index].args[2])) :
        RuleNode(rule_index)

    if !ruleset.isterminal[rule_index]
        for ch in child_types(ruleset, rule_index)
            push!(rulenode.children, rand(RuleNode, ruleset, ch, max_depth-1))
        end
    end
    return rulenode
end

"""
    sample(root::RuleNode, typ::Symbol, ruleset::RuleSet)

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
    sample(rulenode::RuleNode, typ::Symbol, ruleset::RuleSet)

Selects a uniformly random node of the given return type, typ.
"""
function StatsBase.sample(root::RuleNode, typ::Symbol, ruleset::RuleSet)
    i = 0
    selected = root
    stack = RuleNode[root]
    while !isempty(stack)
        node = pop!(stack)
        if ruleset.types[node.ind] == typ
            i += 1
            if rand() ≤ 1/i
                selected = node
            end
        end
        append!(stack, node.children)
    end
    ruleset.types[selected.ind] == typ || error("type $typ not found in RuleNode")
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
    sample(::Type{NodeLoc}, root::RuleNode, typ::Symbol, ruleset::RuleSet)

Selects a uniformly random node in the tree of a given type, specified using its parent such that the subtree can be replaced.
Returns a tuple (rulenode::RuleNode, i::Int) where rulenode is the parent RuleNode and i is the index
in rulenode.children corresponding to the selected node.
If the root node is selected, rulenode is the root node and i is 0.
"""
function StatsBase.sample(::Type{NodeLoc}, root::RuleNode, typ::Symbol, ruleset::RuleSet)
    i = 1
    selected = root_node_loc(root)
    stack = RuleNode[root]
    while !isempty(stack)
        node = pop!(stack)
        if ruleset.types[node.ind] == typ
            for (j,child) in enumerate(node.children)
                i += 1
                if rand() ≤ 1/i
                    selected = NodeLoc(node, j)
                end
                append!(stack, child.children)
            end
        end
    end
    ruleset.types[get(root,selected).ind] == typ || error("type $typ not found in RuleNode")
    return selected
end

###

function _next_state!(node::RuleNode, ruleset::RuleSet, max_depth::Int)

    if max_depth < 1
        return (node, false) # did not work
    elseif isterminal(ruleset, node)
        # do nothing
        return (node, false) # cannot change leaves
    else # !isterminal
        if isempty(node.children)
            if max_depth ≤ 1
                return (node,false) # cannot expand
            end

            # build out the node
            for c in child_types(ruleset, node)

                worked = false
                i = 0
                child = RuleNode(0)
                child_rules = ruleset[c]
                while !worked && i < length(child_rules)
                    i += 1
                    child = RuleNode(child_rules[i])
                    worked = true
                    if !isterminal(ruleset, child)
                        child, worked = _next_state!(child, ruleset, max_depth-1)
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

                child, child_worked = _next_state!(child, ruleset, max_depth-1)
                while !child_worked
                    child_type = return_type(ruleset, child)
                    child_rules = ruleset[child_type]
                    i = findfirst(child_rules, child.ind)
                    if i < length(child_rules)
                        child_worked = true
                        child = RuleNode(child_rules[i+1])
                        if !isterminal(ruleset, child)
                            child, child_worked = _next_state!(child, ruleset, max_depth-1)
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
                        c = child_types(ruleset, node)[child_index2]
                        worked = false
                        i = 0
                        child = RuleNode(0)
                        child_rules = ruleset[c]
                        while !worked && i < length(child_rules)
                            i += 1
                            child = RuleNode(child_rules[i])
                            worked = true
                            if !isterminal(ruleset, child)
                                child, worked = _next_state!(child, ruleset, max_depth-1)
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
    ruleset::RuleSet
    max_depth::Int
    sym::Symbol
end
Base.iteratorsize(::ExpressionIterator) = Base.SizeUnknown()
Base.eltype(::ExpressionIterator) = RuleNode
Base.done(iter::ExpressionIterator, state::Tuple{RuleNode,Bool}) = !state[2]
function Base.start(iter::ExpressionIterator)
    node = RuleNode(iter.ruleset[iter.sym][1])
    return _next_state!(node, iter.ruleset, iter.max_depth)
end
function Base.next(iter::ExpressionIterator, state::Tuple{RuleNode,Bool})
    ruleset, max_depth = iter.ruleset, iter.max_depth
    item = deepcopy(state[1])
    node, worked = _next_state!(state[1], ruleset, max_depth)

    while !worked
        # increment root's rule
        rules = ruleset[iter.sym]
        i = findfirst(rules, node.ind)
        if i < length(rules)
            node, worked = RuleNode(rules[i+1]), true
            if !isterminal(ruleset, node)
                node, worked = _next_state!(node, ruleset, max_depth)
            end
        else
            break
        end
    end

    state = (node, worked)
    return (item, state)
end
function count_expressions(ruleset::RuleSet, max_depth::Int, sym::Symbol)
    retval = 0
    for root_rule in ruleset[sym]
        node = RuleNode(root_rule)
        if isterminal(ruleset, node)
            retval += 1
        else
            node, worked = _next_state!(node, ruleset, max_depth)
            while worked
                retval += 1
                node, worked = _next_state!(node, ruleset, max_depth)
            end
        end
    end
    return retval
end
count_expressions(iter::ExpressionIterator) = count_expressions(iter.ruleset, iter.max_depth, iter.sym)


end # module
