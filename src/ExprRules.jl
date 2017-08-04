__precompile__()

module ExprRules

import TreeView: walk_tree
using StatsBase

export
        RuleSet,
        RuleNode,
        NodeLoc,

        @ruleset,
        @digits,
        depth,
        isterminal,
        return_type,
        child_types,
        nonterminals,
        get_executable,
        sample,
        numnodes,
        root_node_loc

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

nonterminals(ruleset::RuleSet) = collect(keys(ruleset.bytype))
return_type(ruleset::RuleSet, rule_index::Int) = ruleset.types[rule_index]
child_types(ruleset::RuleSet, rule_index::Int) = ruleset.childtypes[rule_index]
isterminal(ruleset::RuleSet, rule_index::Int) = ruleset.isterminal[rule_index]


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
isterminal(ruleset::RuleSet,  node::RuleNode) = ruleset.isterminal[node.ind]

function Base.isequal(A::RuleNode, B::RuleNode)
    if A.ind != B.ind || A._val != B._val
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
                if !ruleset.isterminal[child.ind]
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

    rules = ruleset.bytype[typ]
    rule_index = max_depth > 1 ?
        StatsBase.sample([rules[i] for i in 1 : length(rules)]) :
        StatsBase.sample([rules[i] for i in 1 : length(rules) if ruleset.isterminal[i]])

    rulenode = ruleset.iseval[rule_index] ?
        RuleNode(rule_index, eval(Main, ruleset.rules[rule_index].args[2])) :
        RuleNode(rule_index)

    if !ruleset.isterminal[rule_index]
        for arg in ruleset.rules[rule_index].args
            _add_children!(rulenode, ruleset, arg, max_depth-1)
        end
    end
    rulenode
end
function _add_children!(rulenode::RuleNode, ruleset::RuleSet, arg, max_depth::Int)
    if haskey(ruleset.bytype, arg)
        child = rand(RuleNode, ruleset, arg, max_depth-1)
        push!(rulenode.children, child)
    end
end
function _add_children!(rulenode::RuleNode, ruleset::RuleSet, ex::Expr, max_depth::Int)
    for arg in ex.args
        _add_children!(rulenode, ruleset, arg, max_depth)
    end
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

end # module
