__precompile__()

module ExprRules

import TreeView: walk_tree
using StatsBase

export
        RuleSet,
        RuleNode,
        NodeLoc,

        @ruleset,
        isterminal,
        get_executable,
        sample,
        numnodes

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
TODO: docs
- generated via macro
- maybe have an example of use
"""
struct RuleSet
    rules::Vector{Any}
    types::Vector{Symbol}
    isterminal::BitVector
    bytype::Dict{Symbol,Vector{Int}}
end
macro ruleset(ex)
    rules = Any[]
    types = Symbol[]
    bytype = Dict{Symbol,Vector{Int}}()
    for e in ex.args
        if e.head == :(=)
            s = e.args[1] # name of return type
            r = e.args[2] # expression?
            push!(rules, r)
            push!(types, s)
            bytype[s] = push!(get(bytype, s, Int[]), length(rules))
        end
    end
    alltypes = collect(keys(bytype))
    is_terminal = [isterminal(rule, alltypes) for rule in rules]
    return RuleSet(rules, types, is_terminal, bytype)
end

"""
    RuleNode

INSERT DOCS HERE
"""
struct RuleNode
    ind::Int # index in ruleset
    children::Vector{RuleNode}
end
RuleNode(ind::Int) = RuleNode(ind, RuleNode[])

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


"""
    get_executable(rulenode::RuleNode, ruleset::RuleSet)

INSERT DOCS HERE
"""
function _get_executable!(expr::Expr, rulenode::RuleNode, ruleset::RuleSet)
    j = 0
    stack = Expr[expr]
    while !isempty(stack)
        ex = pop!(stack)
        for (k,arg) in enumerate(ex.args)
            if haskey(ruleset.bytype, arg)
                child = rulenode.children[j+=1]
                ex.args[k] = deepcopy(ruleset.rules[child.ind])
                if !ruleset.isterminal[child.ind]
                    _get_executable!(ex.args[k], child, ruleset)
                end
            elseif isa(arg, Expr)
                push!(stack, arg)
            end
        end
    end
    return expr
end
function get_executable(rulenode::RuleNode, ruleset::RuleSet)
    root = deepcopy(ruleset.rules[rulenode.ind])
    if !ruleset.isterminal[rulenode.ind] # not terminal
        _get_executable!(root, rulenode, ruleset)
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

    rulenode = RuleNode(rule_index)

    if !ruleset.isterminal[rule_index]
        # add children
        for arg in ruleset.rules[rule_index].args
            if haskey(ruleset.bytype, arg)
                child = rand(RuleNode, ruleset, arg, max_depth-1)
                push!(rulenode.children, child)
            end
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
"""
struct NodeLoc
    parent::RuleNode
    i::Int
end

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
    selected = NodeLoc(root, 0)
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

end # module