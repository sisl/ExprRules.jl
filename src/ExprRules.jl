__precompile__()

module ExprRules

import TreeView: walk_tree
using StatsBase
using AbstractTrees
using DataStructures  #NodeRecycler

export
        Grammar,
        RuleNode,
        NodeLoc,

        ExpressionIterator,

        @grammar,
        max_arity,
        depth,
        node_depth,
        isterminal,
        iseval,
        return_type,
        contains_returntype,
        nchildren,
        child_types,
        nonterminals,
        get_executable,
        sample,
        root_node_loc,
        count_expressions,
        mindepth_map,
        mindepth,

        SymbolTable,
        interpret,

        NodeRecycler,
        recycle!


include("interpreter.jl")
using .Interpreter

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

"""
    get_childtypes(rule::Any, types::AbstractVector{Symbol})

Returns the child types of a production rule.
"""
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
    Grammar

Represents a grammar and its production rules.
Use the @grammar macro to create a Grammar object.
"""
struct Grammar
    rules::Vector{Any}    # list of RHS of rules (subexpressions)
    types::Vector{Symbol} # list of LHS of rules (types, all symbols)
    isterminal::BitVector # whether rule i is terminal
    iseval::BitVector     # whether rule i is an eval rule
    bytype::Dict{Symbol,Vector{Int}}   # maps type to all rules of said type
    childtypes::Vector{Vector{Symbol}} # list of types of the children for each rule. Empty if terminal
end
"""
    @grammar

Define a grammar and return it as a Grammar. For example:
```julia-repl
grammar = @grammar begin
    R = x
    R = 1 | 2
    R = R + R
end
```
"""
macro grammar(ex)
    rules = Any[]
    types = Symbol[]
    bytype = Dict{Symbol,Vector{Int}}()
    for e in ex.args
        if isa(e, Expr)
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
            collect(interpret(ex.args[2])) :    #|(a:c) case
            ex.args[2:end]                      #a|b|c case
        for t in terms
            _parse_rule!(v, t)
        end
    else
        push!(v, ex)
    end
end

Base.getindex(grammar::Grammar, typ::Symbol) = grammar.bytype[typ]

"""
   append!(grammar1::Grammar, grammar2::Grammar)

Appends the production rules of grammar2 to grammar1.
"""
function Base.append!(grammar1::Grammar, grammar2::Grammar)
    N = length(grammar1.rules)
    append!(grammar1.rules, grammar2.rules)
    append!(grammar1.types, grammar2.types)
    append!(grammar1.isterminal, grammar2.isterminal)
    append!(grammar1.iseval, grammar2.iseval)
    append!(grammar1.childtypes, copy.(grammar2.childtypes))
    for (s,v) in grammar2.bytype
        grammar1.bytype[s] = append!(get(grammar1.bytype, s, Int[]), N .+ v)
    end
    grammar1
end

"""
    nonterminals(grammar::Grammar)

Returns a list of nonterminals in the grammar.
"""
nonterminals(grammar::Grammar) = collect(keys(grammar.bytype))

"""
    return_type(grammar::Grammar, rule_index::Int)

Returns the type of the production rule at rule_index.
"""
return_type(grammar::Grammar, rule_index::Int) = grammar.types[rule_index]

"""
    child_types(grammar::Grammar, rule_index::Int)

Returns the types of the children (nonterminals) of the production rule at rule_index.
"""
child_types(grammar::Grammar, rule_index::Int) = grammar.childtypes[rule_index]

"""
    isterminal(grammar::Grammar, rule_index::Int)

Returns true if the production rule at rule_index is terminal, i.e., does not contain any nonterminal symbols.
"""
isterminal(grammar::Grammar, rule_index::Int) = grammar.isterminal[rule_index]

"""
    iseval(grammar::Grammar)

Returns true if any production rules in grammar contain the special _() eval function.
"""
iseval(grammar::Grammar) = any(grammar.iseval)

"""
    iseval(grammar::Grammar, rule_index::Int)

Returns true if the production rule at rule_index contains the special _() eval function.
"""
iseval(grammar::Grammar, index::Int) = grammar.iseval[index]

"""
    nchildren(grammar::Grammar, rule_index::Int)

Returns the number of children (nonterminals) of the production rule at rule_index.
"""
nchildren(grammar::Grammar, rule_index::Int) = length(grammar.childtypes[rule_index])

"""
    max_arity(grammar::Grammar)

Returns the maximum arity (number of children) over all production rules in the grammar.
"""
max_arity(grammar::Grammar) = maximum(length(cs) for cs in grammar.childtypes)


"""
    RuleNode

Type for representing nodes in an expression tree.
"""
mutable struct RuleNode
    ind::Int # index in grammar
    _val::Any  #value of _() evals
    children::Vector{RuleNode}
end
RuleNode(ind::Int) = RuleNode(ind, nothing, RuleNode[])
RuleNode(ind::Int, children::Vector{RuleNode}) = RuleNode(ind, nothing, children)
RuleNode(ind::Int, _val::Any) = RuleNode(ind, _val, RuleNode[])

include("recycler.jl")


"""
    return_types(grammar::Grammar, node::RuleNode)

Returns the return type in the production rule used by node.
"""
return_type(grammar::Grammar, node::RuleNode) = grammar.types[node.ind]

"""
    child_types(grammar::Grammar, node::RuleNode)

Returns the list of child types in the production rule used by node.
"""
child_types(grammar::Grammar, node::RuleNode) = grammar.childtypes[node.ind]

"""
    isterminal(grammar::Grammar, node::RuleNode)

Returns true if the production rule used by node is terminal, i.e., does not contain any nonterminal symbols.
"""
isterminal(grammar::Grammar, node::RuleNode) = grammar.isterminal[node.ind]

"""
    nchildren(grammar::Grammar, node::RuleNode)

Returns the number of children in the production rule used by node.
"""
nchildren(grammar::Grammar, node::RuleNode) = length(child_types(grammar, node))

"""
    contains_returntype(node::RuleNode, grammar::Grammar, sym::Symbol, maxdepth::Int=typemax(Int))

Returns true if the tree rooted at node contains at least one node at depth less than maxdepth
with the given return type.
"""
function contains_returntype(node::RuleNode, grammar::Grammar, sym::Symbol, maxdepth::Int=typemax(Int))
    maxdepth < 1 && return false
    if return_type(grammar, node) == sym
        return true
    end
    for c in node.children
        if contains_returntype(c, grammar, sym, maxdepth-1)
            return true
        end
    end
    return false
end
function Base.:(==)(A::RuleNode, B::RuleNode)
    (A.ind == B.ind) &&
        (A._val == B._val) && 
        (length(A.children) == length(B.children)) && #required because zip doesn't check lengths
        all(isequal(a,b) for (a,b) in zip(A.children, B.children))
end

function Base.hash(node::RuleNode, h::UInt=zero(UInt))
    retval = hash(node.ind, h)
    for child in node.children
        retval = hash(child, retval)
    end
    return retval
end

function Base.show(io::IO, grammar::Grammar)
    for i in eachindex(grammar.rules)
        println(io, i, ": ", grammar.types[i], " = ", grammar.rules[i])
    end
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

"""
    depth(root::RuleNode)

Return the depth of the expression tree rooted at root.
"""
function depth(root::RuleNode)
    retval = 1
    for c in root.children
        retval = max(retval, depth(c)+1)
    end
    return retval
end

"""
    node_depth(root::RuleNode, node::RuleNode)

Return the depth of node for an expression tree rooted at root.  Depth is 1 when root == node.
"""
function node_depth(root::RuleNode, node::RuleNode)
    root === node && return 1
    for c in root.children
        d = node_depth(c, node)
        d > 0 && (return d+1)
    end
    return 0
end

"""
    get_executable(rulenode::RuleNode, grammar::Grammar)

Returns the executable julia expression represented in the expression tree with root rulenode.  The returned expression can be evaluated using eval().
"""
function get_executable(rulenode::RuleNode, grammar::Grammar)
    root = (rulenode._val != nothing) ?
        rulenode._val : deepcopy(grammar.rules[rulenode.ind])
    if !grammar.isterminal[rulenode.ind] # not terminal
        root,j = _get_executable(root, rulenode, grammar)
    end
    return root
end
function _get_executable(expr::Expr, rulenode::RuleNode, grammar::Grammar, j=0)
    for (k,arg) in enumerate(expr.args)
        if isa(arg, Expr)
            expr.args[k],j = _get_executable(arg, rulenode, grammar, j)
        elseif haskey(grammar.bytype, arg)
            child = rulenode.children[j+=1]
            expr.args[k] = (child._val != nothing) ?
                child._val : deepcopy(grammar.rules[child.ind])
            if !isterminal(grammar, child)
                expr.args[k],_ = _get_executable(expr.args[k], child, grammar, 0)
            end
        end
    end
    return expr, j
end
function _get_executable(typ::Symbol, rulenode::RuleNode, grammar::Grammar, j=0)
    retval = typ
    if haskey(grammar.bytype, typ)
        child = rulenode.children[1]
        retval = (child._val != nothing) ?
            child._val : deepcopy(grammar.rules[child.ind])
        if !grammar.isterminal[child.ind]
            retval,_ = _get_executable(retval, child, grammar, 0)
        end
    end
    retval, j
end

"""
    Core.eval(rulenode::RuleNode, grammar::Grammar)

Evaluate the expression tree with root rulenode.
"""
Core.eval(rulenode::RuleNode, grammar::Grammar) = Core.eval(Main, get_executable(rulenode, grammar))
Core.eval(grammar::Grammar, index::Int) = Core.eval(Main, grammar.rules[index].args[2])
"""
    Core.eval(tab::SymbolTable, ex::Expr) 

Evaluate the expression ex using symbol table tab 
"""
Core.eval(tab::SymbolTable, ex) = interpret(tab, ex)
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
function Base.rand(::Type{RuleNode}, grammar::Grammar, typ::Symbol, max_depth::Int=10, 
    bin::Union{NodeRecycler,Nothing}=nothing)
    rules = grammar[typ]
    rule_index = max_depth > 1 ?
        StatsBase.sample(rules) :
        StatsBase.sample(filter(r->isterminal(grammar,r), rules))

    rulenode = iseval(grammar, rule_index) ?
        RuleNode(bin, rule_index, Core.eval(grammar, rule_index)) :
        RuleNode(bin, rule_index)

    if !grammar.isterminal[rule_index]
        for ch in child_types(grammar, rule_index)
            push!(rulenode.children, rand(RuleNode, grammar, ch, max_depth-1, bin))
        end
    end
    return rulenode
end
"""
    rand(::Type{RuleNode}, grammar::Grammar, typ::Symbol, dmap::AbstractVector{Int}, max_depth::Int=10)

Generates a random RuleNode of return type typ and maximum depth max_depth guided by a minimum depth map dmap.
"""
function Base.rand(::Type{RuleNode}, grammar::Grammar, typ::Symbol, dmap::AbstractVector{Int}, 
    max_depth::Int=10, bin::Union{NodeRecycler,Nothing}=nothing)
    rules = grammar[typ]
    rule_index = StatsBase.sample(filter(r->dmap[r] ≤ max_depth, rules))

    rulenode = iseval(grammar, rule_index) ?
        RuleNode(bin, rule_index, Core.eval(grammar, rule_index)) :
        RuleNode(bin, rule_index)

    if !grammar.isterminal[rule_index]
        for ch in child_types(grammar, rule_index)
            push!(rulenode.children, rand(RuleNode, grammar, ch, dmap, max_depth-1, bin))
        end
    end
    return rulenode
end

mutable struct RuleNodeAndCount
    node::RuleNode
    cnt::Int
end
"""
    sample(root::RuleNode, typ::Symbol, grammar::Grammar, maxdepth::Int=typemax(Int))

Selects a uniformly random node from the tree, limited to maxdepth.
"""
function StatsBase.sample(root::RuleNode, maxdepth::Int=typemax(Int))
    x = RuleNodeAndCount(root, 1)
    for child in root.children
        _sample(child, x, maxdepth-1)
    end
    x.node
end
function _sample(node::RuleNode, x::RuleNodeAndCount, maxdepth::Int)
    maxdepth < 1 && return
    x.cnt += 1
    if rand() <= 1/x.cnt
        x.node = node
    end
    for child in node.children
        _sample(child, x, maxdepth-1)
    end
end

"""
    sample(root::RuleNode, typ::Symbol, grammar::Grammar,
                          maxdepth::Int=typemax(Int))

Selects a uniformly random node of the given return type, typ, limited to maxdepth.
"""
function StatsBase.sample(root::RuleNode, typ::Symbol, grammar::Grammar,
                          maxdepth::Int=typemax(Int))
    x = RuleNodeAndCount(root, 0)
    if grammar.types[root.ind] == typ
        x.cnt += 1
    end
    for child in root.children
        _sample(child, typ, grammar, x, maxdepth-1)
    end
    grammar.types[x.node.ind] == typ || error("type $typ not found in RuleNode")
    x.node
end
function _sample(node::RuleNode, typ::Symbol, grammar::Grammar, x::RuleNodeAndCount,
                 maxdepth::Int)
    maxdepth < 1 && return
    if grammar.types[node.ind] == typ
        x.cnt += 1
        if rand() <= 1/x.cnt
            x.node = node
        end
    end
    for child in node.children
        _sample(child, typ, grammar, x, maxdepth-1)
    end
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

"""
    root_node_loc(root::RuleNode)

Returns a NodeLoc pointing to the root node.
"""
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

mutable struct NodeLocAndCount
    loc::NodeLoc
    cnt::Int
end
"""
    sample(::Type{NodeLoc}, root::RuleNode, maxdepth::Int=typemax(Int))

Selects a uniformly random node in the tree no deeper than maxdepth using reservoir sampling.
Returns a NodeLoc that specifies the location using its parent so that the subtree can be replaced.
"""
function StatsBase.sample(::Type{NodeLoc}, root::RuleNode, maxdepth::Int=typemax(Int))
    x = NodeLocAndCount(root_node_loc(root), 1)
    _sample(NodeLoc, root, x, maxdepth-1)
    x.loc
end
function _sample(::Type{NodeLoc}, node::RuleNode, x::NodeLocAndCount, maxdepth::Int)
    maxdepth < 1 && return
    for (j,child) in enumerate(node.children)
        x.cnt += 1
        if rand() <= 1/x.cnt
            x.loc = NodeLoc(node, j)
        end
        _sample(NodeLoc, child, x, maxdepth-1)
    end
end

"""
    sample(::Type{NodeLoc}, root::RuleNode, typ::Symbol, grammar::Grammar)

Selects a uniformly random node in the tree of a given type, specified using its parent such that the subtree can be replaced.
Returns a NodeLoc.
"""
function StatsBase.sample(::Type{NodeLoc}, root::RuleNode, typ::Symbol, grammar::Grammar,
                          maxdepth::Int=typemax(Int))
    x = NodeLocAndCount(root_node_loc(root), 0)
    if grammar.types[root.ind] == typ
        x.cnt += 1
    end
    _sample(NodeLoc, root, typ, grammar, x, maxdepth-1)
    grammar.types[get(root,x.loc).ind] == typ || error("type $typ not found in RuleNode")
    x.loc
end
function _sample(::Type{NodeLoc}, node::RuleNode, typ::Symbol, grammar::Grammar,
                 x::NodeLocAndCount, maxdepth::Int)
    maxdepth < 1 && return
    for (j,child) in enumerate(node.children)
        if grammar.types[child.ind] == typ
            x.cnt += 1
            if rand() <= 1/x.cnt
                x.loc = NodeLoc(node, j)
            end
        end
        _sample(NodeLoc, child, typ, grammar, x, maxdepth-1)
    end
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
                    i = something(findfirst(isequal(child.ind), child_rules), 0)
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

"""
    ExpressionIterator(grammar::Grammar, max_depth::Int, sym::Symbol)

An iterator over all possible expressions of a grammar up to max_depth with start symbol sym.
"""
mutable struct ExpressionIterator
    grammar::Grammar
    max_depth::Int
    sym::Symbol
end
Base.IteratorSize(::ExpressionIterator) = Base.SizeUnknown()
Base.eltype(::ExpressionIterator) = RuleNode

function Base.iterate(iter::ExpressionIterator)
    grammar, sym, max_depth = iter.grammar, iter.sym, iter.max_depth
    node = RuleNode(grammar[sym][1])
    if isterminal(grammar, node)
        return (deepcopy(node), node)
    else
        node, worked =  _next_state!(node, grammar, max_depth)
        while !worked
            # increment root's rule
            rules = grammar[sym]
            i = something(findfirst(isequal(node.ind), rules), 0)
            if i < length(rules)
                node, worked = RuleNode(rules[i+1]), true
                if !isterminal(grammar, node)
                    node, worked = _next_state!(node, grammar, max_depth)
                end
            else
                break
            end
        end
        return worked ? (deepcopy(node), node) : nothing
    end
end

function Base.iterate(iter::ExpressionIterator, state::RuleNode)
    grammar, max_depth = iter.grammar, iter.max_depth
    node, worked = _next_state!(state, grammar, max_depth)

    while !worked
        # increment root's rule
        rules = grammar[iter.sym]
        i = something(findfirst(isequal(node.ind), rules), 0)
        if i < length(rules)
            node, worked = RuleNode(rules[i+1]), true
            if !isterminal(grammar, node)
                node, worked = _next_state!(node, grammar, max_depth)
            end
        else
            break
        end
    end
    return worked ? (deepcopy(node), node) : nothing
end

"""
    count_expressions(grammar::Grammar, max_depth::Int, sym::Symbol)

Count the number of possible expressions of a grammar up to max_depth with start symbol sym.
"""
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

"""
    count_expressions(iter::ExpressionIterator)

Count the number of possible expressions in the expression iterator.
"""
count_expressions(iter::ExpressionIterator) = count_expressions(iter.grammar, iter.max_depth, iter.sym)

# Interface to AbstractTrees.jl
AbstractTrees.children(node::RuleNode) = node.children
AbstractTrees.printnode(io::IO, node::RuleNode) = print(io, node.ind)

"""
    mindepth_map(grammar::Grammar)

Returns the minimum depth achievable for each production rule, dmap.
"""
function mindepth_map(grammar::Grammar)
    dmap0 = Int[isterminal(grammar,i) ? 0 : typemax(Int)/2 for i in eachindex(grammar.rules)]
    dmap1 = fill(-1, length(grammar.rules)) 
    while dmap0 != dmap1
        for i in eachindex(grammar.rules)
            dmap1[i] = _mindepth(grammar, i, dmap0)
        end
        dmap1, dmap0 = dmap0, dmap1
    end
    dmap0
end
function _mindepth(grammar::Grammar, rule_index::Int, dmap::AbstractVector{Int})
    isterminal(grammar, rule_index) && return 0
    return 1 + maximum([mindepth(grammar, ctyp, dmap) for ctyp in child_types(grammar, rule_index)])
end
"""
    mindepth(grammar::Grammar, typ::Symbol, dmap::AbstractVector{Int})

Returns the minimum depth achievable for a given nonterminal symbol
"""
function mindepth(grammar::Grammar, typ::Symbol, dmap::AbstractVector{Int})
    return minimum(dmap[grammar.bytype[typ]])
end

"""
    Interpreter.SymbolTable(grammar::Grammar, mod::Module=Main)

Returns a symbol table populated with mapping from symbols in grammar to
symbols in module mod or Main, if defined.
"""
function Interpreter.SymbolTable(grammar::Grammar, mod::Module=Main)
    tab = SymbolTable()
    for rule in grammar.rules
        _add_to_symboltable!(tab, rule, mod)
    end
    tab
end
_add_to_symboltable!(tab::SymbolTable, rule::Any, mod::Module) = true
function _add_to_symboltable!(tab::SymbolTable, rule::Expr, mod::Module)
    if rule.head == :call && !iseval(rule)
        s = rule.args[1]
        if !_add_to_symboltable!(tab, s, mod)
            @warn "Unable to add function $s to symbol table"  
        end
        for s in rule.args[2:end]  #nested exprs
            _add_to_symboltable!(tab, s, mod)
        end
    end
    return true
end
function _add_to_symboltable!(tab::SymbolTable, s::Symbol, mod::Module)
    if isdefined(mod, s)
        tab[s] = getfield(mod, s)
        return true
    elseif isdefined(Base, s)
        tab[s] = getfield(Base, s)
        return true
    elseif isdefined(Main, s)
        tab[s] = getfield(Main, s)
        return true
    else
        return false
    end
end

end # module
