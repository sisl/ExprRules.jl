
"""
Use a circular buffer to contain used RuleNodes to be recycled.
First check the recycle bin to see if there are available RuleNodes, if not allocated a new one.  This helps minimize memory allocations and improves performance
"""
const NodeRecycler = CircularBuffer{RuleNode}

RuleNode(bin::Nothing, ind::Int) = RuleNode(ind)
RuleNode(bin::Nothing, ind::Int, children::Vector{RuleNode}) = RuleNode(ind, children)
RuleNode(bin::Nothing, ind::Int, _val::Any) = RuleNode(ind, _val)
RuleNode(bin::Nothing, ind::Int, _val::Any, children::Vector{RuleNode}) = RuleNode(ind, _val, children)
function RuleNode(bin::NodeRecycler, ind::Int)
    isempty(bin) && return RuleNode(ind)
    node = pop!(bin)
    node.ind, node._val = ind, nothing
    empty!(node.children)
    return node
end
function RuleNode(bin::NodeRecycler, ind::Int, children::Vector{RuleNode})
    isempty(bin) && return RuleNode(ind, children)
    node = pop!(bin)
    node.ind, node._val, node.children = ind, nothing, children
    return node
end
function RuleNode(bin::NodeRecycler, ind::Int, _val::Any)
    isempty(bin) && return RuleNode(ind, _val)
    node = pop!(bin)
    node.ind, node._val = ind, _val 
    empty!(node.children)
    return node
end
function RuleNode(bin::NodeRecycler, ind::Int, _val::Any, children::Vector{RuleNode})
    isempty(bin) && return RuleNode(ind, _val, children)
    node = pop!(bin)
    node.ind, node._val, node.children = ind, _val, children
    return node
end

function recycle!(bin::NodeRecycler, root::RuleNode)
    push!(bin, root)
    for child in root.children
        recycle!(bin, child)
    end
end
