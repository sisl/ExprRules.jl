"""
Evaluates an expression without compiling it.
Uses AST and symbol lookups. Only supports :call and :(=) 
expressions at the moment.
Example:
tab = SymbolTable(:f => f, :x => x)
ex = :(f(x))
interpret(tab, ex)
"""
module Interpreter

export SymbolTable, interpret

const SymbolTable = Dict{Symbol,Any}

interpret(tab::SymbolTable, x::Any) = x
interpret(tab::SymbolTable, s::Symbol) = haskey(tab,s) ? tab[s] : getfield(Main, s)

function interpret(tab::SymbolTable, ex::Expr)
    result = if ex.head == :call
        f = interpret(tab, ex.args[1])
        call_func(tab, ex.args...)
    elseif ex.head == :||
        interpret(tab, ex.args[1]) || interpret(tab, ex.args[2])
    elseif ex.head == :&&
        interpret(tab, ex.args[1]) && interpret(tab, ex.args[2])
    elseif ex.head == :(=)
        tab[ex.args[1]] = interpret(tab, ex.args[2]) #assignments done to symboltable
    elseif ex.head == :block
        result = nothing
        for x in ex.args
            result = interpret(tab, x)
        end
        result
    else
        error("Expression type not supported")
    end
    result
end

#unroll for performance and avoid excessive allocations
function call_func(tab::SymbolTable, f)
    func = interpret(tab,f)
    func()
end
function call_func(tab::SymbolTable, f, x1)
    func = interpret(tab,f)
    func(interpret(tab,x1))
end
function call_func(tab::SymbolTable, f, x1, x2)
    func = interpret(tab,f)
    func(interpret(tab, x1),
        interpret(tab, x2))
end
function call_func(tab::SymbolTable, f, x1, x2, x3)
    func = interpret(tab,f)
    func(interpret(tab, x1),
        interpret(tab, x2),
       interpret(tab, x3))
end
function call_func(tab::SymbolTable, f, x1, x2, x3, x4)
    func = interpret(tab,f)
    func(interpret(tab, x1),
        interpret(tab, x2),
       interpret(tab, x3),
       interpret(tab, x4))
end
function call_func(tab::SymbolTable, f, x1, x2, x3, x4, x5)
    func = interpret(tab,f)
    func(interpret(tab, x1),
        interpret(tab, x2),
       interpret(tab, x3),
       interpret(tab, x4),
       interpret(tab, x5))
end

end #module
