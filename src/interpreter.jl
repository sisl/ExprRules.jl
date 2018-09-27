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
interpret(tab::SymbolTable, s::Symbol) = haskey(tab,s) ? tab[s] : getproperty(Main, s)

function interpret(tab::SymbolTable, ex::Expr)
    result = if ex.head == :call
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

### Raw interpret, no symbol table
function interpret(ex::Expr, M::Module=Main)
    result = if ex.head == :call
        call_func(M, ex.args...)
    elseif ex.head == :vect
        ex.args
    else
        Core.eval(M, ex)
    end
end
call_func(M::Module, f::Symbol) = getproperty(M,f)()
call_func(M::Module, f::Symbol, x1) = getproperty(M,f)f(x1)
call_func(M::Module, f::Symbol, x1, x2) = getproperty(M,f)(x1, x2)
call_func(M::Module, f::Symbol, x1, x2, x3) = getproperty(M,f)(x1, x2, x3)
call_func(M::Module, f::Symbol, x1, x2, x3, x4) = getproperty(M,f)(x1, x2, x3, x4)
call_func(M::Module, f::Symbol, x1, x2, x3, x4, x5) = getproperty(M,f)(x1, x2, x3, x4, x5)

end #module
