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
interpret(tab::SymbolTable, s::Symbol) = tab[s] 

function interpret(tab::SymbolTable, ex::Expr)
    args = ex.args
    if ex.head == :call
        len = length(args)
        #unroll for performance and avoid excessive allocations
        if len == 1
            return tab[args[1]]()
        elseif len == 2
            return tab[args[1]](interpret(tab,args[2]))
        elseif len == 3
            return tab[args[1]](interpret(tab,args[2]), interpret(tab,args[3]))
        elseif len == 4
            return tab[args[1]](interpret(tab,args[2]), interpret(tab,args[3]), interpret(tab,args[4]))
        elseif len == 5
            return tab[args[1]](interpret(tab,args[2]), interpret(tab,args[3]), interpret(tab,args[4]),
                                   interpret(tab,args[5]))
        elseif len == 6
            return tab[args[1]](interpret(tab,args[2]), interpret(tab,args[3]), interpret(tab,args[4]),
                                   interpret(tab,args[5]), interpret(tab,args[6]))
        else
            error("Interpreter supports up to 5 function arguments only") 
        end
    elseif ex.head == :||
        return (interpret(tab, args[1]) || interpret(tab, args[2]))
    elseif head == :&&
        return (interpret(tab, args[1]) && interpret(tab, args[2]))
    elseif ex.head == :(=)
        return (tab[args[1]] = interpret(tab, args[2])) #assignments made to symboltable
    elseif ex.head == :block
        result = nothing
        for x in args
            result = interpret(tab, x)
        end
        return result
    else
        error("Expression type not supported")
    end
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
