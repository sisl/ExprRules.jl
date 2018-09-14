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
    interpret(tab, ex, Val{ex.head})
end
function interpret(tab::SymbolTable, ex::Expr, ::Type{Val{:||}})
    interpret(tab, ex.args[1]) || interpret(tab, ex.args[2])
end
function interpret(tab::SymbolTable, ex::Expr, ::Type{Val{:&&}})
    interpret(tab, ex.args[1]) && interpret(tab, ex.args[2])
end
function interpret(tab::SymbolTable, ex::Expr, ::Type{Val{:call}})
    f = interpret(tab, ex.args[1])
    result = call_func(Val{length(ex.args)}, f, tab, ex.args)
    result
end
function interpret(tab::SymbolTable, ex::Expr, ::Type{Val{:(=)}})
    tab[ex.args[1]] = interpret(tab, ex.args[2]) #assignments done to symboltable
end
function interpret(tab::SymbolTable, ex::Expr, ::Type{Val{:block}})
    result = nothing
    for x in ex.args
        result = interpret(tab, x)
    end
    result
end
function interpret(tab::SymbolTable, ex::Expr, x)
    error("Expression type not supported")
end

#unroll for performance and avoid excessive allocations
function call_func(::Type{Val{2}}, f::Function, tab::SymbolTable, args)
    f(interpret(tab, args[2]))
end
function call_func(::Type{Val{3}}, f::Function, tab::SymbolTable, args)
    f(interpret(tab, args[2]),
        interpret(tab, args[3]))
end
function call_func(::Type{Val{4}}, f::Function, tab::SymbolTable, args)
    f(interpret(tab, args[2]),
        interpret(tab, args[3]),
        interpret(tab, args[4]))
end
function call_func(::Type{Val{5}}, f::Function, tab::SymbolTable, args)
    f(interpret(tab, args[2]),
        interpret(tab, args[3]),
        interpret(tab, args[4]),
        interpret(tab, args[5]))
end
function call_func(::Type{Val{6}}, f::Function, tab::SymbolTable, args)
    f(interpret(tab, args[2]),
        interpret(tab, args[3]),
        interpret(tab, args[4]),
        interpret(tab, args[5]),
        interpret(tab, args[6]))
end
function call_func(::Type{Val{7}}, f::Function, tab::SymbolTable, args)
    f(interpret(tab, args[2]),
        interpret(tab, args[3]),
        interpret(tab, args[4]),
        interpret(tab, args[5]),
        interpret(tab, args[6]),
        interpret(tab, args[7]))
end
function call_func(::Any, f::Function, tab::SymbolTable, args)
    args = [interpret(tab, args[i]) for i = 2:lastindex(args)]
    f(args...)
end

end #module
