using ExprRules, Statistics
using BenchmarkTools
using Random

protectedDiv(x, y) = iszero(y) ? 1.0 : x / y

const grammar = @grammar begin
    R = x
    R = R + R
    R = R - R
    R = R * R
    R = protectedDiv(R, R)
    R = -R
    R = cos(R)
    R = sin(R)
    R = |(1.0:5.0) 
end
const S = SymbolTable(grammar, Main)

gt(x) = x^4 + x^3 + x^2 + x
function loss1(tree::RuleNode, grammar::Grammar)
    ex = get_executable(tree, grammar)
    los = 0.0
    global x
    for y = -1.0:0.1:1.0
        x = y
        los += abs2(eval(ex) - gt(x))
    end
    mean(los)
end
function loss2(tree::RuleNode, grammar::Grammar)
    ex = get_executable(tree, grammar)
    los = 0.0
    for y = -1.0:0.1:1.0
        S[:x] = y
        los += abs2(Core.eval(S, ex) - gt(y))
    end
    mean(los)
end

function run_benchmark(seed::Int=1)
    Random.seed!(seed)
    tree = rand(RuleNode, grammar, :R, 10)
    ex = get_executable(tree, grammar)
    @show ex
    @btime loss1($tree, $grammar) 
    @btime loss2($tree, $grammar) 
end
