using ExprRules
using Base.Test

#=
TESTS NEED TO BE MORE THOROUGHLY DEFINED
=#

srand(0)

ruleset = @ruleset begin
    Real = Base.rand()
    Real = x
    Real = Real + Real
    Real = 3.0
    Real = 1.0
    Real = sqrt(Real)
    Bool = Real > Real
    Bool = Real < Real
    Bool = true
    Real = Bool ? Real : Real
    Real = 2 / (1 + x)
    Real = 2 / (1 + Real)
end

rulenode = RuleNode(3, [RuleNode(2), RuleNode(1)])
@test length(rulenode) == 3
@test depth(rulenode) == 2
@test string(get_executable(rulenode, ruleset)) == string(:(x + Base.rand()))


rulenode = RuleNode(12, [RuleNode(3, [RuleNode(2), RuleNode(4)])])
@test length(rulenode) == 4
@test depth(rulenode) == 3
@test string(get_executable(rulenode, ruleset)) == string(:(2 / (1 + (x + 3.0))))

rulenode = rand(RuleNode, ruleset, :Real, 3)
hash(rulenode)

x = 3
eval(rulenode, ruleset)

sample(rulenode)
sample(rulenode, :Real, ruleset)
loc = sample(NodeLoc, rulenode)
get(rulenode, loc)
insert!(rulenode, loc, rand(RuleNode, ruleset, :Real, 3))
