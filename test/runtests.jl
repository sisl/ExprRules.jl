using ExprRules
using Base.Test

#=
TESTS NEED TO BE MORE THOROUGHLY DEFINED
=#

let
    ruleset = @ruleset begin
        R = I
        I = 1
    end

    @test nonterminals(ruleset) == [:R,:I]
    @test return_type(ruleset, 1) == :R
    @test return_type(ruleset, 2) == :I
    @test child_types(ruleset, 1) == [:I]
    @test child_types(ruleset, 2) == Symbol[]
    @test isterminal(ruleset, 1) == false
    @test isterminal(ruleset, 2) == true
    @test nchildren(ruleset, 1) == 1
    @test nchildren(ruleset, 2) == 0
    @test max_arity(ruleset) == 1

    rulenode = RuleNode(1, [RuleNode(2)])
    get_executable(rulenode, ruleset)
end

let
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

    @test max_arity(ruleset) == 3
    @test ruleset.childtypes == [
        Symbol[],
        Symbol[],
        [:Real, :Real],
        Symbol[],
        Symbol[],
        [:Real],
        [:Real, :Real],
        [:Real, :Real],
        Symbol[],
        [:Bool, :Real, :Real],
        Symbol[],
        [:Real],
    ]


    rulenode = RuleNode(3, [RuleNode(2), RuleNode(1)])
    @test length(rulenode) == 3
    @test depth(rulenode) == 2
    @test return_type(ruleset, rulenode) == :Real
    @test child_types(ruleset, rulenode) == [:Real,:Real]
    @test isterminal(ruleset, rulenode) == false
    @test nchildren(ruleset, rulenode) == 2
    @test return_type(ruleset, rulenode.children[1]) == :Real
    @test child_types(ruleset, rulenode.children[1]) == Symbol[]
    @test isterminal(ruleset, rulenode.children[1]) == true
    @test nchildren(ruleset, rulenode.children[1]) == 0
    @test  isequal(rulenode, RuleNode(3, [RuleNode(2), RuleNode(1)]))
    @test !isequal(rulenode, RuleNode(1))
    @test !isequal(rulenode, RuleNode(3, [RuleNode(1), RuleNode(2)]))
    @test string(get_executable(rulenode, ruleset)) == string(:(x + Base.rand()))

    rulenode = RuleNode(12, [RuleNode(3, [RuleNode(2), RuleNode(4)])])
    @test length(rulenode) == 4
    @test depth(rulenode) == 3
    @test string(get_executable(rulenode, ruleset)) == string(:(2 / (1 + (x + 3.0))))

    rulenode = RuleNode(1, "potato")

    rulenode = rand(RuleNode, ruleset, :Real, 3)
    hash(rulenode)

    x = 3
    eval(rulenode, ruleset)

    sample(rulenode)
    sample(rulenode, :Real, ruleset)
    loc = sample(NodeLoc, rulenode)
    get(rulenode, loc)
    insert!(rulenode, loc, rand(RuleNode, ruleset, :Real, 3))
end
