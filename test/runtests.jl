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
    @test ruleset[:R] == ruleset.bytype[:R]
    @test ruleset[:I] == ruleset.bytype[:I]
    @test contains_returntype(rulenode, ruleset, :R)
    @test contains_returntype(rulenode, ruleset, :I)
    @test !contains_returntype(rulenode, ruleset, :B)

    io = IOBuffer()
    show(io, rulenode)
    @test String(take!(io)) == "1{2}"

    display(rulenode, ruleset)
end

let
    ruleset = @ruleset begin
        Real = x
        Real = Real * Real
        Real = f(Real)
        Real = _(Base.rand(1.0:5.0))
        Real = A | B | g(Real)
        Real = 1 | 2 | 3
        Real = |(4:6)
        Real = |([7,8,9])
    end
end

x = 3
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

    @test contains_returntype(rulenode, ruleset, :Real)
    @test !contains_returntype(rulenode, ruleset, :Bool)

    rulenode = RuleNode(12, [RuleNode(3, [RuleNode(2), RuleNode(4)])])
    @test length(rulenode) == 4
    @test depth(rulenode) == 3
    @test string(get_executable(rulenode, ruleset)) == string(:(2 / (1 + (x + 3.0))))

    rulenode = RuleNode(1, "potato")

    rulenode = rand(RuleNode, ruleset, :Real, 3)
    hash(rulenode)

    eval(rulenode, ruleset)

    srand(0)
    sample(rulenode)

    srand(1)
    sample(rulenode, :Real, ruleset)

    srand(2)
    loc = sample(NodeLoc, rulenode)
    get(rulenode, loc)

    srand(3)
    insert!(rulenode, loc, rand(RuleNode, ruleset, :Real, 3))

    srand(4)
    rulenode = RuleNode(3, [RuleNode(4), RuleNode(5)])
    for i in 1 : 10
        loc = sample(NodeLoc, rulenode, :Real, ruleset)
    end
end

let
    ruleset = @ruleset begin
        R = R + R
        R = 1
        R = 2
    end

    node = RuleNode(1)
    node, worked = ExprRules._next_state!(node, ruleset, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(2), RuleNode(2)]))

    node = RuleNode(1, [RuleNode(2), RuleNode(2)])
    node, worked = ExprRules._next_state!(node, ruleset, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(2), RuleNode(3)]))

    node = RuleNode(1, [RuleNode(2), RuleNode(3)])
    node, worked = ExprRules._next_state!(node, ruleset, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(3), RuleNode(2)]))

    node = RuleNode(1, [RuleNode(3), RuleNode(2)])
    node, worked = ExprRules._next_state!(node, ruleset, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(3), RuleNode(3)]))

    node = RuleNode(1, [RuleNode(3), RuleNode(3)])
    node, worked = ExprRules._next_state!(node, ruleset, 2)
    @test !worked

    ###

    node = RuleNode(1)
    for testnode in [
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]), RuleNode(1, [RuleNode(2), RuleNode(2)])]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]), RuleNode(1, [RuleNode(2), RuleNode(3)])]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]), RuleNode(1, [RuleNode(3), RuleNode(2)])]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]), RuleNode(1, [RuleNode(3), RuleNode(3)])]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]), RuleNode(2)]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]), RuleNode(3)]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(3)]), RuleNode(1, [RuleNode(2), RuleNode(2)])]),
            RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(3)]), RuleNode(1, [RuleNode(2), RuleNode(3)])]),
        ]
        node, worked = ExprRules._next_state!(node, ruleset, 3)
        @test worked
        @test isequal(node, testnode)
    end

    ###

    iter = ExpressionIterator(ruleset, 2, :R)
    state = start(iter)
    @test !done(iter, state)
    @test isequal(first(iter), RuleNode(1, [RuleNode(2), RuleNode(2)]))
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(1, [RuleNode(2), RuleNode(2)]),
        RuleNode(1, [RuleNode(2), RuleNode(3)]),
        RuleNode(1, [RuleNode(3), RuleNode(2)]),
        RuleNode(1, [RuleNode(3), RuleNode(3)]),
        RuleNode(2),
        RuleNode(3),
    ]))
    @test count_expressions(ruleset, 2, :R) == 6
end

let
    ruleset = @ruleset begin
        R = I | F
        I = 1 | 2
        F = F + F
        F = 1.5
    end

    iter = ExpressionIterator(ruleset, 2, :R)
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(1, [RuleNode(3)]),
        RuleNode(1, [RuleNode(4)]),
        RuleNode(2, [RuleNode(6)]),
    ]))

    iter = ExpressionIterator(ruleset, 3, :R)
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(1, [RuleNode(3)]),
        RuleNode(1, [RuleNode(4)]),
        RuleNode(2, [RuleNode(5, [RuleNode(6), RuleNode(6)])]),
        RuleNode(2, [RuleNode(6)]),
    ]))

    @test count_expressions(ruleset, 2, :R) == 3
    @test count_expressions(ruleset, 3, :R) == 4
end

let
    ruleset = @ruleset begin
        R = 1
        R = 2
        R = R + R
    end

    iter = ExpressionIterator(ruleset, 2, :R)
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(1),
        RuleNode(2),
        RuleNode(3, [RuleNode(1), RuleNode(1)]),
        RuleNode(3, [RuleNode(1), RuleNode(2)]),
        RuleNode(3, [RuleNode(2), RuleNode(1)]),
        RuleNode(3, [RuleNode(2), RuleNode(2)]),
    ]))

    @test count_expressions(ruleset, 2, :R) == 6
    @test count_expressions(iter) == 6
end