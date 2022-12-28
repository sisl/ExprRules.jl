using ExprRules
using Test, Random

#=
TESTS NEED TO BE MORE THOROUGHLY DEFINED
=#

let
    grammar = @grammar begin
        R = I
        I = 1
    end

    @test nonterminals(grammar) == [:R,:I]
    @test return_type(grammar, 1) == :R
    @test return_type(grammar, 2) == :I
    @test child_types(grammar, 1) == [:I]
    @test child_types(grammar, 2) == Symbol[]
    @test isterminal(grammar, 1) == false
    @test isterminal(grammar, 2) == true
    @test nchildren(grammar, 1) == 1
    @test nchildren(grammar, 2) == 0
    @test max_arity(grammar) == 1

    rulenode = RuleNode(1, [RuleNode(2)])
    get_executable(rulenode, grammar)
    @test grammar[:R] == grammar.bytype[:R]
    @test grammar[:I] == grammar.bytype[:I]
    @test contains_returntype(rulenode, grammar, :R)
    @test contains_returntype(rulenode, grammar, :I)
    @test !contains_returntype(rulenode, grammar, :B)

    io = IOBuffer()
    show(io, rulenode)
    @test String(take!(io)) == "1{2}"

    display(rulenode, grammar)
end

let
    grammar = @grammar begin
        R = A
        A = B
        B = 1
    end

    rulenode = rand(RuleNode, grammar, :R)
    @test contains_returntype(rulenode, grammar, :R, 1)
    @test contains_returntype(rulenode, grammar, :R, 2)
    @test contains_returntype(rulenode, grammar, :R, 3)
    @test contains_returntype(rulenode, grammar, :A, 1) == false
    @test contains_returntype(rulenode, grammar, :A, 2)
    @test contains_returntype(rulenode, grammar, :A, 3)
    @test contains_returntype(rulenode, grammar, :B, 1) == false
    @test contains_returntype(rulenode, grammar, :B, 2) == false
    @test contains_returntype(rulenode, grammar, :B, 3)

    sample(rulenode, :R, grammar, 1)
    sample(rulenode, :R, grammar, 2)
    sample(rulenode, :R, grammar, 3)
    sample(rulenode, :A, grammar, 2)
    sample(rulenode, :A, grammar, 3)
    sample(rulenode, :B, grammar, 3)

    completed = false
    try
        sample(rulenode, :B, grammar, 2)
        completed = true
    catch
        completed = false
    end
    @test !completed #should throw an error for not found

    @test length(unique([sample(rulenode, 1) for i=1:10])) == 1
    @test length(unique([sample(rulenode, 2) for i=1:10])) <= 2
    @test length(unique([sample(rulenode, 3) for i=1:10])) <= 3

    @test length(unique([sample(NodeLoc, rulenode, 1) for i=1:10])) == 1
    @test length(unique([sample(NodeLoc, rulenode, 2) for i=1:10])) <= 2
    @test length(unique([sample(NodeLoc, rulenode, 3) for i=1:10])) <= 3

    @test sample(NodeLoc, rulenode, :R, grammar, 1) == NodeLoc(rulenode, 0)
    @test sample(NodeLoc, rulenode, :R, grammar, 2) == NodeLoc(rulenode, 0)
    @test sample(NodeLoc, rulenode, :R, grammar, 3) == NodeLoc(rulenode, 0)
    @test sample(NodeLoc, rulenode, :A, grammar, 2) == NodeLoc(rulenode, 1)
    @test sample(NodeLoc, rulenode, :A, grammar, 3) == NodeLoc(rulenode, 1)

    completed = false
    try
        sample(NodeLoc, rulenode, :A, grammar, 1)
        completed = true
    catch
        completed = false
    end
    @test !completed #should throw an error for not found
end

let
    grammar = @grammar begin
        R = R + R
        R = 1
    end

    root = RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]),
                            RuleNode(1, [RuleNode(2), RuleNode(2)])])

    @test node_depth(root, root) == 1
    @test node_depth(root, root.children[1]) == 2
    @test node_depth(root, root.children[2]) == 2
    @test node_depth(root, root.children[1].children[1]) == 3
    @test node_depth(root, root.children[1].children[2]) == 3
    @test node_depth(root, root.children[2].children[1]) == 3
    @test node_depth(root, root.children[2].children[2]) == 3
end

let
    grammar = @grammar begin
        Real = x
        Real = Real * Real
        Real = f(Real)
        Real = _(rand(1.0:5.0))
        Real = A | B | g(Real)
        Real = 1 | 2 | 3
        Real = |(4:6)
        Real = |([7,8,9])
    end

    @test iseval(grammar, 1) == false
    @test iseval(grammar, 2) == false
    @test iseval(grammar, 3) == false
    @test iseval(grammar, 4) == true
    @test iseval(grammar) == true
end

x = 3
let
    Random.seed!(0)

    grammar = @grammar begin
        Real = rand()
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

    @test max_arity(grammar) == 3
    @test grammar.childtypes == [
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
    @test return_type(grammar, rulenode) == :Real
    @test child_types(grammar, rulenode) == [:Real,:Real]
    @test isterminal(grammar, rulenode) == false
    @test nchildren(grammar, rulenode) == 2
    @test return_type(grammar, rulenode.children[1]) == :Real
    @test child_types(grammar, rulenode.children[1]) == Symbol[]
    @test isterminal(grammar, rulenode.children[1]) == true
    @test nchildren(grammar, rulenode.children[1]) == 0
    @test  isequal(rulenode, RuleNode(3, [RuleNode(2), RuleNode(1)]))
    @test !isequal(rulenode, RuleNode(1))
    @test !isequal(rulenode, RuleNode(3, [RuleNode(1), RuleNode(2)]))
    @test string(get_executable(rulenode, grammar)) == string(:(x + rand()))

    @test contains_returntype(rulenode, grammar, :Real)
    @test !contains_returntype(rulenode, grammar, :Bool)

    rulenode = RuleNode(12, [RuleNode(3, [RuleNode(2), RuleNode(4)])])
    @test length(rulenode) == 4
    @test depth(rulenode) == 3
    @test string(get_executable(rulenode, grammar)) == string(:(2 / (1 + (x + 3.0))))

    rulenode = RuleNode(1, "potato")

    rulenode = rand(RuleNode, grammar, :Real, 3)
    hash(rulenode)

    Core.eval(rulenode, grammar)

    ex = get_executable(rulenode, grammar)
    eval(ex)
    S = SymbolTable(grammar, Main)
    interpret(S, ex) 
    Core.eval(S, ex)

    Random.seed!(0)
    sample(rulenode)

    Random.seed!(1)
    sample(rulenode, :Real, grammar)

    Random.seed!(2)
    loc = sample(NodeLoc, rulenode)
    get(rulenode, loc)

    Random.seed!(3)
    insert!(rulenode, loc, rand(RuleNode, grammar, :Real, 3))

    node = RuleNode(1, [RuleNode(2), RuleNode(2)])
    child_loc = NodeLoc(node, 1)
    new = RuleNode(3)
    inserted = insert!(node, child_loc, new)
    @test node == inserted

    node = RuleNode(1, [RuleNode(2), RuleNode(2)])
    root_loc = NodeLoc(node, 0)
    new = RuleNode(3)
    inserted =  insert!(node, root_loc, new)
    @test node == inserted

    Random.seed!(4)
    rulenode = RuleNode(3, [RuleNode(4), RuleNode(5)])
    for i in 1 : 10
        loc = sample(NodeLoc, rulenode, :Real, grammar)
    end
end

let
    grammar = @grammar begin
        R = R + R
        R = 1
        R = 2
    end

    node = RuleNode(1)
    node, worked = ExprRules._next_state!(node, grammar, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(2), RuleNode(2)]))

    node = RuleNode(1, [RuleNode(2), RuleNode(2)])
    node, worked = ExprRules._next_state!(node, grammar, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(2), RuleNode(3)]))

    node = RuleNode(1, [RuleNode(2), RuleNode(3)])
    node, worked = ExprRules._next_state!(node, grammar, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(3), RuleNode(2)]))

    node = RuleNode(1, [RuleNode(3), RuleNode(2)])
    node, worked = ExprRules._next_state!(node, grammar, 2)
    @test worked
    @test isequal(node, RuleNode(1, [RuleNode(3), RuleNode(3)]))

    node = RuleNode(1, [RuleNode(3), RuleNode(3)])
    node, worked = ExprRules._next_state!(node, grammar, 2)
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
        node, worked = ExprRules._next_state!(node, grammar, 3)
        @test worked
        @test isequal(node, testnode)
    end

    ###
    iter = ExpressionIterator(grammar, 1, :R)
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(2), RuleNode(3)]))
    @test count_expressions(grammar, 1, :R) == 2


    iter = ExpressionIterator(grammar, 2, :R)
    x0 = iterate(iter)
    @test x0 != nothing
    item, state = x0
    @test isequal(item, RuleNode(1, [RuleNode(2), RuleNode(2)]))
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(1, [RuleNode(2), RuleNode(2)]),
        RuleNode(1, [RuleNode(2), RuleNode(3)]),
        RuleNode(1, [RuleNode(3), RuleNode(2)]),
        RuleNode(1, [RuleNode(3), RuleNode(3)]),
        RuleNode(2),
        RuleNode(3),
    ]))
    @test count_expressions(grammar, 2, :R) == 6
end

let
    grammar = @grammar begin
        R = I | F
        I = 1 | 2
        F = F + F
        F = 1.5
    end

    iter = ExpressionIterator(grammar, 2, :R)
    @test all(a == b for (a,b) in zip(collect(iter), [
        RuleNode(1, [RuleNode(3)]),
        RuleNode(1, [RuleNode(4)]),
        RuleNode(2, [RuleNode(6)]),
    ]))

    iter = ExpressionIterator(grammar, 3, :R)
    @test all(a == b for (a,b) in zip(collect(iter), [
        RuleNode(1, [RuleNode(3)]),
        RuleNode(1, [RuleNode(4)]),
        RuleNode(2, [RuleNode(5, [RuleNode(6), RuleNode(6)])]),
        RuleNode(2, [RuleNode(6)]),
    ]))

    @test count_expressions(grammar, 2, :R) == 3
    @test count_expressions(grammar, 3, :R) == 4
    @test first(iter) == RuleNode(1, [RuleNode(3)])
end

let
    grammar = @grammar begin
        R = 1
        R = 2
        R = R + R
    end

    iter = ExpressionIterator(grammar, 2, :R)
    @test all(isequal(a,b) for (a,b) in zip(collect(iter), [
        RuleNode(1),
        RuleNode(2),
        RuleNode(3, [RuleNode(1), RuleNode(1)]),
        RuleNode(3, [RuleNode(1), RuleNode(2)]),
        RuleNode(3, [RuleNode(2), RuleNode(1)]),
        RuleNode(3, [RuleNode(2), RuleNode(2)]),
    ]))

    @test count_expressions(grammar, 2, :R) == 6
    @test count_expressions(iter) == 6
    @test first(iter) == RuleNode(1)
end

let
    grammar = @grammar begin
        x = x + y
        x = y + 1
        y = x + y
        y = 1
    end

    dmap = mindepth_map(grammar)
    @test all(dmap .== [2, 1, 2, 0])
    @test mindepth(grammar, :x, dmap) == 1
    @test mindepth(grammar, :y, dmap) == 0

    node = rand(RuleNode, grammar, :y, dmap, 0)
    @test node == RuleNode(4)
    node = rand(RuleNode, grammar, :x, dmap, 1)
    @test node == RuleNode(2, [RuleNode(4)])
end

let
    grammar = @grammar begin
        b = f(x) < n
        x = true
        n = 15
    end
    node = RuleNode(1, [RuleNode(2),RuleNode(3)])
    ex = get_executable(node, grammar)
    @test ex == :(f(true) < 15)
end

let
    grammar = @grammar begin
        R = R + R
        R = 1
    end

    root = RuleNode(1, [RuleNode(1, [RuleNode(2), RuleNode(2)]),
                            RuleNode(1, [RuleNode(2), RuleNode(2)])])

    bin = NodeRecycler(20)
    recycle!(bin, root)

    RuleNode(bin, 1)
    RuleNode(bin, 1, true)
    RuleNode(bin, 1, [RuleNode(1)])
    RuleNode(bin, 1, true, [RuleNode(1)]) 
end

let
    grammar1 = @grammar begin
        x = x | y | a 
        y = a | b 
    end
    grammar2 = @grammar begin
        y = y | z
        z = a | b | c
    end

    grammar = deepcopy(grammar1)
    append!(grammar, grammar2)
end

let
    f(x1,x2,x3,x4,x5,x6,x7,x8)=x1+x2+x3+x4+x5+x6+x7+x8
    ex = :(f(1,2,3,4,5,6,7,8))
    S = SymbolTable(:f => f)
    interpret(S, ex) 
    Core.eval(S, ex)
end

let 
    grammar = @grammar begin
        Vector = [1, 2]
        Vector = exp.(Vector)
    end
    tab = SymbolTable(grammar)
    tree = RuleNode(2, [RuleNode(1)])
    ex = get_executable(tree, grammar)
    interpret(tab, ex) == exp.([1, 2])
end

let
    grammar = @grammar begin
        A = B
        B = C
        C = D
        D = E
        E = 1
    end
    rulenode = rand(RuleNode, grammar, :A, 1)
end

let
    S = SymbolTable(:x => [1,2,3], :y => [0,2,4], :b1 => [1,1,1], :b2=>[1,0,1])
    @test all(interpret(S, Meta.parse("x .<= y")) .== [false, true, true])
    @test all(interpret(S, Meta.parse("x .>= y")) .== [true, true, false])
    @test all(interpret(S, Meta.parse("x .== y")) .== [false, true, false])
    @test all(interpret(S, Meta.parse("b1 .& b2")) .== [true, false, true])
    @test all(interpret(S, Meta.parse("b1 .| b2")) .== [true, true, true])
end

let
    grammar = @grammar begin
        all = mat[:, :]
        col1 = mat[1, :]
        row2 = mat[:, 2]
        slice_34 = mat[3, 4]
        slice_2r = mat[2:r, :]
        slice_2c = mat[:, 2:c]
        slice_r2c = mat[r, 2:c]
        slice_2r2c = mat[2:r, 2:c]
        slice_list_23 = mat[[2, 3], :]
        slice_list_rc = mat[:, [r, c]]
        slice_list_23rc = mat[[2, 3], [r, c]]
        slice_list_rrc = mat[r, [r, c]]
        slice_list_comb = mat[[r, c, 20], 2:4]
    end
    mat = reshape(1:100, (20, 5))
    r, c = 5, 3

    S = SymbolTable(grammar)
    S[:mat] = mat
    S[:r] = r
    S[:c] = c
    rn = Dict(
        sym=>rand(RuleNode, grammar, sym) for sym in nonterminals(grammar)
    )
    @test Core.eval(S, get_executable(rn[:all], grammar)) == mat[:, :]
    @test Core.eval(S, get_executable(rn[:col1], grammar)) == mat[1, :]
    @test Core.eval(S, get_executable(rn[:row2], grammar)) == mat[:, 2]
    @test Core.eval(S, get_executable(rn[:slice_34], grammar)) == mat[3, 4]
    @test Core.eval(S, get_executable(rn[:slice_2r], grammar)) == mat[2:r, :]
    @test Core.eval(S, get_executable(rn[:slice_2c], grammar)) == mat[:, 2:c]
    @test Core.eval(S, get_executable(rn[:slice_r2c], grammar)) == mat[r, 2:c]
    @test Core.eval(S, get_executable(rn[:slice_2r2c], grammar)) == mat[2:r, 2:c]
    @test Core.eval(S, get_executable(rn[:slice_list_23], grammar)) == mat[[2, 3], :]
    @test Core.eval(S, get_executable(rn[:slice_list_rc], grammar)) == mat[:, [r, c]]
    @test Core.eval(S, get_executable(rn[:slice_list_23rc], grammar)) == mat[[2, 3], [r, c]]
    @test Core.eval(S, get_executable(rn[:slice_list_rrc], grammar)) == mat[r, [r, c]]
    @test Core.eval(S, get_executable(rn[:slice_list_comb], grammar)) -= mat[[r, c, 20], 2:4]
end
