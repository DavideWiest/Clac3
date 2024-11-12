module Clac3.BuiltIn

open Clac3.Domain

open Clac3.Representation

// TODO: How do you define these rules as an object, not a condition of a match statement? + rule validation (+ rule tree)
// TODO: rule order? 1) by probability of matching 2) by 
// NOTE: rules should be defined so that all the defined values come first - like in the if-then-else rule
//  the expressions are evaluated depth first from left to right
//  in boolean algebra, it would be a lot more efficient to compute the simpler expressions first
// NOTE: results should not contian References, and strings as References should not be passed to SomePrimLeaf/toprimitive, they will be treated as Strings
// NOTE: rules of dynamic length would be useful too (for arrays, syntax simplification)
// NOTE: rules should be maximally nested to speed up matching
// NOTE: at some point, the expression has to be evaluated. recursive rules should not have a expression-catching block, as they need it evaluated, or it will lead to infinite recursion

let coreRuleSet = {
    pattern = matchNode (function

    // DENESTING
    
    | [Leaf x] -> SLeaf x
    | [Node(children)] -> SNode children
    
    // CONTROL FLOW
    
    | [Leaf(Keyword "if"); Leaf(Bool b); Leaf(Keyword "then"); thenExpr; Leaf(Keyword "else"); elseExpr] -> Some (if b then thenExpr else elseExpr)
    | [x; Leaf(Keyword "|>"); Node(ys)] -> Some (Node (ys@[x]))

    // LOGIC
    
    // = could alternatively try to match both sides beofre they are evaluated, but that would be performance net negative in all practical cases
    | [Leaf x; Leaf(Keyword "="); Leaf y] when isDefinedLeaf x && isDefinedLeaf y -> BoolLeaf (x = y)

    //  REFLECTION

    | [Leaf(Keyword "typeof"); Leaf x] -> Some (Leaf (String (x.GetType().Name)))
    | [Leaf(Keyword "stringify"); x] -> StringLeaf (ToString.expression x)
    
    // ARITHMETIC OPERATORS
    
    | [Leaf x; Leaf(Keyword "+" ); Leaf y] -> 
        match x,y with
        | Integer a, Integer b -> IntLeaf (a+b)
        | Float a, Float b -> FloatLeaf (a+b)
        | Integer a, Float b -> FloatLeaf (float a + b)
        | Float a, Integer b -> FloatLeaf (a + float b)

        | String a, String b -> StringLeaf (a+b)

        | List a, List b -> ListLeaf (a@b)

        | _ -> None

    | [Leaf x; Leaf(Keyword "-" ); Leaf y] -> 
        match x,y with
        | Integer a, Integer b -> IntLeaf (a-b)
        | Float a, Float b -> FloatLeaf (a-b)
        | Integer a, Float b -> FloatLeaf (float a - b)
        | Float a, Integer b -> FloatLeaf (a - float b)

        | _ -> None

    | [Leaf x; Leaf(Keyword "*" ); Leaf y] -> 
        match x,y with
        | Integer a, Integer b -> IntLeaf (a*b)
        | Float a, Float b -> FloatLeaf (a*b)
        | Integer a, Float b -> FloatLeaf (float a * b)
        | Float a, Integer b -> FloatLeaf (a * float b)

        | _ -> None

    | [Leaf x; Leaf(Keyword "/" ); Leaf y] -> 
        match x,y with
        | Integer a, Integer b -> IntLeaf (a/b)
        | Float a, Float b -> FloatLeaf (a/b)
        | Integer a, Float b -> FloatLeaf (float a / b)
        | Float a, Integer b -> FloatLeaf (a / float b)

        | _ -> None

    | [Leaf x; Leaf(Keyword "**" ); Leaf y] -> 
        match x,y with
        | Integer a, Integer b -> IntLeaf (pown a b)
        | Float a, Float b -> FloatLeaf (a ** b)
        | Integer a, Float b -> FloatLeaf (float a ** b)
        | Float a, Integer b -> FloatLeaf (a ** float b)

        | _ -> None

    // LISTS

    | [Leaf x; Leaf(Keyword "::"); Leaf y] ->
        match x,y with
        | List a, b -> ListLeaf (a@[Leaf b])
        | a, List b -> ListLeaf ((Leaf x)::b)

        | _ -> None
    
    | [Leaf (List x); Leaf(Keyword "map"); Node(ys)] -> ListLeaf (x |> List.map (fun item -> Node (ys@[item])))

    | _ -> None
    )
}
