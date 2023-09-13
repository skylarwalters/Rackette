open CS17SetupRackette;
open Read.Reader;
open Types /*
 * Input: addends, a two-element list of values, specifically NumVs
 * Output: a NumV(n), where n is the sum of the two elements in addends
*/;

// ---------------------- Top Level Environment Built-Ins----------------------

//+

let plus: list(value) => value =
  addends =>
    switch (addends) {
    | [NumV(a), NumV(b)] => NumV(a + b)
    | _ => failwith("Must have two numbers to add")
    } /*
 * Input: subtrahends, a two-element list of values, specifically NumVs
 * Output: a NumV(n), where n is the result of subtracting the first element in
 *    subtrahends from the second element
 */;

//-

let minus: list(value) => value =
  subtrahends =>
    switch (subtrahends) {
    | [NumV(a), NumV(b)] => NumV(a - b)
    | _ => failwith("Must have two numbers to subtract")
    } /*
 * Input: multipliers, a two-element list of values, specifically NumVs
 * Output: a NumV(n), where n is the result of multiplying the two elements in
 *   multipliers
 */;

//*

let times: list(value) => value =
  multipliers =>
    switch (multipliers) {
    | [NumV(a), NumV(b)] => NumV(a * b)
    | _ => failwith("Must have two numbers to multiply")
    } /*
 * Input: dividends, a two-element list of values, specifically NumVs
 * Output: a NumV(n), where n is the result of dividing the first element in
 *    dividends by the second element
 */;

///

let divide: list(value) => value =
  dividends =>
    switch (dividends) {
    | [NumV(a), NumV(b)] => NumV(a / b)
    | _ => failwith("Must have two numbers to divide")
    } /*
 * Input: dividends, a two-element list of values, specifically NumVs
 * Output: a NumV(n), where n is the remainder of dividing the first element in
 *    dividends by the second element
 */;

//remainder

let remainder: list(value) => value =
  dividends =>
    switch (dividends) {
    | [NumV(a), NumV(b)] => NumV(a - b * (a / b))
    | _ => failwith("Must have two numbers to find remainder")
    } /*
 * Input: nums, a two-element list of values, specifically NumVs
 * Output: a BoolV(b), where b is true if the first element of values is less
 *    than the second element, false if not
 */;

//<

let lessThan: list(value) => value =
  nums =>
    switch (nums) {
    | [NumV(a), NumV(b)] =>
      if (a < b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | _ => failwith("must have two numbers to use <")
    } /*
 * Input: nums, a two-element list of values, specifically NumVs
 * Output: a BoolV(b), where b is true if the first element of values is
 *    greater than the second element, false if not
 */;

//>

let greaterThan: list(value) => value =
  nums =>
    switch (nums) {
    | [NumV(a), NumV(b)] =>
      if (a > b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | _ => failwith("must have two numbers to use >")
    } /*
 * Input: nums, a two-element list of values, specifically NumVs
 * Output: a BoolV(b), where b is true if the first element of values is less
 *    than or equal to the second element, false if not
 */;

// <=

let lessThanEqualTo: list(value) => value =
  nums =>
    switch (nums) {
    | [NumV(a), NumV(b)] =>
      if (a <= b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | _ => failwith("must have two numbers to use <=")
    } /*
 * Input: nums, a two-element list of values, specifically NumVs
 * Output: a BoolV(b), where b is true if the first element of values is less
 *    than or equal to the second element, false if not
 */;

//>=

let greaterThanEqualTo: list(value) => value =
  nums =>
    switch (nums) {
    | [NumV(a), NumV(b)] =>
      if (a >= b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | _ => failwith("must have two numbers to use >=")
    } /*
 * Input: nums, a two-element list of values, specifically NumVs
 * Output: a BoolV(b), where b is true if the first element of values is equal
 *    to the second element, false if not
 */;

//=

let equal: list(value) => value =
  nums =>
    switch (nums) {
    | [NumV(a), NumV(b)] =>
      if (a == b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | _ => failwith("must have two numbers to use =")
    } /*
 * Input: data, a one-element list of values
 * Output: a BoolV(b), where b is true if the element in data is a NumV,
 *    false if not
 */;

// number?

let isItANumber: list(value) => value =
  data =>
    switch (data) {
    | [NumV(n)] => BoolV(true)
    | [_] => BoolV(false)
    | _ => failwith("wrong number of arguments")
    }; /*
 * Input: input, a one-element list of values, specifically a NumV
 * Output: a BoolV(b), where b is true if the element in data is NumV(0),
 *    false if not
 */

// zero?

let isItZero: list(value) => value =
  input =>
    switch (input) {
    | [NumV(0)] => BoolV(true)
    | [NumV(_)] => BoolV(false)
    | _ => failwith("can only use zero? on ints")
    } /*
 * Input: listy, a one-element list of values, specifically a ListV
 * Output: a value, the first element of the ListV contained in listy
 */;

// first

let first: list(value) => value =
  listy =>
    switch (listy) {
    | [ListV([])] => failwith("cannot call first on an empty list")
    | [ListV([head, ..._])] => head
    | _ => failwith("cannot call first on a non-list")
    } /*
 * Input: listy, a one-element list of values, specifically a ListV
 * Output: a ListV(tl), where tl represents all the elements of the original
 *    ListV contained in listy, except for the first element
 */;

//rest

let rest: list(value) => value =
  listy =>
    switch (listy) {
    | [ListV([])] => failwith("cannot call rest on an empty list")
    | [ListV([_hd, ...tl])] => ListV(tl)
    | _ => failwith("cannot call rest on a non-list")
    } /*
 * Input: lst, a two-element list of values, specifically a value of any type,
 *    then a ListV
 * Output: a ListV(v) where v is the first element of lst followed by all the
 *    elements contained in the original list v
 */;

//cons

let cons: list(value) => value =
  lst =>
    switch (lst) {
    | [item, ListV(listy)] => ListV([item, ...listy])
    | _ => failwith("cons expression must contain item, then ListV")
    } /*
 * Input: listy, a one-element list of values, specifically a ListV
 * Output: a BoolV(b), where b is true if listy contains only the empty list
 *    ListV([]), false otherwise
 */;

// empty?

let istItEmpty: list(value) => value =
  listy =>
    switch (listy) {
    | [ListV([])] => BoolV(true)
    | [ListV([_, ..._])] => BoolV(false)
    | _ => failwith("cannot call empty? on a non-list")
    } /*
 * Input: listy, a one-element list of values, specifically a ListV
 * Output: a BoolV(b), where b is true if listy contains a ListV(v) where v is a
 *    non-empty list of values, false otherwise
 */;

// cons?

let isItCons: list(value) => value =
  listy =>
    switch (listy) {
    | [ListV([])] => BoolV(false)
    | [ListV([_, ..._])] => BoolV(true)
    | _ => failwith("cannot call cons? on a non-list")
    } /*
 * Input: pred, a list of values
 * Output: a BoolV(b), where b is true if listy contains only the empty list
 *    ListV([]), false otherwise
 */;

// not

let myNot: list(value) => value =
  pred =>
    switch (pred) {
    | [BoolV(true)] => BoolV(false)
    | [BoolV(false)] => BoolV(true)
    | _ => failwith("not can only be called on boolean values")
    } /*
 * Input: data, a two-element list of values
 * Output: a BoolV(b)
 *    b is true if listy contains NumV(x) and NumV(y) and x equals y
 *    b is true if listy contains BoolV(x) and BoolV(y) and x and y are
 *      either both true or both false
 *    b is true if listy contains ListV(x) and ListV(y) and x and y are
 *      lists containing the same elements in the same order
 *    b is false otherwise
 *
 * OI: [ListV([]), ListV([])]
 *  RI: n/a
 *  RO: n/a
 *  Idea: base case -- if both lists empty, return BoolV(true)
 * OO: BoolV(true)
 *
 * OI: [ListV([]), ListV([NumV(1)])]
 *  RI: n/a
 *  RO: n/a
 *  Idea: base case -- if one list empty and the other not (either order!),
 *    return BoolV(false)
 * OO: BoolV(false)
 *
 * OI: [ListV([NumV(1)]), ListV([NumV(2), NumV(3)])]
 *  RI: [ListV([]), ListV([NumV(3)])]
 *  RO: BoolV(false)
 *  Idea: recursive call on the tail of both lists until reach a base case
 * OO: BoolV(false)
 *
 */;

// equal?

let rec equalQuery: list(value) => value =
  data =>
    switch (data) {
    | [NumV(a), NumV(b)] =>
      if (a == b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | [BoolV(a), BoolV(b)] =>
      if (a == b) {
        BoolV(true);
      } else {
        BoolV(false);
      }
    | [ListV(lst1), ListV(lst2)] =>
      switch (lst1, lst2) {
      | ([], []) => BoolV(true)
      | ([], _) => BoolV(false)
      | (_, []) => BoolV(false)
      | ([hd1, ...tl1], [hd2, ...tl2]) =>
        let BoolV(b) = equalQuery([hd1, hd2]);
        if (b) {
          equalQuery([ListV(tl1), ListV(tl2)]);
        } else {
          BoolV(false);
        };
      }
    | [_, _] => BoolV(false)
    | _ => failwith("equal? can only compare two NumVs, BoolVs, or ListVs")
    };


//---------------------------- initial TLE ----------------------------------
let initialTle: environment = [
  (Name("+"), BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus})),
  (Name("-"), BuiltinV({printedRep: "<builtin-proc-->", bProc: minus})),
  (Name("*"), BuiltinV({printedRep: "<builtin-proc-*>", bProc: times})),
  (Name("/"), BuiltinV({printedRep: "<builtin-proc-/>", bProc: divide})),
  (
    Name("remainder"),
    BuiltinV({printedRep: "<builtin-proc-remainder>", bProc: remainder}),
  ),
  (Name("="), BuiltinV({printedRep: "<builtin-proc-=>", bProc: equal})),
  (Name("<"), BuiltinV({printedRep: "<builtin-proc-<>", bProc: lessThan})),
  (
    Name(">"),
    BuiltinV({printedRep: "<builtin-proc->>", bProc: greaterThan}),
  ),
  (
    Name("<="),
    BuiltinV({printedRep: "<builtin-proc-<=>", bProc: lessThanEqualTo}),
  ),
  (
    Name(">="),
    BuiltinV({printedRep: "<builtin-proc->=>", bProc: greaterThanEqualTo}),
  ),
  (
    Name("equal?"),
    BuiltinV({printedRep: "<builtin-proc-equal?>", bProc: equalQuery}),
  ),
  (
    Name("number?"),
    BuiltinV({printedRep: "<builtin-proc-number?>", bProc: isItANumber}),
  ),
  (
    Name("zero?"),
    BuiltinV({printedRep: "<builtin-proc-zero?>", bProc: isItZero}),
  ),
  (
    Name("cons"),
    BuiltinV({printedRep: "<builtin-proc-cons>", bProc: cons}),
  ),
  (
    Name("first"),
    BuiltinV({printedRep: "<builtin-proc-first>", bProc: first}),
  ),
  (
    Name("rest"),
    BuiltinV({printedRep: "<builtin-proc-rest>", bProc: rest}),
  ),
  (
    Name("empty?"),
    BuiltinV({printedRep: "<builtin-proc-empty?>", bProc: istItEmpty}),
  ),
  (
    Name("cons?"),
    BuiltinV({printedRep: "<builtin-proc-cons?>", bProc: isItCons}),
  ),
  (Name("not"), BuiltinV({printedRep: "<builtin-proc-not>", bProc: myNot})),
] /*
 * Input: kronkrete, a concreteProgramPiece, specifically representing an
 *    expression (can't take form ListC([SymbolC("define"), ...tl]))
 * Output: an expression representing kronkrete
 *
 * OI: NumberC(3)
 *  RI: n/a
 *  RO: n/a
 *  Idea: base case -- if just a NumberC or SymbolC, output appropriate NumE,
 *    BoolE, or EmptyE
 * OO: NumE(3)
 *
 * OI: ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")])
 *  RI1: SymbolC("true")
 *  RO1: BoolE(true)
 *  RI2: SymbolC("false")
 *  RO2: BoolE(false)
 *  Idea: if first item of ListC is a SymbolC representing a keyword create an
 *    expression of corresponding keyword type, parse corresponding expressions
 * OO: AndE(BoolE(true), BoolE(false))
 *
 */;

// ---------------------------parse expression--------------------------------

let rec parseExpression: concreteProgramPiece => expression =
  kronkrete => {
    //------------------------ helper functions ------------------------------
    //helper for let pairs
    /*
     * Input: pairs, a list of concreteProgramPieces, specfically two-element
     *   ListCs containing a SymbolC and another concreteProgramPiece
     * Output: a list of letPairs; each letPair represents one
     *   ListC([SymbolC(n), exp]), where the pairName is Name(n) and the pairExpr
     *   is the parsed expression
     *
     * OI: [ListC([SymbolC("x"), NumberC(3)])]
     *  RI: n/a
     *  RO: n/a
     *  Idea: base case -- return a list of one letPair
     * OO: [{pairName: Name("x"), pairExpr: NumE(3)}]
     *
     * OI: [ListC([SymbolC("x"), NumberC(3)]),
            ListC([SymbolC("y"), NumberC(2)])]
     *  RI: [ListC([SymbolC("y"), NumberC(2)])]
     *  RO: [{pairName: Name("y"), pairExpr: NumE(2)}]
     *  Idea: make letPair out of head of list, cons onto recursive call on tail
     * OO: [pairName: Name("x"), pairExpr: NumE(3)},
     *      {pairName: Name("y"), pairExpr: NumE(2)}]
     */
    let rec letPairHelper: list(concreteProgramPiece) => list(letPair) =
      pairs =>
        switch (pairs) {
        | [] => []
        | [ListC([SymbolC(n), exp])] => [
            {pairName: Name(n), pairExpr: parseExpression(exp)},
          ]
        | [ListC([SymbolC(n), exp]), ...tl] => [
            {pairName: Name(n), pairExpr: parseExpression(exp)},
            ...letPairHelper(tl),
          ]
        | _ => failwith("improper let expression syntax")
        } /*
    * Input: tail, a list of concreteProgramPieces, specifically two-element
    *   ListCs
    * Output: a list of condData, where each element of the list represents a
    *   ListC in tail -- a ListC([pred, exp]) is represented as
    *   {conditionExpr: parsed pred, resultExpr: parsed exp}
    *
    * OI: [ListC([Symbol("true"), NumberC(3)])]
    *   RI: n/a
    *   RO: n/a
    *   Idea: base case -- when one element, create one element list of
    *     condData
    * OO: [{conditionExpr: BoolE(true), NumE(3)}]
    *
    * OI: [ListC([Symbol("false"), NumberC(3)]),
    *      ListC([Symbol("true"), NumberC(4)])]
    *   RI: [ListC([Symbol("true"), NumberC(4)])]
    *   RO: [{conditionExpr: BoolE(true), NumE(4)}]
    *   Idea: Make condData out of head of list, cons onto recursive call on
    *     tail
    * OO: [{conditionExpr: BoolE(false), NumE(3)},
    *      {conditionExpr: BoolE(true), NumE(34)}]
    *
    */;

    // helper for cond parsing (includes failwiths for poor cond formatting)

    let rec condParseHelper: list(concreteProgramPiece) => list(condData) =
      tail =>
        switch (tail) {
        | [ListC([pred, exp])] => [
            {
              conditionExpr: parseExpression(pred),
              resultExpr: parseExpression(exp),
            },
          ]
        | [ListC([pred, exp]), ...tl] => [
            {
              conditionExpr: parseExpression(pred),
              resultExpr: parseExpression(exp),
            },
            ...condParseHelper(tl),
          ]
        | _ =>
          failwith(
            "cond must be followed by at least one condition/result pair.",
          )
        } /*
    * Input: names, a list of concreteProgramPieces, specifically of SymbolCs
    *   (representing names of formal arguments in a lambda expression)
    * Output: a list of names, the parsed representations of the SymbolCs in names
    *   in their input order
    *
    * OI: [SymbolC("x")]
    *   RI: n/a
    *   RO: n/a
    *   Idea: base case -- if one-element list of SymbolC(s), create one-element
    *     list of Name(s)
    * OO: [Name("x")]
    *
    * OI: [SymbolC("x"), SymbolC("y")]
    *   RI: [SymbolC("y")]
    *   RO: [Name("y")]
    *   Idea: Make name with string from SymbolC at head of list, cons onto
    *     recursive call on tail
    * OO: [Name("x"), Name("y")]
    *
    */;

    // helper for lambda parsing

    let rec lambdaHelper: list(concreteProgramPiece) => list(name) =
      names =>
        switch (names) {
        | [SymbolC(hd)] => [Name(hd)]
        | [SymbolC(hd), ...tl] => [Name(hd), ...lambdaHelper(tl)]
        | [] => []
        | _ => failwith("lambda catchall error for pattern matching")
        };

    //  -----------------------  switch statement ---------------------------
    switch (kronkrete) {
    | NumberC(num) => NumE(num)
    | SymbolC("true") => BoolE(true)
    | SymbolC("false") => BoolE(false)
    | SymbolC("empty") => EmptyE
    | SymbolC(str) => switch(str){
        | "define"  
        | "lambda" 
        | "let" 
        | "if" 
        | "cond"
        | "and"
        | "or" => failwith("parseExpression reserved keyword")
        | _ => NameE(Name(str))
      };
    // and statements
    | ListC([SymbolC("and"), exp1, exp2]) =>
      AndE(parseExpression(exp1), parseExpression(exp2))
    //failwiths for and
    | ListC([SymbolC("and"), ..._]) =>
      failwith("and must be followed by two boolean expressions")
    //or statements
    | ListC([SymbolC("or"), exp1, exp2]) =>
      OrE(parseExpression(exp1), parseExpression(exp2))
    //failwiths for or
    | ListC([SymbolC("or"), ..._]) =>
      failwith("or must be followed by two boolean expressions")
    // if statements
    | ListC([SymbolC("if"), pred, exp1, exp2]) =>
      IfE({
        boolExpr: parseExpression(pred),
        trueExpr: parseExpression(exp1),
        falseExpr: parseExpression(exp2),
      })
    // failwith for if
    | ListC([SymbolC("if"), ..._]) =>
      failwith(
        "if statements must have a predicate followed by two expressions",
      )
    // cond statement
    | ListC([SymbolC("cond"), ...tl]) => CondE(condParseHelper(tl))
    // cond syntax error checking in helper function
    // lambda expression
    | ListC([SymbolC("lambda"), ListC(names), expr]) =>
      LambdaE({
        nameList: lambdaHelper(names),
        lambdaBody: parseExpression(expr),
      })
    //lambda failwith
    | ListC([SymbolC("lambda"), ..._]) =>
      failwith(
        "lambda expressions must contain a list of names followed by an list of expressions",
      )
    //let expression
    | ListC([SymbolC("let"), ListC(letPairs), expr]) =>
      LetE({
        letPairs: letPairHelper(letPairs),
        letBody: parseExpression(expr),
      })
    // let error
    | ListC([SymbolC("let"), ..._]) =>
      failwith(
        "let expressions must contain a list of one or more let pairs and a list of one or more expressions",
      )
    // applicationE
    | ListC([]) => failwith("cannot enter empty program")
    | ListC(n) => ApplicationE(List.map(parseExpression, n))
    };
  } /*
 * Input: concrete, a concreteProgramPiece, specifically representing a
 *    definition (takes form (ListC([SymbolC("define"), ..._]))
 * Output: a definition representing the information in concrete
*/;

//-------------------------- parseDefinition ---------------------------------

let parseDefinition: concreteProgramPiece => definition =
  concrete =>
    switch (concrete) {
    | ListC([SymbolC("define"), SymbolC(str), tlExp]) => 
      switch(str){
        | "define"  
        | "lambda" 
        | "let" 
        | "if" 
        | "cond"
        | "and"
        | "or"
        | "empty"
        | "true"
        | "false" => failwith("cannot redefine keyword")
        | _ =>
          (
        Name(str),
        parseExpression(tlExp),
        )
      }
    | ListC([SymbolC("define"), SymbolC(_), ..._]) =>
      failwith("definition must be followed by an expression")
    | _ => failwith("definition has invalid format")
    }; /*
 * Input: input, a concreteProgramPiece
 * Output: an abstractProgramPiece representing the input; inputs starting with
 *    (ListC([SymbolC("define"), ..._])) represented as Definitions, all others
 *    represented as Expression
 */

// ---------------------------- parsePiece ------------------------------------

let parsePiece: concreteProgramPiece => abstractProgramPiece =
  input =>
    switch (input) {
    | ListC([SymbolC("define"), ..._]) =>
      Definition(parseDefinition(input))
    | _ => Expression(parseExpression(input))
    } /*
 * Input: input, a concreteProgram
 * Output: an abstractProgram representing input -- a list representing each
 *    parsed piece of the concreteProgram, in its input order
 */;

//---------------------------------- parse ------------------------------------

let parse: concreteProgram => abstractProgram =
  input =>
    /* this will parse all of the pieces of this program,
     * giving us a list of pieces, our abstract syntax */
    List.map(parsePiece, input) /*
 * Input:
 *    env: an environment
 *    n: a name
 * Output: a bool; true if n is contained in env, false if not
 *
 * OI: ([], Name("x"))
 *  RI: n/a
 *  RO: n/a
 *  Idea: base case -- if env is empty, return false
 * OO: false
 *
 * OI: ([(Name("x"), NumV(1)), (Name("y"), NumV(2))], Name("y"))
 *  RI: ([(Name("y"), NumV(2))])
 *  RO: true
 *  Idea: check if n in head binding; if not, recursive call on tail of env
 * OO: true
 *
 */;

// ----------- lookup: helper function for addDefinition, lookupVal ----------

let rec lookup: (environment, name) => bool =
  (env, n) =>
    switch (env, n) {
    | ([], _) => false
    | ([(Name(a), _), ...tl], _) =>
      if (n == Name(a)) {
        true;
      } else {
        lookup(tl, n);
      }
    } /*
 * Input:
 *    env: an environment which contains n (otherwise throw error)
 *    n: a name
 * Output: a value, the value bound to n in env
 *
 * OI: ([(Name("x"), NumV(3))], Name("x"))
 *  RI: n/a
 *  RO: n/a
 *  Idea: base case -- n matches binding of head binding of env, return value
 *    n is bound to
 * OO: NumV(3)
 *
 * OI: ([(Name("x"), NumV(1)), (Name("y"), NumV(2))], Name("y"))
 *  RI: ([(Name("y"), NumV(2))], Name("y"))
 *  RO: NumV(2)
 *  Idea: check if n is in head binding of env -- if it is return the value it
 *    is bound to, if not recursive call on tail of env
 * OO: NumV(2)
 */;

// ------------------- lookupVal: helper function for eval --------------------

let rec lookupVal: (environment, name) => value =
  (env, n) =>
    switch (env, n) {
    | ([], _) => failwith("name not yet bound to value")
    | ([(Name(a), v), ...tl], Name(c)) =>
      if (c == a) {
        v;
      } else {
        lookupVal(tl, n);
      }
    } /*
 * Input:
 *  tle: an environment, representing the top level environment
 *  env: an environment, a local environment
 *  expr: an expression
 * Output: a value
 *
 * OI: NumE(3)
 *  RI: n/a
 *  RO: n/a
 *  Idea: if input is lone NameE, NumE, BoolE, or EmptyE, then simply represent
 *    as appropriate NumV, BoolV, or ListV
 * OO: NumV(3)
 *
 * OI: AndE(BoolE(true), BoolE(false))
 *  RI1: BoolE(true)
 *  RO1: BoolV(true)
 *  RI2: BoolE(false)
 *  RO2: BoolV(false)
 *  Idea: if input is an AndE, OrE, IfE, CondE, LambdaE, LetE, or ApplicationE,
 *    follow rules of evaluation to evaluate expression.  E.g. for AndE evaluate
 *    first expression; if evaluates to BoolV(false) then stop evaluation and
 *    return BoolV(false); if evaluates to BoolV(true) then evaluate the next
 *    expression; if evaluates to BoolV(false) then stop evaluation and
 *    return BoolV(false); if evaluates to BoolV(true) then return BoolV(true)
 * OO: BoolV(false)
 *
 */;

// --------------------------------- eval -------------------------------------
let rec lambdaEHelper: list('a) => bool = names =>
  switch(names){
    | [] => false
    | [hd,...tl] => 
      if(List.mem(hd, tl)){
        true
      }
      else{
        lambdaEHelper(tl)
      }
  }

let rec letEDuplicate: list(letPair) => list(name) = pairs =>
  switch(pairs){
    | [] => []
    | [{pairName: n, pairExpr: _},...tl] =>
        [n,...letEDuplicate(tl)]
  }

let rec eval: (environment, environment, expression) => value =
  (tle, env, expr) => {
    switch (expr) {
    | NameE(Name(str)) => lookupVal(env @ tle, Name(str))
    | NumE(n) => NumV(n)
    | BoolE(boo) => BoolV(boo)
    | EmptyE => ListV([])
    | AndE(exp1, exp2) =>
      let evalExp1 = eval(tle, env, exp1);
      switch (evalExp1) {
      | BoolV(true) =>
        let evalExp2 = eval(tle, env, exp2);
        switch (evalExp2) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ =>
          failwith("the second clause of an and must evaluate to a boolean")
        };
      | BoolV(false) => BoolV(false)
      | _ => failwith("the first clause of an and must evaluate to a boolean")
      };
    | OrE(exp1, exp2) =>
      let evalExp1 = eval(tle, env, exp1);
      switch (evalExp1) {
      | BoolV(true) => BoolV(true)
      | BoolV(false) =>
        let evalExp2 = eval(tle, env, exp2);
        switch (evalExp2) {
        | BoolV(true) => BoolV(true)
        | BoolV(false) => BoolV(false)
        | _ => failwith("the second clause of or must evaluate to a boolean")
        };
      | _ => failwith("the first clause of or must evaluate to a boolean")
      };
    | IfE({boolExpr: pred, trueExpr: trueExp, falseExpr: falseExp}) =>
      let a = eval(tle, env, pred);
      switch (a) {
      | BoolV(true) => eval(tle, env, trueExp)
      | BoolV(false) => eval(tle, env, falseExp)
      | _ =>
        failwith("first condition of if expression must evaluate to boolean")
      };
    | CondE([{conditionExpr: cExpr, resultExpr: rExpr}, ...tl]) =>
      switch (eval(tle, env, cExpr)) {
      | BoolV(true) => eval(tle, env, rExpr)
      | BoolV(false) => eval(tle, env, CondE(tl))
      | _ =>
        failwith("condition expressions must evaluate to true/false values")
      }
    | LambdaE({nameList: namez, lambdaBody: bod}) =>
      if(lambdaEHelper(namez)){
        failwith("lambda expression can't have duplicate arg names")
      }
      else{
        ClosureV({cNameList: namez, cExpr: bod, cEnv: env})
      };
    | LetE({letPairs: pairs, letBody: bodExpr}) =>
      let rec letEHelper:
        (environment, environment, list(letPair)) => environment = (
        (tle, env, lp) =>
          if(lambdaEHelper(letEDuplicate(pairs))){
            failwith("cannot have duplicate names in letPairs")
          }
          else{
            switch (lp) {
          | [{pairName: n, pairExpr: e}, ...tl] =>
            letEHelper(tle, [(n, eval(tle, env, e)), ...env], tl)
          | [] => env
          }
          }
          
      );
      eval(tle, letEHelper(tle, env, pairs), bodExpr);
    | ApplicationE(input) =>
      let p =
        List.map(
          fun
          | expr => eval(tle, env, expr),
          input,
        ); //maps eval onto the input!!
      switch (p) {
      | [BuiltinV({printedRep: _, bProc: procky}), ...tl] => procky(tl)
      | [ClosureV({cNameList: lst, cExpr: procky, cEnv: closureEnv}), ...tl] =>
        let rec bindHelper: (list(name), list(value)) => environment = (
          (lst, tl) =>
            switch (lst, tl) {
            | ([], []) => []
            | ([head1, ...tail1], [head2, ...tail2]) => [
                (head1, head2),
                ...bindHelper(tail1, tail2),
              ]
            | ([], _) =>
              failwith("must have same number of formals and actuals")
            | (_, []) =>
              failwith("must have same number of formals and actuals")
            }
        );
        eval(tle, bindHelper(lst, tl) @ closureEnv, procky);
      | _ => failwith("Application expression must start with a procedure")
      };
    | _ => NumV(-1234)
    };
  };

// ----------------------------- addDefinition --------------------------------
// addDefinition
//input: a tuple consisting of an environment, env, and a tuple containing a
//       name and expression, or (id, expr).
//output: an environment. If the name does not exist in the environment env,
//        then the name-expression tuple will be added to  env. If the name
//        already exists, the procedure terminates with an error.

let addDefinition: (environment, (name, expression)) => environment =
  (env, (id, expr)) =>
    switch (id, expr) {
    | (Name(_), expr) =>
      if (lookup(env, id)) {
        failwith("cannot redefine names; name already used");
      } else {
        [(id, eval(env, [], expr)), ...env];
      }
    } /* TODO: write the header comment parts required by the Design Recipe
 * and implement stringOfValue*/ /*
OI: NumV(3)
RI: n/a
RO: n/a
ideation: this acts as a base case. here, the 3 from inside of NumV can be
          "removed" through pattern matching and then converted to the OO
          via string_of_int(3)
OO: "3"

OI: ListV([NumV(3), NumV(4)])
RI: NumV(4)
RO: "(list 4"
ideation: Here, we are applying fold left to the interior of our listV to get
          to the string list equivalent. recur on the last value so it is
          converted to a string, then string-append it onto the RO. This will
          all get appended onto ) .
OO:"(list 3 4)"
*/;

// ----------------------------- stringOfValue -------------------------------
// String of Value
// input: a value, aValue
// output: a string representation of aValue.

// Recursion Diagrams for stringOfValue

let rec stringOfValue: value => string =
  aValue =>
    switch (aValue) {
    | NumV(n) => string_of_int(n)
    | BoolV(boo) => string_of_bool(boo)
    | BuiltinV(_) => "#<procedure>"
    | ListV(interior) =>
      List.fold_left(
        (base, e) => base ++ " " ++ stringOfValue(e),
        "(list",
        interior,
      )
      ++ ")"
    | ClosureV(_) => "#<procedure>"
    } /*
OI: ([], [])
RI: n/a
RO: n/a
ideation: represents a base case in which the list of abstract program pieces
          is empty. return the empty list of values
OO: []

OI: ([], [Definition(Name("x"), NumE(3))])
RI: []
RO: []
ideation: when we process the definition, we only add it to the environment. No
          value is produced that will be put in a printed format here.
OO: []

OI: ([], [Definition(Name("x"), NumE(3)), Expression(ApplicationE([
  NameE(Name("+")), NameE(Name("x")), NumE(1)
]))])
RI: ([], [Definition(Name("x"), NumE(3)))
RO: (([Name("x"), NumV(3)]), [Expression(ApplicationE([
  NameE(Name("+")), NameE(Name("x")), NumE(1)
])])
ideation: to get to the OO, recur on Expression(...) to process the expressions
          in the new tle, which now contains the binding between x and 3.
OO: [NumV(4)]

OI: [Expression(ApplicationE([NameE(Name("+"), NumE(3), NumE(1)])))]
RI: []
RO: []
ideation: to get to the OO, eval the application expression housed within
          Expression within the tle and local environment
OO: [NumV(4)]

*/;

// ------------------------------------ process ------------------------------
//process
//Input: pieces, an abstractProgram
//Output: a list of values that represents each element of abstractProgram
//        converted into a value. This is done via processHelper

//processHelper
// Input: a tuple of tle, an environment (in this case the initialTle) and
//        pieces, an abstract program
// Output: A list of values representing each elelment of pieces converted
//        to a value. If pieces is a definition, it looks up and adds the def.
//        to the top level environment (if the name is not yet bound) via add
//        definition. If pieces is an expression, it evaluates each element of
//        the expression within the top level environment extended by the local
//        environment.

//Recursion diagrams for processHelper

let process: abstractProgram => list(value) =
  pieces => {
    let rec processHelper: (environment, abstractProgram) => list(value) =
      (tle, pieces) =>
        switch (pieces) {
        | [] => []
        | [Definition(d), ...tl] => processHelper(addDefinition(tle, d), tl)
        | [Expression(e), ...tl] => [
            eval(tle, [], e),
            ...processHelper(tle, tl),
          ]
        };
    processHelper(initialTle, pieces);
  };

// -------------------------- Rackette! the big boy. -------------------------
//Rackette
//Input: a rawProgram, program, in the form of a string
//Output: the raw program, program, interpreted into a list of strings that can
//        be printed. This represents the processing and evaluation of a racket
//        program into the printed form shown by DrRacket

let rackette: rawProgram => list(string) =
  program => List.map(stringOfValue, process(parse(readAll(program))));

// ----------------------------------------------------------------------------
// ------------------------------- TEST CASES ---------------------------------
// ----------------------------------------------------------------------------

// Given Test Cases
// sample test: parseExpression on concreteProgramPiece
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parse empty expression",
);
// sample test: parseExpression with read
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parse empty expression",
);

//------------- check expects for parseExpression and parseDefinition ---------

//basic concreteProgramPieces- NumberC, SymbolC, ListC
// empty list
checkExpectExpression(
  parseExpression(SymbolC("empty")),
  EmptyE,
  "parseExpression on empty list",
);
checkExpectExpression(
  parseExpression(read("empty")),
  EmptyE,
  "read and parseExpression on empty expression",
);

// names
checkExpectExpression(
  parseExpression(SymbolC("x")),
  NameE(Name("x")),
  "parseExpression on a name",
);
checkExpectExpression(
  parseExpression(read("x")),
  NameE(Name("x")),
  "read and parseExpression on a name",
);

//numbers
checkExpectExpression(
  parseExpression(NumberC(1)),
  NumE(1),
  "parseExpression on a number",
);
checkExpectExpression(
  parseExpression(read("3")),
  NumE(3),
  "read and parseExpression on a number",
);

//booleans
checkExpectExpression(
  parseExpression(SymbolC("true")),
  BoolE(true),
  "parseExpression on boolean",
);
checkExpectExpression(
  parseExpression(read("false")),
  BoolE(false),
  "read and parseExpression on boolean",
);

// basic applicationE
checkExpectExpression(
  parseExpression(ListC([SymbolC("+"), NumberC(1), NumberC(2)])),
  ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
  "parseExpression ListC to basic ApplicationE",
);

checkExpectExpression(
  parseExpression(read("(+ 1 2)")),
  ApplicationE([NameE(Name("+")), NumE(1), NumE(2)]),
  "read and parseExpression basic ListC to ApplicationE",
);

//check expects for specific expression outputs
//andE
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")]),
  ),
  AndE(BoolE(true), BoolE(false)),
  "parseExpression for and",
);
checkExpectExpression(
  parseExpression(read("(and true true)")),
  AndE(BoolE(true), BoolE(true)),
  "read and parseExpression for and",
);

//orE
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("or"), SymbolC("true"), SymbolC("false")]),
  ),
  OrE(BoolE(true), BoolE(false)),
  "parseExpression for or",
);

checkExpectExpression(
  parseExpression(read("(or false false)")),
  OrE(BoolE(false), BoolE(false)),
  "read and parseExpression for or",
);

//ifE
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("if"),
      ListC([SymbolC("<"), NumberC(0), SymbolC("x")]),
      SymbolC("x"),
      NumberC(0),
    ]),
  ),
  IfE({
    boolExpr:
      ApplicationE([NameE(Name("<")), NumE(0), NameE(Name("x"))]),
    trueExpr: NameE(Name("x")),
    falseExpr: NumE(0),
  }),
  "parseExpression for if",
);

checkExpectExpression(
  parseExpression(read("(if (< 0 x) x 0 )")),
  IfE({
    boolExpr:
      ApplicationE([NameE(Name("<")), NumE(0), NameE(Name("x"))]),
    trueExpr: NameE(Name("x")),
    falseExpr: NumE(0),
  }),
  "read and parseExpression for if",
);

//Cond E
checkExpectExpression(
  parseExpression(
    ListC([SymbolC("cond"), ListC([SymbolC("empty"), SymbolC("empty")])]),
  ),
  CondE([{conditionExpr: EmptyE, resultExpr: EmptyE}]),
  "parseExpresion for cond",
);
checkExpectExpression(
  parseExpression(read("(cond ((> x 1) x) ((<= x 1) 1))")),
  CondE([
    {
      conditionExpr:
        ApplicationE([NameE(Name(">")), NameE(Name("x")), NumE(1)]),
      resultExpr: NameE(Name("x")),
    },
    {
      conditionExpr:
        ApplicationE([NameE(Name("<=")), NameE(Name("x")), NumE(1)]),
      resultExpr: NumE(1),
    },
  ]),
  "read and parseExpression for cond",
);

//lambda
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("x")]),
      ListC([SymbolC("*"), SymbolC("x"), NumberC(1)]),
    ]),
  ),
  LambdaE({
    nameList: [Name("x")],
    lambdaBody:
      ApplicationE([NameE(Name("*")), NameE(Name("x")), NumE(1)]),
  }),
  "parseExpression for lambda",
);
checkExpectExpression(
  parseExpression(read("(lambda (x y) (+ x y))")),
  LambdaE({
    nameList: [Name("x"), Name("y")],
    lambdaBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "read and parseExpression lambda",
);

//letE
checkExpectExpression(
  parseExpression(
    ListC([
      SymbolC("let"),
      ListC([ListC([SymbolC("x"), NumberC(1)])]),
      ListC([SymbolC("*"), SymbolC("x"), NumberC(5)]),
    ]),
  ),
  LetE({
    letPairs: [{pairName: Name("x"), pairExpr: NumE(1)}],
    letBody: ApplicationE([NameE(Name("*")), NameE(Name("x")), NumE(5)]),
  }),
  "parseExpression for let",
);

checkExpectExpression(
  parseExpression(read("(let ((x 1) (y 3)) (+ x y) )")),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(1)},
      {pairName: Name("y"), pairExpr: NumE(3)},
    ],
    letBody:
      ApplicationE([
        NameE(Name("+")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "read and parseExpression let",
);

//------------------------------- parseDefinition -----------------------------

checkExpectDefinition(
  parseDefinition(read("(define x 2)")),
  (Name("x"), NumE(2)),
  "read and parseDefinition w/ name set to a number",
);
checkExpectDefinition(
  parseDefinition(ListC([SymbolC("define"), SymbolC("x"), NumberC(1)])),
  (Name("x"), NumE(1)),
  "parseDefinition w/ name set to number",
);

checkExpectDefinition(
  parseDefinition(read("(define x (+ 3 1))")),
  (Name("x"), ApplicationE([NameE(Name("+")), NumE(3), NumE(1)])),
  "read and parseDefinition w/ name set to a number",
);
checkExpectDefinition(
  parseDefinition(
    ListC([
      SymbolC("define"),
      SymbolC("x"),
      ListC([SymbolC("/"), NumberC(10), NumberC(2)]),
    ]),
  ),
  (Name("x"), ApplicationE([NameE(Name("/")), NumE(10), NumE(2)])),
  "parseDefinition w/ name set to application",
);

checkExpectDefinition(
  parseDefinition(read("(define y (cons 3 (cons 5 empty)))")),
  (
    Name("y"),
    ApplicationE([
      NameE(Name("cons")),
      NumE(3),
      ApplicationE([NameE(Name("cons")), NumE(5), EmptyE]),
    ]),
  ),
  "parseDefinition to list",
);

checkExpectDefinition(
  parseDefinition(read("(define x true)")),
  (Name("x"), BoolE(true)),
  "read and parseDefinition w/ name set to a boolean",
);

// ------------------------------- parsePiece --------------------------------

// definitions
checkExpectAbstractProgramPiece(
  parsePiece(read("(define x true)")),
  Definition((Name("x"), BoolE(true))),
  "read and parse piece on boolean definition",
);

checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("define"), SymbolC("x"), SymbolC("false")])),
  Definition((Name("x"), BoolE(false))),
  "parse piece w/ name set to boolean",
);

checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("define"), SymbolC("x"), SymbolC("false")])),
  Definition((Name("x"), BoolE(false))),
  "parse piece w/ name set to boolean",
);

//parse piece for expressions
//nums
checkExpectAbstractProgramPiece(
  parsePiece(NumberC(9)),
  Expression(NumE(9)),
  "parse piece for num",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("9")),
  Expression(NumE(9)),
  "parse piece and read for num",
);

//bools
checkExpectAbstractProgramPiece(
  parsePiece(read("true")),
  Expression(BoolE(true)),
  "parse piece and read for num",
);

//empty
checkExpectAbstractProgramPiece(
  parsePiece(read("empty")),
  Expression(EmptyE),
  "parse piece and read for empty",
);

// names
checkExpectAbstractProgramPiece(
  parsePiece(read("x")),
  Expression(NameE(Name("x"))),
  "parse piece and read for name",
);
checkExpectAbstractProgramPiece(
  parsePiece(SymbolC("x")),
  Expression(NameE(Name("x"))),
  "parse piece for name",
);

//and
checkExpectAbstractProgramPiece(
  parsePiece(read("(and true false)")),
  Expression(AndE(BoolE(true), BoolE(false))),
  "parse piece and read for and",
);
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("and"), SymbolC("true"), SymbolC("false")])),
  Expression(AndE(BoolE(true), BoolE(false))),
  "parseExpression for and",
);

//or
checkExpectAbstractProgramPiece(
  parsePiece(ListC([SymbolC("or"), SymbolC("true"), SymbolC("false")])),
  Expression(OrE(BoolE(true), BoolE(false))),
  "parse piece for or",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(or false false)")),
  Expression(OrE(BoolE(false), BoolE(false))),
  "read and parse piece for or",
);

//if
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      SymbolC("if"),
      ListC([SymbolC("<"), NumberC(0), SymbolC("x")]),
      SymbolC("x"),
      NumberC(0),
    ]),
  ),
  Expression(
    IfE({
      boolExpr:
        ApplicationE([NameE(Name("<")), NumE(0), NameE(Name("x"))]),
      trueExpr: NameE(Name("x")),
      falseExpr: NumE(0),
    }),
  ),
  "parse piece for if",
);

checkExpectAbstractProgramPiece(
  parsePiece(read("(if (< 0 x) x 0 )")),
  Expression(
    IfE({
      boolExpr:
        ApplicationE([NameE(Name("<")), NumE(0), NameE(Name("x"))]),
      trueExpr: NameE(Name("x")),
      falseExpr: NumE(0),
    }),
  ),
  "read and parse piece for if",
);

//cond
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([SymbolC("cond"), ListC([SymbolC("empty"), SymbolC("empty")])]),
  ),
  Expression(CondE([{conditionExpr: EmptyE, resultExpr: EmptyE}])),
  "parse piece for cond",
);
checkExpectAbstractProgramPiece(
  parsePiece(read("(cond ((> x 1) x) ((<= x 1) 1))")),
  Expression(
    CondE([
      {
        conditionExpr:
          ApplicationE([NameE(Name(">")), NameE(Name("x")), NumE(1)]),
        resultExpr: NameE(Name("x")),
      },
      {
        conditionExpr:
          ApplicationE([NameE(Name("<=")), NameE(Name("x")), NumE(1)]),
        resultExpr: NumE(1),
      },
    ]),
  ),
  "read and parsePiece for cond",
);

//lambda
checkExpectAbstractProgramPiece(
  parsePiece(
    ListC([
      SymbolC("lambda"),
      ListC([SymbolC("y")]),
      ListC([SymbolC("/"), SymbolC("y"), NumberC(10)]),
    ]),
  ),
  Expression(
    LambdaE({
      nameList: [Name("y")],
      lambdaBody:
        ApplicationE([NameE(Name("/")), NameE(Name("y")), NumE(10)]),
    }),
  ),
  "parsePiece for lambda",
);

checkExpectAbstractProgramPiece(
  parsePiece(read("(lambda (x y) (- x y))")),
  Expression(
    LambdaE({
      nameList: [Name("x"), Name("y")],
      lambdaBody:
        ApplicationE([
          NameE(Name("-")),
          NameE(Name("x")),
          NameE(Name("y")),
        ]),
    }),
  ),
  "read and parse piece lambda",
);

//let
checkExpectExpression(
  parseExpression(read("(let ((x 12) (y 2)) (remainder x y) )")),
  LetE({
    letPairs: [
      {pairName: Name("x"), pairExpr: NumE(12)},
      {pairName: Name("y"), pairExpr: NumE(2)},
    ],
    letBody:
      ApplicationE([
        NameE(Name("remainder")),
        NameE(Name("x")),
        NameE(Name("y")),
      ]),
  }),
  "read and parse piece let",
);

//---------------------------- parsing and evaluating -------------------------

// numVs
checkExpect(
  eval(initialTle, [], parseExpression(read("3"))),
  NumV(3),
  "eval and parse num",
);

// names + fun with local and top level environments!!
checkExpect(
  eval([(Name("x"), NumV(3))], [], parseExpression(read("x"))),
  NumV(3),
  "eval a name to its value in the tle",
);
checkExpect(
  eval(
    [(Name("x"), NumV(3))],
    [(Name("x"), NumV(99))],
    parseExpression(read("x")),
  ),
  NumV(99),
  "parse and eval a name to its value in the local environment",
);

//booleans
checkExpect(
  eval(initialTle, [], parseExpression(read("true"))),
  BoolV(true),
  "eval and parse bool",
);
checkExpect(
  eval(initialTle, [], OrE(BoolE(false), BoolE(false))),
  BoolV(false),
  "eval or expression to a boolean",
);

//empty lists
checkExpect(
  eval(initialTle, [], parseExpression(read("empty"))),
  ListV([]),
  "eval and parse empty list",
);

checkExpect(
  rackette("(if (> 3 4) 3 (if false 6 7))"),
  ["7"],
  "complex if statement ***************************",
);

// ---------------------------- eval on builtins ----------------------------

//testing builtins - eval
checkExpect(
  eval(initialTle, [], parseExpression(read("(+ 3 5)"))),
  NumV(8),
  "parsing and evaluating adding!",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(- 10 5)"))),
  NumV(5),
  "parsing and eval subtracting!",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(* 3 5)"))),
  NumV(15),
  "parse and eval mutliplying!",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(/ 100 5)"))),
  NumV(20),
  "parse and eval divide!",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(remainder 10 3)"))),
  NumV(1),
  "parse and eval remainder 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(remainder 9 8)"))),
  NumV(1),
  "parse and eval remainder 2",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(remainder 10 11)"))),
  NumV(10),
  "parse and eval remainder 3",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(= 2 3)"))),
  BoolV(false),
  "parse and eval =",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(< 2 5)"))),
  BoolV(true),
  "parse eval <",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(> 2 5)"))),
  BoolV(false),
  "parse eval >",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(<= 2 5)"))),
  BoolV(true),
  "parse eval <= 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(<= 5 5)"))),
  BoolV(true),
  "parse eval <= 2",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(<= 6 5)"))),
  BoolV(false),
  "parse eval <= 3",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(>= 2 5)"))),
  BoolV(false),
  "parse eval >= 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(>= 5 5)"))),
  BoolV(true),
  "parse eval >= 2",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(>= 6 5)"))),
  BoolV(true),
  "parse eval >= 3",
);
checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(
      read("(equal? (cons 3 (cons 4 empty)) (cons 3 (cons 4 empty)))"),
    ),
  ),
  BoolV(true),
  "parse and eval equal? list 1",
);
checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(
      read(
        "(equal? (cons 3 (cons 4 empty)) (cons 3 (cons 4 (cons 5 empty))))",
      ),
    ),
  ),
  BoolV(false),
  "parse and eval equal? list 2",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(equal? 1 2)"))),
  BoolV(false),
  "parse and eval equal? num 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(equal? 1 1)"))),
  BoolV(true),
  "parse and eval equal? num 2",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(equal? true true)"))),
  BoolV(true),
  "parse and eval equal? bool 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(equal? true false)"))),
  BoolV(false),
  "parse and eval equal? bool 2",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(number? 4)"))),
  BoolV(true),
  "parse and eval number? 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(number? empty)"))),
  BoolV(false),
  "parse and eval number? 2",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(zero? 5)"))),
  BoolV(false),
  "parse and eval zero?",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(zero? 0)"))),
  BoolV(true),
  "parse and eval zero?",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(cons 3 empty)"))),
  ListV([NumV(3)]),
  "parse and eval cons 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(cons 3 (cons 4 empty))"))),
  ListV([NumV(3), NumV(4)]),
  "parse and eval cons 2",
);
checkExpect(
  rackette("(rest (cons 3 (cons 4 (cons 5 empty))))"),
  ["(list 4 5)"],
  "rest",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(empty? (cons 3 empty))"))),
  BoolV(false),
  "empty? parse and eval 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(empty? empty)"))),
  BoolV(true),
  "empty? parse and eval 2",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(cons? (cons 3 empty))"))),
  BoolV(true),
  "cons? parse and eval 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(cons? empty)"))),
  BoolV(false),
  "cons? parse and eval 2",
);

checkExpect(
  eval(initialTle, [], parseExpression(read("(not true)"))),
  BoolV(false),
  "not parse and eval 1",
);
checkExpect(
  eval(initialTle, [], parseExpression(read("(not (> 2 5))"))),
  BoolV(true),
  "not parse and eval 2",
);

checkExpect(
  eval(
    initialTle,
    [],
    parseExpression(read("(first (cons 3 (cons 4 (cons 5 empty)))))")),
  ),
  NumV(3),
  "first",
);

checkExpect(
  process(parse([ListC([SymbolC("+"), NumberC(1), NumberC(2)])])),
  [NumV(3)],
  "process",
);

// NameE
checkExpect(
  eval(initialTle, [(Name("x"), NumV(3))], NameE(Name("x"))),
  NumV(3),
  "eval: number",
);

//NumE
checkExpect(eval(initialTle, [], NumE(4)), NumV(4), "eval: number");

//BoolE
checkExpect(
  eval(initialTle, [], BoolE(true)),
  BoolV(true),
  "eval: boolean",
);

//EmptyE
checkExpect(eval(initialTle, [], EmptyE), ListV([]), "eval: empty list");

//AndE
checkExpect(
  eval(initialTle, [], AndE(BoolE(true), BoolE(false))),
  BoolV(false),
  "eval: and",
);

//OrE
checkExpect(
  eval(initialTle, [], OrE(BoolE(true), BoolE(false))),
  BoolV(true),
  "eval: and",
);

//IfE
checkExpect(
  eval(
    initialTle,
    [],
    IfE({boolExpr: BoolE(true), trueExpr: NumE(3), falseExpr: NumE(4)}),
  ),
  NumV(3),
  "eval: if",
);

//CondE
checkExpect(
  eval(
    initialTle,
    [],
    CondE([{conditionExpr: BoolE(true), resultExpr: NumE(1)}]),
  ),
  NumV(1),
  "eval: cond",
);

//LambdaE
checkExpect(
  eval(
    initialTle,
    [],
    LambdaE({nameList: [Name("x")], lambdaBody: NumE(3)}),
  ),
  ClosureV({cNameList: [Name("x")], cExpr: NumE(3), cEnv: []}),
  "eval: lambda",
);

//LetE
checkExpect(
  eval(
    initialTle,
    [],
    LetE({
      letPairs: [{pairName: Name("x"), pairExpr: NumE(1)}],
      letBody: BoolE(false),
    }),
  ),
  BoolV(false),
  "eval: let",
);

//ApplicationE
checkExpect(
  eval(
    initialTle,
    [],
    ApplicationE([NameE(Name("+")), NumE(1), NumE(4)]),
  ),
  NumV(5),
  "eval: application",
);

// -------------------------- testing process --------------------------------

checkExpect(
  process([
    Expression(ApplicationE([NameE(Name("+")), NumE(1), NumE(4)])),
  ]),
  [NumV(5)],
  "process: applicationE",
);

checkExpect(
  process([
    Expression(LambdaE({nameList: [Name("x")], lambdaBody: NumE(3)})),
  ]),
  [ClosureV({cNameList: [Name("x")], cExpr: NumE(3), cEnv: []})],
  "process: lambda",
);

//process is further tested via running rackette *****

//-------------------------- testing Rackette! -------------------------------
//Rackette test cases
//binary types
checkExpect(rackette("3"), ["3"], "rackette: single number input");
checkExpect(rackette("3 4"), ["3", "4"], "rackette: two number input");
checkExpect(rackette("true"), ["true"], "rackette: single boo, input");

//defining a name
checkExpect(
  rackette("(define x 2) x"),
  ["2"],
  "rackette: define num, return num",
);
//defining a name
checkExpect(
  rackette("(define y 6) (define x (if (> y 7) 1 2)) x"),
  ["2"],
  "rackette: defining two values, using one value to define the other",
);

checkExpect(rackette("(+ 3 5)"), ["8"], "rackette: adding");

checkExpect(
  rackette("(define x 1) (+ x 2)"),
  ["3"],
  "rackette: define name, add to name",
);

checkExpect(rackette("(> 3 5)"), ["false"], "rackette: num comparison");

checkExpect(
  rackette("(define x 3) (define y 4) (> y x)"),
  ["true"],
  "rackette: define variables and compare",
);

checkExpect(rackette("(and (> 5 4) (< 4 8))"), ["true"], "rackette on and");
checkExpect(
  rackette("(and (> 3 4) (< 4 8))"),
  ["false"],
  "rackette: and -> short circuit to false",
);
checkExpect(
  rackette("(and (> 6 4) (< 9 8))"),
  ["false"],
  "rackette: and, false on second expression",
);

checkExpect(
  rackette("(or (> 5 4) (< 4 8))"),
  ["true"],
  "rackette: or -> short circuit to true",
);
checkExpect(
  rackette("(or (> 1 4) (< 4 8))"),
  ["true"],
  "rackette:  or -> second expr true, first false",
);

checkExpect(
  rackette("(define a 8) (if (>= a 8) 8 1)"),
  ["8"],
  "rackette: if",
);

checkExpect(
  rackette("(define a 1) (if (= a 1) 99 100)"),
  ["99"],
  "rackette: a eval to true",
);

checkExpect(
  rackette("(define a false)(if a 99 100)"),
  ["100"],
  "rackette: a eval to false",
);

checkExpect(
  rackette("(define a 1) (+ a 2)"),
  ["3"],
  "simple rackette define",
);
checkExpect(
  rackette("(define a true) (not a)"),
  ["false"],
  "simple rackette define with bool",
);
checkExpect(
  rackette("(define a 1) (if (= a 1) 4 1)"),
  ["4"],
  "simple rackette define with if",
);
checkExpect(
  rackette("(define a empty) (if (empty? a) 4 1)"),
  ["4"],
  "simple rackette define with empty, if",
);
checkExpect(
  rackette("(define a false) (if a 4 1)"),
  ["1"],
  "simple rackette define with if",
);

checkExpect(
  rackette(
    "(define u (cons 8 (cons 1 empty)))(if (> (first u) 5) (first u) (rest u))",
  ),
  ["8"],
  "rackette: if and first on list",
);
checkExpect(
  rackette(
    "(define u (cons 3 (cons 1 empty)))(if (> (first u) 5) (first u) (rest u))",
  ),
  ["(list 1)"],
  "rackette: if and rest on list",
);

//cond
checkExpect(
  rackette("(define a 1) (cond ((= a 1) 4))"),
  ["4"],
  "simple rackette define with cond",
);

// cond and let
// used rackette expression that cannot be shortened to 80 char limit
checkExpect(
  rackette(
    "(define a (cons 3 (cons 99 (cons 0 empty))))(cond ((> 1 (first a))"
    ++ "(first a)) ((= 1 (first a)) 1) ((< 1 (first a)) (let ((x 4))"
    ++ "(cons x a))))",
  ),
  ["(list 4 3 99 0)"],
  "rackette: cond and let",
);

// used rackette expression that cannot be shortened to 80 char limit
checkExpect(
  rackette(
    "(define a (cons 3 (cons 99 (cons 0 empty)))) (cond ((>= 1 (first a))"
    ++ "(first a)) ((< 1 (first a)) ((lambda (x) (* x 3)) (first a))))",
  ),
  ["9"],
  "rackette: cond and lambda",
);

//lambda
checkExpect(
  rackette("((lambda (x y z) (cons x (cons y (cons z empty)))) 9 0 8)"),
  ["(list 9 0 8)"],
  "rackette: use lambda to form a list",
);
checkExpect(
  rackette("((lambda (x y z) (+ x (- z y))) 9 0 8)"),
  ["17"],
  "rackette: use lambda to add",
);

checkExpect(
  rackette("(define f (lambda (x) (+ x 2)))(f 3)"),
  ["5"],
  "rackette: checking basic lambda application",
);

checkExpect(
  rackette("((lambda (x) (* x x)) 4)"),
  ["16"],
  "rackette: basic application with lambda",
);

checkExpect(
  rackette("(lambda (x) (* 8 x))"),
  ["#<procedure>"],
  "rackette: use lambda to create procedure",
);

//let
checkExpect(
  rackette("(let (( x 8 ) (y 3)) (- x y))"),
  ["5"],
  "rackette: let expression",
);
checkExpect(
  rackette("(let ((x 2) (y 6)) (let (( x y )) (/ y x)))"),
  ["1"],
  "rackette: nested lets",
);

checkExpect(
  rackette(
    "(let (( x 8 ) (y 3)) (let ((x y)) ((lambda (x y) (+ x y)) x y)))",
  ),
  ["6"],
  "rackette: nested lets and lambda",
);

checkExpect(
  rackette("(define x 11) (let ((x 4))(cond ((> 10 x) x) ((<= 10 x) 11)))"),
  ["4"],
  "rackette: nested let, cond, and local vs. tle",
);

//for giant programs, misc., and lists of outputs

checkExpect(
  rackette(
    "((lambda (x y) ((lambda (y) (+ x y)) x)) (let ((x 0) (y 18))"
    ++ "(let ((f (lambda (a b) (+ x b ))) (x 17)) (f y x))) 18)",
  ),
  ["34"],
  "ahh!",
);
checkExpect(
  rackette(
    "((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)(let ((x 0) (y 18))"
    ++ "(let ((f (lambda (a b) (+ x b ))) (x 17)) (f y x)))(define y 17)"
    ++ "(let ((y 3)) ( + y 7) )",
  ),
  ["34", "17", "10"],
  "programs in succession w/ local env manipulation",
);

checkExpect(
  rackette(
    "(define x (cons 4 (cons 5 (cons 6 empty))))(cond((> 9 (first x))"
    ++ "(cons 9 x)))x(let ((x 3)) (cons x (cons x empty)))x",
  ),
  ["(list 9 4 5 6)", "(list 4 5 6)", "(list 3 3)", "(list 4 5 6)"],
  "changing local and tle, multi-procedure",
);

checkExpect(
  rackette(
    "(define a empty)(let ((a (/ 99 (+ 10 1))))"
    ++ "(cons a empty))(empty? a)(equal? a empty)",
  ),
  ["(list 9)", "true", "true"],
  "multi procedure output, lists, let",
);

checkExpect(
  rackette(
    "(define a 9397983923)(define b 33333)(let ((b 9397983923))"
    ++ "(and (equal? b a) (not false)))(equal? b a)",
  ),
  ["true", "false"],
  "multi-output, with equal?, not, let, and local env. manipulation",
);

// ------------------------- add Definition checking --------------------------

//add definition
checkExpect(
  addDefinition([], (Name("x"), NumE(3))),
  [(Name("x"), NumV(3))],
  "checking addDefinition",
);

checkExpect(
  addDefinition([(Name("y"), BoolV(true))], (Name("x"), NumE(3))),
  [(Name("x"), NumV(3)), (Name("y"), BoolV(true))],
  "checking addDefinition -> already have definition in env",
);

// --------------------- rackette on "eval by hand" section -----------------

checkExpect(
  rackette("((lambda (x y) ((lambda (y) (+ x y)) x)) 17 18)"),
  ["34"],
  "eval by hand #1",
);

checkExpect(
  rackette(
    "(let ((x 0) (y 18)) (let ((f (lambda (a b) (+ x b ))) (x 17)) (f y x)))",
  ),
  ["17"],
  "eval by hand #2",
);

checkExpect(rackette("(+ 3 5)"), ["8"], "eval by hand #3");

checkExpect(
  rackette("(define y 17) (let ((y 3)) ( + y 7) )"),
  ["10"],
  "eval by hand #4",
);

// ------------------------- checkErrors --------------------------------------

//--------------------- for builtin procedures --------------------------------
//adding
checkError(() => rackette("(+)"), "Must have two numbers to add");
checkError(() => rackette("(+ 3 true)"), "Must have two numbers to add");

// subtracting
checkError(() => rackette("(-)"), "Must have two numbers to subtract");
checkError(
  () => rackette("(- 3 true)"),
  "Must have two numbers to subtract",
);
//mutliplying
checkError(() => rackette("(*)"), "Must have two numbers to multiply");
checkError(
  () => rackette("(* 11 false)"),
  "Must have two numbers to multiply",
);

//dividing
checkError(() => rackette("(/)"), "Must have two numbers to divide");
checkError(
  () => rackette("(/ 10 false)"),
  "Must have two numbers to divide",
);
//remainder
checkError(
  () => rackette("(remainder)"),
  "Must have two numbers to find remainder",
);
checkError(
  () => rackette("(remainder 3 true)"),
  "Must have two numbers to find remainder",
);
//less than
checkError(() => rackette("(<)"), "must have two numbers to use <");
checkError(
  () => rackette("(< false true)"),
  "must have two numbers to use <",
);
//greater than
checkError(() => rackette("(>)"), "must have two numbers to use >");
checkError(
  () => rackette("(> false true)"),
  "must have two numbers to use >",
);
//greater than equal to
checkError(() => rackette("(>=)"), "must have two numbers to use >=");
checkError(() => rackette("(>= 0 true)"), "must have two numbers to use >=");
//less than equal to
checkError(() => rackette("(<=)"), "must have two numbers to use <=");
checkError(
  () => rackette("(<= false 4)"),
  "must have two numbers to use <=",
);
//equal => not to numvs
checkError(() => rackette("(=)"), "must have two numbers to use =");
checkError(
  () => rackette("(= false true)"),
  "must have two numbers to use =",
);
//zero?
checkError(() => rackette("(zero? empty)"), "can only use zero? on ints");

//first
checkError(
  () => rackette("(first empty)"),
  "cannot call first on an empty list",
);
checkError(() => rackette("(first 7)"), "cannot call first on a non-list");

//rest
checkError(
  () => rackette("(rest empty)"),
  "cannot call rest on an empty list",
);
checkError(() => rackette("(rest 7)"), "cannot call rest on a non-list");

//cons
checkError(
  () => rackette("(cons 5)"),
  "cons expression must contain item, then ListV",
);

//empty?
checkError(() => rackette("(empty? 5)"), "cannot call empty? on a non-list");

//cons?
checkError(() => rackette("(cons? 5)"), "cannot call cons? on a non-list");

//not
checkError(
  () => rackette("(not 5)"),
  "not can only be called on boolean values",
);

//equal?
checkError(
  () => rackette("(equal? 5)"),
  "equal? can only compare two NumVs, BoolVs, or ListVs",
);

//-------------------- for parse failures ----------------------------------
//and
checkError(
  () => rackette("(and true)"),
  "and must be followed by two boolean expressions",
);
//or
checkError(
  () => rackette("(or true 5 8)"),
  "or must be followed by two boolean expressions",
);
//if
checkError(
  () => rackette("(if true)"),
  "if statements must have a predicate followed by two expressions",
);
checkError(
  () => rackette("(if true 5 false 4)"),
  "if statements must have a predicate followed by two expressions",
);

//cond
checkError(
  () => rackette("(cond)"),
  "cond must be followed by at least one condition/result pair.",
);

//lambda
checkError(
  () => rackette("(lambda)"),
  "lambda expressions must contain a list of names"
  ++ " followed by an list of expressions",
);
checkError(
  () => rackette("(lambda 3 (+ 1 1))"),
  "lambda expressions must contain a list of names followed by an list of"
  ++ " expressions",
);

checkError(
  () => rackette("(lambda empty (+ 2 1))"),
  "lambda expressions must contain a list of names followed by an list of"
  ++ " expressions",
);

// let
checkError(
  () => rackette("(let 3)"),
  "let expressions must contain a list of one or more let pairs"
  ++ " and a list of one or more expressions",
);
checkError(
  () => rackette("(let (empty) (+ x 1))"),
  "improper let expression syntax",
);
checkError(
  () => rackette("(let (x) (+ x 1))"),
  "improper let expression syntax",
);

// ----------------------- parse definition errors -------------------------
//parse definition
checkError(() => rackette("(define 8 7)"), "definition has invalid format");
checkError(
  () => rackette("(define a)"),
  "definition must be followed by an expression",
);

//-------------------- for eval errors ----------------------------------
//checkError(() => rackette("(and 1 false)"), )

//lookup Val error (helper for eval)
checkError(() => rackette("(+ n 1)"), "name not yet bound to value");

//and
checkError(
  () => rackette("(and 1 false)"),
  "the first clause of an and must evaluate to a boolean",
);
checkError(
  () => rackette("(and true 1)"),
  "the second clause of an and must evaluate to a boolean",
);

//or
checkError(
  () => rackette("(or 1 false)"),
  "the first clause of or must evaluate to a boolean",
);
checkError(
  () => rackette("(or false 1)"),
  "the second clause of or must evaluate to a boolean",
);

//if
checkError(
  () => rackette("(if 1 3 4)"),
  "first condition of if expression must evaluate to boolean",
);

//cond
checkError(
  () => rackette("(cond (1 3) (3 4))"),
  "condition expressions must evaluate to true/false values",
);

//applicationE
checkError(
  () => rackette("((lambda (x) (+ x 1)) 4 1)"),
  "must have same number of formals and actuals",
);
checkError(
  () => rackette("((lambda (x) (+ x 1)))"),
  "must have same number of formals and actuals",
);

checkError(
  () =>
    eval(
      initialTle,
      [],
      parseExpression(ListC([NumberC(3), NumberC(4), NumberC(5)])),
    ),
  "Application expression must start with a procedure",
);

//error for addDefinition
checkError(
  () => addDefinition([(Name("n"), NumV(3))], (Name("n"), NumE(7))),
  "cannot redefine names; name already used",
);
