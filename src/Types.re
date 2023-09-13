//Data Definitions
//rawProgram
/* A rawProgram is a string representation of the raw, inputted text of a 
Rackette program. A rawProgram is a string followed by parentheticals, which
house the actual Rackette program. 
*/
/* Examples for a rawProgram:
   "17"
   "hello"
   "true"
   "(+ 3 4)"
   "(- 8 0)"
   "(+ 9 (/ 6 2))"
   "(define a 3)"
   "(define b (+ 9 8))"
   "(define f (lambda (x) (+ x 2)))"
   "(let ((x 1)) (+ x 8))"
   "(cons 7 empty)"
   "(define list1 empty)"
   "(define list2 (cons 8 (cons 0 (cons 4 empty))))"
*/
type rawProgram = string;
 

/*
concreteProgramPiece
a concreteProgramPiece is the "read" version of a section of Rackette program 
in which each "piece" of the rawProgram was broken down into constituent parts,
ex. strings, numbers, or lists of these pieces.
It is either a:
NumberC(n), which represents an int n
SymbolC(s), which represents non-punctuation text s
ListC(lists), which represents a series of NumberCs, SymbolCs, and ListCs.


/* Examples for concreteProgramPiece:
   NumberC(8)
   NumberC(99)
   NumberC(2)

   SymbolC("+")
   SymbolC("*")
   SymbolC("lambda")
   SymbolC("cons")
   SymbolC("first")
   SymbolC(">=")

   ListC([(SymbolC("+"), NumberC(2), NumberC(9))])
   ListC([(SymbolC("lambda"), NumberC(2), NumberC(9))])
   ListC([(SymbolC("define"), SymbolC("x"), NumberC(18))])
   ListC([SymbolC("if"), ListC([ SymbolC ("="), SymbolC ("x"), NumberC (16)]),
   NumberC(100), NumberC( -1)])]

*/
*/
type concreteProgramPiece =
  | NumberC(int)
  | SymbolC(string)
  | ListC(list(concreteProgramPiece));

/*
concreteProgram
a concreteProgram is a sequence of concreteProgramPieces (either NumberCs, 
SymbolCs, or ListCs). 
*/

/* Examples for concreteProgram:
   [NumberC(8)]
   [ListC([(SymbolC("define"), SymbolC("x"), NumberC(18))]), SymbolC("x")]
   [ListC([(SymbolC("+"), NumberC(2), NumberC(9))]), NumberC(8)]
   [ListC([(SymbolC("define"), SymbolC("x"), NumberC(18))]),
       ListC([(SymbolC("+"), NumberC(x), NumberC(9))])]
*/

type concreteProgram = list(concreteProgramPiece);

/* a Rackette name */
/* name
name is a representation of a name in Rackette. This can include variable 
names, such as a, or procedure names such as +
*/
/* Examples for name:
   Name("+")
   Name("define")
   Name("lambda") 
*/
type name =
  | Name(string);

/* a Rackette expression */
/* 
expression
an expression is a program that can be evaluated to produce a result. they 
include:
NumE(n), which represents number n
BoolE(b), which represents boolean b
EmptyE, which represents the empty list
NameE(n), which represents name type (above) n
AndE(a, b), which represents an and operation, where a and b are booleans
OrE(a, b), which represents an or operation, where a and b are booleans
IfE({boolExpr: a, trueExpr: b, falseExpr: c}), which represents an if
               statement in which a is the predicate and b and c are the 
               options is a is true or false, respectively
CondE(list({conditionExpr: a, resultExpr: b}...)), which represents a cond 
               statement. this houses a list of condData, 
               consisting of records of the conditionExpr, a boolean a, and the
               resultExpr, b, of that expr being true. 
LambdaE({nameList: n, lambdaBody: b}), which represents a lambda expression 
               with a list of names n and body to be evaluated, b.
LetE({letPairs: [{pairName: n, pairExpr: e}], letBody: b}), which represents a
               let expression containing a list of let pairs records with name
               n and value housed in expression e, and a body to be evaluated 
               with the value b.
ApplicationE(e), which represents a procedure-application expression containing
               a series of other expressions 
*/

/* Examples for expression:
NumE(5)
BoolE(true)
EmptyE
NameE("+")
AndE(BoolE(true), BoolE(false))
OrE(BoolE(false), BoolE(true))
IfE(ApplicationE([NameE(">"), NameE("x"), NumE(5)]), NameE(x), NumE(5))
       
LambdaE({nameList: [NameE("x")], 
         lambdaBody: ApplicationE([NameE("+"), NameE("x"), NumE("1")])})

LetE({letPairs: [NameE("x"), NumE(5)], 
      letBody: ApplicationE([NameE("+"), NameE("x"), NumE("1") })
ApplicationE([NameE(">"), NameE(x), NumE(5)])
*/

type expression =
  | NumE(int)
  | BoolE(bool)
  | EmptyE
  | NameE(name)
  | AndE(expression, expression)
  | OrE(expression, expression)
  | IfE(ifData)
  | CondE(list(condData)) 
  | LambdaE(lambdaData)
  | LetE(letData)
  | ApplicationE(list(expression))
//ifData
// if data is a record representing the contents of an if statement
// it includes a boolExpr, housing an expression, that will be evaluated.
// if this is true, it goes to the trueExpr, an expression. if it is false, it 
// goes to the falseExpr, an expression
/* Examples for ifData:
{predicate: ApplicationE([NameE(">"), NameE("x"), NumE(5)])
ifTrue: NameE("x")}
*/  
  and ifData = {
    boolExpr: expression,
    trueExpr: expression,
    falseExpr: expression,
  }
// condData
// cond data is a record representing the contents of a cond statement
// it consists of a conditionExpr, housing an expression, that acts as the 
// "true/false" statement and a resultExpr, housing an expression, with the 
// result that evaluates if the condition Expr is true. 
/* Examples for condData:
{conditionExpr: ApplicationE([NameE(">"), NameE(x), NumE(5)])
resultExpr: NumV(10)}
*/
  and condData = { 
    conditionExpr: expression, 
    resultExpr: expression,
  }
// lambdaData
// lambda data is a record representing the contents of a lambda statement.
// it consists of a list of names, a nameList, and a lambdaBody, containing an 
// expression that will be evaluated. 
/* Examples for lambdaData:
{nameList: [Name("x"), Name("y")]
lambdaBody: ApplicationE([Name("+"), Name("x"), Name("y")])}
*/
  and lambdaData = {
    nameList: list(name),
    lambdaBody: expression,
  } 
// letPair
// let pair is a record representing the contents of the list of pairs at the 
// start of a let expression. It consists of a name, pairName, and an 
// expression, pairExpression, which is the value of name
/* Examples for letPair:
{pairName: Name("x"), pairExpr: NumV(5)}
*/ 
  and letPair = {
    pairName: name, 
    pairExpr: expression,  
  }

// let data
// let data represents the interior of a let expression. it is a record and 
// contains a list of letPairs (above) and a letBody, or the expression the let
// statement evaluates to.
/* Examples for letData:
letData: {
letPairs: [{pairName: Name("x"), pairExpr: NumV(5)}, 
          {pairName: Name("y"), pairExpr: NumV(6)}],
letBody: [ApplicationE(Name("+"), Name("x"), Name("y"))]}
*/
  and letData = {
    letPairs: list(letPair),
    letBody: expression,
  }
  
/* a Rackette definition 
this contains a
name, representing the name 
expression, which can be evaluated to a value that will be assigned to name
*/
/* Examples for definition: 
 (Name("x"), NumE(20))
 (Name("y"), BoolE(false))
 */
type definition = (name, expression);


/* a piece of Rackette that can be processed:
 * either a definition or an expression */
 /* Examples for abstractProgramPiece: 
  Definition((Name("x"), NumE(20)))
  Expression(NumE(27))
  Expression(ApplicationE([NameE(Name(">=")), NameE(Name("x")), NumE(1)]))
 */
type abstractProgramPiece =
  | Definition(definition)
  | Expression(expression);

/* 
 * Data Definition:
 a representation of a Rackette program - any number of pieces
  
  Examples:
  []
  [Definition(NameE(x), NumE(3)), Expression(IfE({
    boolExpr: true,
    trueExpr: NumE(1),
    falseExpr: NumE(2),
  }))]
*/
type abstractProgram = list(abstractProgramPiece);

/* 
 * Data Definition:
 * a Rackette value: the result of evaluating a Rackette expression 
 * 
 * Examples:
 * NumV(4)
 * BoolV(false)
 * ListV([NumV(1), NumV(2)])
 * BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus})
 * ClosureV({cNameList:[Name("x")], 
 *    cExpr: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumV(3)])})
 * 
*/
type value =
  | NumV(int)
  | BoolV(bool)
  | ListV(list(value))
  | BuiltinV(builtinData)
  | ClosureV(closureData)
  and builtinData = { 
    printedRep: string,
    bProc: list(value) => value,
  }
  /* 
  * Data Definition:
  * closureData is a record of the information in a closure, composed of:
  *   cNameList: a list of names representing the formal arguments
  *   cExpr: an expression, representing the body of the procedure
  *   cEnv: the environment, representing the local environment
  *
  * Examples:
  * {cNameList:[Name("x")], 
  *   cExpr: ApplicationE([NameE(Name("+")), NameE(Name("x")), NumV(3)])}
  * {}
  */
  and closureData = {
    cNameList: list(name),
    cExpr: expression, 
    cEnv: environment,
  }
  /* Environments and bindings aren't values
     But we use "and" here so bindings have access to values
     and closures have access to environments */

  /*
  * Data Definition:
  * an environment is a list of bindings; conceptually, it represents the
  *   bindings one has access to at a particular point in evaluation.
  *
  * Examples: 
  * []
  * [((Name("+"), BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus})), 
     (Name("x"), NumV(3))]
  */
  and environment = (list(binding))

  /*
  * Data Definition:
  * a binding is a tuple of a name and a value
  *
  * Examples: 
  * ((Name("+"), BuiltinV({printedRep: "<builtin-proc-+>", bProc: plus}))
  * (Name("x"), NumV(3))
  */
  and binding = (name, value);
