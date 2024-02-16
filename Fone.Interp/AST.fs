module Fone.Interp.AST

open TokenParser

module Unchecked =
    type ArgInfo =
        { Name: string; TypeConstraint: string option }
    type Expression =
        | Let of ArgInfo * Expression
        | Call of callee: Expression * args: Expression list
        | Range of from: Expression * until: Expression
        | Ident of string
        | Throw of Token
        | Sequence of Expression list
        | Number of string
        | ForLoop of bindings: Expression list * range: Expression * body: Expression
        | Ignore
        | UnitConstant
        | ArrayLiteral of Expression list
        | Lambda of arg: ArgInfo * body: Expression
        | Assign of dest: Expression * value: Expression
        | NonCurriedLambda of args: ArgInfo list * body: Expression
        | RecordInfo of fields: (Expression * Expression) list
