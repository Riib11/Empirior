# Empirior

A simple imperitive language supporting gradual Hoare-logical verification.

## Grammar

```
  <program> ::= <statement>

  <statement> ::=
    | function <name> ([<name> : <type>]) <type> <formula> <formula> <statement>
    | predicate <name> ([<name> : <type>]) <formula>
    | assert <formula>
    | if <expression> { <statement> } else { <statement> }
    | while <expression> { <statement> }
    | fold <name> ( [<expression>] ) // fold predication
    | unfold <name> ( [<expression>] ) // unfold predication
    | <name> : <type> // variable declaration
    | <name> := <expression> // variable assignment
    | return <expression>
    | <statement> ; <statement>
    | skip

  <formula> ::=
    | ? /\ <precise-formula> // imprecise formula
    | <precise-formula>

  <precise-formula> ::=
    | <expression>
    | ~ <precise-formula> // negation
    | <precise-formula> <formula-operator> <precise-formula>
    | <name> ( [<expression>] ) // predication
    | if <expression> then <precise-formula> else <precise-formula>
    | unfolding <name> ( [<expression>] ) in <precise-formula> // unfolding predication

  <formula-operator> ::= /\ | \/

  <expression> ::=
    | <value>
    | <name> // variable
    | <expression> <expression-operator> <expression>
    | <name> ( [<expression>] )

  <expression-operator> ::=
    | + | - | *
    | > | >= | < | <= | = | !=
    | && | ||

  <value> ::=
    | unit
    | <boolean>
    | <integer>

  <type> ::=
    | Void
    | Unit
    | Boolean
    | Integer
    | Function ([<type>]) -> Type
    | Predicate ([<type>])

  <name> ::= <string>
```
