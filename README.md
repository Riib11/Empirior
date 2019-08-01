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
    | fold <name> ( [<expression>] )
    | unfold <name> ( [<expression>] )
    | <name> : <type>
    | <name> := <expression>
    | skip
    | return <expression>
    | <statement> ; <statement>

  <formula> ::=
    | ? /\ <precise-formula>
    | <precise-formula>

  <precise-formula> ::=
    | <expression>
    | ~ <precise-formula>
    | <precise-formula> <formula-operator> <precise-formula>
    | <name> ( [<expression>] )
    | if <expression> then <precise-formula> else <precise-formula>
    | unfolding <name> ( [<expression>] ) in <precise-formula>

  <formula-operator> ::= /\ | \/

  <expression> ::=
    | <value>
    | <variable>
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
