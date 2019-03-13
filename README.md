# hawkek

OWL validator. Grammar description:
```antlr
<letters> ::= [a-zA-Z]+

<program> ::= <statement> <opt-program>
<opt-program> ::= <doubleNextLine> <program> | ""
<doubleNextLine> ::= "\n\n"
<statement> ::= <subClassOf>|<disjointClasses>|<sameIndividual>|<differentIndividuals>|<equivalentClasses>

<subClassOf> ::= "SubClassOf" "(" <classExpression> <classExpression> ")"
<equivalentClasses> ::= "EquivalentClasses" "(" <classExpression> <classExpression> <opt-classExpressions> ")"
<disjointClasses> ::= "DisjointClasses " "(" <classExpression> <classExpression> <opt-classExpressions> ")"
<sameIndividual> ::= "SameIndividual " "(" <identifier> <identifier> <opt-identifiers> ")"
<differentIndividuals> ::= "DifferentIndividuals " "(" <identifier> <identifier> <opt-identifiers> ")"

<classExpression> ::= <identifier>|<objectIntersectionOf>|<objectUnionOf>|<objectComplementOf>|<objectOneOf>
<opt-classExpressions> = <classExpression> <opt-classExpressions> | ""
<identifier> ::= ":"<letters>
<opt-identifiers> ::= <identifier> <opt-identifiers> | ""

<objectIntersectionOf> ::= "ObjectIntersectionOf" "(" <classExpression> <classExpression> <opt-classExpressions> ")"
<objectUnionOf> ::= "ObjectUnionOf" "(" <classExpression> <classExpression> <opt-classExpressions> ")"
<objectComplementOf> ::= "ObjectComplementOf" "(" <classExpression> ")"
<ObjectOneOf> ::= "ObjectOneOf" "(" <identifier> <opt-identifiers> ")"
```

To test run command: `docker-compose run --rm app ./Main.hs "owl_test.rdf"`
