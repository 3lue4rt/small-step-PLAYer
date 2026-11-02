# small-step-PLAYer

También referido como Player, será una página web que permita la interacción con programas de los distintos [interpretes](https://users.dcc.uchile.cl/~etanter/play-interps/index.html) presentados en el curso CC4101.

Aquí podrán ejecutar los programas paso a paso (como el debugger de DrRacket) pero de una manera más visual y menos verbosa.

## Funcionalidades

### Consola interactiva

En esta consola podrán ingresar programas para visualizar su parseo e interpretación paso a paso junto a una visualización del ambiente y storage.

```racket
Env: []
{with {foo {fun {x} {+ x x}}}
    {+ {foo 2} {foo 4}}}

Env: []
{with {foo (closureV 'x {+ x x} [])}
    {+ {foo 2} {foo 4}}}

Env: [foo => (closureV 'x {+ x x} [])]
{+ {foo 2} {foo 4}}

Env: [foo => (closureV 'x {+ x x} [])]
{foo 2}

Env: [foo => (closureV 'x {+ x x} [])]
{(closureV 'x {+ x x} []) 2}

Env: [foo => (closureV 'x {+ x x} [])]
{(closureV 'x {+ x x} []) (numV 2)}

Env: [x => (numV 2)]
{+ x x}

Env: [x => (numV 2)]
{+ (numV 2) x}

Env: [x => (numV 2)]
{+ (numV 2) (numV 2)}

Env: [x => (numV 2)]
(numV 4)

Env: [foo => (closureV 'x {+ x x} [])]
{+ (numV 4) {foo 4}}

Env: [foo => (closureV 'x {+ x x} [])]
{foo 4}

Env: [foo => (closureV 'x {+ x x} [])]
{(closureV 'x {+ x x} []) 4}

Env: [foo => (closureV 'x {+ x x} [])]
{(closureV 'x {+ x x} []) (numV 4)}

Env: [x => (numV 4)]
{+ x x}

Env: [x => (numV 4)]
{+ (numV 4) x}

Env: [x => (numV 4)]
{+ (numV 4) (numV 4)}

Env: [x => (numV 4)]
(numV 8)

Env: [foo => (closureV 'x {+ x x} [])]
{+ (numV 4) (numV 8)}

Env: [foo => (closureV 'x {+ x x} [])]
{+ (numV 4) (numV 8)}

Env: [foo => (closureV 'x {+ x x} [])]
(numV 12)
```

### Visualización como AST

Se podrá visualizar el AST del programa como un grafo para ver el **fold** ing del árbol en cada paso de ejecución