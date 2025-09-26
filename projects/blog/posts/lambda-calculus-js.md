---
title: Lambda calculus in JavaScript
tags: [javascript, lambda-calculus, functional]
date: 2019-03-21
language: en
---

This sunday I enjoyed creating some simple functions using only the
[lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus), I chose to use Javascript because of the simple
syntax for the lambda functions. In substance in javscript it's simple
to translate something like $\lambda x . x$ into `(x) => x`.

I started defining the boolean values true and false:

$$
T = \lambda x . \lambda y . x \qquad F = \lambda x . \lambda y . y
$$

```javascript
const T = (x) => ((y) => x)
const F = (x) => ((y) => y)
```

This explicit parenthesization is not necessary but I preferred to
exaggerate rather than making the code even more obfuscated.
I'm going to follow this choice in the whole source. The purpose of
this definition is clarified by the *if-then-else* statement:

$$
\lambda cond . \lambda a . \lambda b . cond \; a \; b
$$

```javascript
const ifThenElse = (cond) => ((a) => ((b) => cond(a)(b)))
```

The lists are created consing nodes recursively, a *node* is a *pair*
(i.e. a *cons* of two "things") where:

- if the first element of the pair is *T* then the node is *nil* (the
  empty list), at this point what is the second element of the pair is
  not relevant
- if the first element of the pair is *F* then the node is not *nil*
  and the content of the node is in its second element.

Using a lisp-like syntax what I'm saying is that the list `[1, 2, 3]`
is something like `(cons (node 1) (cons (node 2) (cons (node 3)
nil)))` where `(node a)` is `(cons F a)`. In code:

```javascript
const cons = (a) => ((b) => ((c) => c(a)(b)))
const car = (l) => l(T)
const cdr = (l) => l(F)
const nil = cons(T)(T)
const isNil = car
const node = (x) => cons(F)(x)
```

And now something a little more interesting; the natural numbers! This
construction remembers the inductive definition by Peano.

$$
\begin{aligned}
0 &= \lambda f . \lambda x . x\\
1 &= \lambda f . \lambda x . f x\\
2 &= \lambda f . \lambda x . f(f x)\\
\vdots \\
n &= \lambda f . \lambda x \; \underbrace{f(\dots f(f(}_{n} n)
\end{aligned}
$$

A number $n$ is simply something that, when called passing a function
$f$ return the composition $\underbrace{f \circ f \circ \dots \circ
f}_n$, with the convention that $f^0 = id$.

$$
succ(n) = \lambda n . \lambda f .\lambda x . f(n(f)(x))
$$

Now should be obvious what the function `succ` does. Conversely how
the arithmetic operators have been implemented may not appear such
obvious.

```javascript
const isZero = (n) => n((k) => F)(T)
const pred = (n) => ((f) => ((x) => n((g) => ((h) => h(g(f))))((u) => x)((u) => u)))

const plus = (m) => ((n) => ((f) => ((x) => m(f)(n(f)(x)))))
const prod = (m) => ((n) => ((f) => ((x) => m(n(f))(x))))
const exp = (m) => ((n) => ((f) => ((x) => n(m)(f)(x))))
const minus = (m) => ((n) => n(pred)(m))
```

I suggest to equip yourself with paper and pen, I personally had some
difficults untangling these lambdas.
A really nice place where to learn how this functions work is [this
page on wikipedia](https://en.wikipedia.org/wiki/Church_encoding).
I urge you to notice that this isn't the only possible implentation,
even continuing to use the *Church numerals* (the representation used
here for the numbers).
However, as the names say, these functions implement the addition, the
multiplication, the exponentiation and the subtraction. `isZero` is a
boolean predicate which tells if a numeral is $0$ and `pred` returns
the predecent.
I enfatize how implementing the subtraction without `pred` wouldn't
have benn easy.

The next logic operators and the comparator of numbers are easy to
understand, it's sufficient the remember what a boolean value and a
number really are.

```javascript
const not = (a) => ifThenElse(a)(F)(T)
const and = (a) => ((b) => a(b)(a))
const or = (a) => ((b) => a(a)(b))
const xor = (a) => ((b) => a(not(b))(b))

const leq = (m => ((n) => isZero(minus(m)(n))))
const eq = (m => ((n) => and(leq(m)(n))(leq(n)(m))))
const lt = (m => ((n) => and(leq(m)(n))(not(eq(m)(n)))))
```

Last but not least the the factorial function! Implemented without the
infamous [Y combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus), that should merit a whole post only for
itself. (maybe in the future)

```javascript
const fac = (n) => n(c => ((q) => q(succ(c(T)))(prod(c(T))(c(F)))))((q) => one)(F)
```

And now feel free to play with this code directly in this page, for
example you can try to calcolate the factorial of $7$ whith
`lambdaToInt(fac(intToLambda(7)))`, not bad if you consider how the
function has been defined.
I suggest to use the functions `boolToLambda`, `lambdaToBool`,
`intToLambda`, `lambdaToInt`, `listToLambda` and `lambdaToList` to
create and get boolean values, integers and lists. How do they works
is auto-explanatory, however you can find the whole source at the end
of this page.

<div id="term_demo"></div>

<script src="/js/jquery.js"></script>
<script src="/js/jquery.terminal.js"></script>
<link href="/css/jquery.terminal.css" rel="stylesheet"/>

<script>
const T = (x) => ((y) => x)
const F = (x) => ((y) => y)
const ifThenElse = (cond) => ((a) => ((b) => cond(a)(b)))

const cons = (a) => ((b) => ((c) => c(a)(b)))
const car = (l) => l(T)
const cdr = (l) => l(F)
const nil = cons(T)(T)
const isNil = car
const node = (x) => cons(F)(x)

const zero = (f) => ((x) => x)
const one = (f) => ((x) => f(x))
const two = (f) => ((x) => f(f(x)))
const succ = (n) => ((f) => ((x) => f(n(f)(x))))
const three = succ(two)
const four = succ(three)
const five = succ(four) //etc...

const isZero = (n) => n((k) => F)(T)
const pred = (n) => ((f) => ((x) => n((g) => ((h) => h(g(f))))((u) => x)((u) => u)))

const plus = (m) => ((n) => ((f) => ((x) => m(f)(n(f)(x)))))
const prod = (m) => ((n) => ((f) => ((x) => m(n(f))(x))))
const exp = (m) => ((n) => ((f) => ((x) => n(m)(f)(x))))
const minus = (m) => ((n) => n(pred)(m))

const not = (a) => ifThenElse(a)(F)(T)
const and = (a) => ((b) => a(b)(a))
const or = (a) => ((b) => a(a)(b))
const xor = (a) => ((b) => a(not(b))(b))

const leq = (m => ((n) => isZero(minus(m)(n))))
const eq = (m => ((n) => and(leq(m)(n))(leq(n)(m))))
const lt = (m => ((n) => and(leq(m)(n))(not(eq(m)(n)))))

const fac = (n) => n(c => ((q) => q(succ(c(T)))(prod(c(T))(c(F)))))((q) => one)(F)

//These functions simply help to see and create integers, booleans and lists
function lambdaToBool(b) {
  return ifThenElse(b)(true)(false);
}

function boolToLambda(b) {
  return b ? T : F;
}

function lambdaToInt(n) {
  return n((x) => x + 1)(0);
}

function intToLambda(n) {
  if(n === 0) return zero;
  else return plus(one)(n == 1 ? zero : intToLambda(n-1));
}

function listToLambda(l) {
  var nl = nil;
  for(var e of l.reverse())
    nl = cons(node(e))(nl)
  return nl;
}

function lambdaToList(l) {
  var a = [];
  while(lambdaToBool(not(isNil(car(cdr(l)))))) {
    a.push(cdr(car(l)))
    l = cdr(l)
  }
  return a;
}

        $('#term_demo').terminal(function(command) {
            if (command !== '') {
                try {
                    var result = window.eval(command);
                    if (result !== undefined) {
                        this.echo(new String(result));
                    }
                } catch(e) {
                    this.error(new String(e));
                }
            } else {
               this.echo('');
            }
        }, {
            greetings: 'Javascript Interpreter',
            name: 'js_demo',
            height: 400,
            prompt: '>>> '
        });
</script>

The complete source:

```javascript
const T = (x) => ((y) => x)
const F = (x) => ((y) => y)
const ifThenElse = (cond) => ((a) => ((b) => cond(a)(b)))

const cons = (a) => ((b) => ((c) => c(a)(b)))
const car = (l) => l(T)
const cdr = (l) => l(F)
const nil = cons(T)(T)
const isNil = car
const node = (x) => cons(F)(x)

const zero = (f) => ((x) => x)
const one = (f) => ((x) => f(x))
const two = (f) => ((x) => f(f(x)))
const succ = (n) => ((f) => ((x) => f(n(f)(x))))
const three = succ(two)
const four = succ(three)
const five = succ(four) //etc...

const isZero = (n) => n((k) => F)(T)
const pred = (n) => ((f) => ((x) => n((g) => ((h) => h(g(f))))((u) => x)((u) => u)))

const plus = (m) => ((n) => ((f) => ((x) => m(f)(n(f)(x)))))
const prod = (m) => ((n) => ((f) => ((x) => m(n(f))(x))))
const exp = (m) => ((n) => ((f) => ((x) => n(m)(f)(x))))
const minus = (m) => ((n) => n(pred)(m))

const not = (a) => ifThenElse(a)(F)(T)
const and = (a) => ((b) => a(b)(a))
const or = (a) => ((b) => a(a)(b))
const xor = (a) => ((b) => a(not(b))(b))

const leq = (m => ((n) => isZero(minus(m)(n))))
const eq = (m => ((n) => and(leq(m)(n))(leq(n)(m))))
const lt = (m => ((n) => and(leq(m)(n))(not(eq(m)(n)))))

const fac = (n) => n(c => ((q) => q(succ(c(T)))(prod(c(T))(c(F)))))((q) => one)(F)

//These functions simply help to see and create integers, booleans and lists
function lambdaToBool(b) {
  return ifThenElse(b)(true)(false);
}

function boolToLambda(b) {
  return b ? T : F;
}

function lambdaToInt(n) {
  return n((x) => x + 1)(0);
}

function intToLambda(n) {
  if(n === 0) return zero;
  else return plus(one)(n == 1 ? zero : intToLambda(n-1));
}

function listToLambda(l) {
  var nl = nil;
  for(var e of l.reverse())
    nl = cons(node(e))(nl)
  return nl;
}

function lambdaToList(l) {
  var a = [];
  while(lambdaToBool(not(isNil(car(cdr(l)))))) {
    a.push(cdr(car(l)))
    l = cdr(l)
  }
  return a;
}
```