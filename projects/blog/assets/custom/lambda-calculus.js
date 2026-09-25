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
