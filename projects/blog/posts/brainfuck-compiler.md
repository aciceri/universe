---
title: From Brainfuck to Python bytecode
tags: [python, compiler, brainfuck, bytecode]
date: 2019-08-27
language: en-us
---

> **Note:** When I wrote this post (and the script) the last Python version was 3.7, due of its low-level nature, the program no longer works with newer versions. However it should be simple to update it (starting from, for example, changing the magic number) or even better, using [this library](https://pypi.org/project/bytecode/).

About one month ago I wrote [this](https://gist.github.com/aciceri/913aa9667d89af8e2ab45e99e557c2aa) simple transpiler from [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) to Python bytecode. I'm going to assume you already can "program" in Brainfuck, otherwise I advise you to read the [dedicated page on Esolang](https://esolangs.org/wiki/Brainfuck), in my opinion the best place to learn about the language. I'm also assuming that you have a minimal knowledge about [stack machines](https://en.wikipedia.org/wiki/Stack_machine), personally I only used them as a theoretical objects during some proofs. I learned to use them as a real software only during the creation of this transpiler.

Understanding how to do all of this was not easy because the Python virtual machine is a moving target, I worked with Python 3.7 and I think my program should work with Python 3.6+. The best places to understand how the things work under the hood is the official [dis module documentation](https://docs.python.org/3.7/library/dis.html) and [this](https://github.com/python/cpython/blob/master/Python/ceval.c) source directly from the Python source code. I recommend to take a look at the first link and to use the second one only for consultation.

I suggest to keep an eye on the source of the program while reading this post.

In the first part of the program there are some imports and the initialization of the [cli parser](https://docs.python.org/3.7/library/argparse.html), the interesting part starts with the parse function.

```python
def parse(src):  # parse the brainfuck source
    stack = []  # to remember if inside a [...]
    endAt = {}   # correspondence between brackes [...]

    for i, char in enumerate(src):
        if char == '[':
            stack.append(i)
        elif char == ']':
            endAt[stack.pop()] = i

    def recParse(start=0, end=len(src)-1):  # recursive parser
        ast = []
        i = start

        while i < end:
            char = src[i]
            if char == '+':
                if ast != [] and isinstance(ast[-1], int):
                    ast[-1] = (ast[-1] + 1) % 256
                else:
                    ast.append(1)
            elif char == '-':
                if ast != [] and isinstance(ast[-1], int):
                    ast[-1] = (ast[-1] - 1) % 256
                else:
                    ast.append(255)
            elif char in ('>', '<', '.', ','):
                ast.append(char)
            elif char == '[':
                ast.append('[')
                ast.append(recParse(i+1, endAt[i]))
                ast.append(']')
                i = endAt[i]

            i += 1

        return ast  # return the abstract syntax tree

    return recParse()
```

That function creates the abstract syntax tree, which is a bit overkill as word because it simply returns a list of lists where the elements can be `[`, `]`, `,`, `.`, `<`, `>` or an integer ∈ [0, 256). This procedure ignores any character that isn't one of the standard 8 brainfuck commands and automatically simplify the successions of `+` and `-` in a number modulo 256. It would be possible to simplify also `<` and `>` but I was too lazy.

The visit function simply depth visits the abstract syntax tree applying the visitor function for every element of the tree.

```python
def visit(visitor, ast):  # depth visit the ast with the visitor function
    for child in ast:
        if isinstance(child, list):
            visit(visitor, child)
        else:
            visitor(child)
```

Then it's created a `bytearray` object called `instructions` containing some instructions for the stack machine (every instruction is 2 bytes long), that part is mandatory for every programs that the compiler generates, basically it contains some imports.

Instead the global variable `addresses` is a stack where the top element is the address (as index of `instructions`) of the last `[` met during the compilation.

The real compilation occurs inside `visitor`, the function itself is basically a big switch statement where different things happen depending on the element of the abstract syntax tree. The only noteworthy branches are `[` and `]`, inside the first one is annotated the address at the top of `addresses` and then 6 `NOP` instructions are added to the program so that when the in the second branch `]` is met, it's possible to change the NOP instructions to manage the `JUMP` instruction.

```python
ast = parse(source)
visit(visitor, ast)

instructions.extend([  # the last instructions for every program
    opmap['LOAD_CONST'], 0,
    opmap['RETURN_VALUE']
])

code = CodeType(
        0,  # argcount
        0,  # kwonlyargcount
        3,  # nlocals
        1000,  # stacksize
        0,  # flags
        bytes(instructions),  # codestring
        (None, *range(257), '', ('end',), arraySize, ('stdin',)),  # consts
        ('print', 'input', 'ord', 'chr', 'sys', 'stdin', 'read'),  # names
        ('array', 'pointer', 'stdin'),  # varnames
        args.outputfile,  # filename
        args.outputfile,  # name
        0,  # firstlineno
        bytes(),  # lnotab
        (),  # freevars
        ()  # cellvars
)


if args.show:
    print(dis(code))  # show the bytecode in a readable format
    exit(0)

with open(args.outputfile, 'wb+') as out:
    # printing the first 16 bytes in the file
    out.write(MAGIC_NUMBER)  # this depends on the Python version
    out.write(bytes([0] * 12))  # because of the pyc file format
    marshal.dump(code, out)

exit(0)
```

Here the parsing and the visiting (i.e. the `instructions` creation) are done, then two last instructions are added, they basically let the program return `None`. Then a `CodeType` "object" is created, it was not easy to find some documentations about this. At the end the `bytearray` is serialized and wrote to a file using the [marshal module](https://docs.python.org/3.7/library/marshal.html). This is how the `.pyc` files are structured inside.

Let's note that before writing the instructions in the file a magic number of 16 bytes is written, it depends on the Python version, so this transpiler should generate bytecode working only with the same Python version used for the interpiler execution.

The complete source code of the program is here on [Gist](https://gist.github.com/aciceri/913aa9667d89af8e2ab45e99e557c2aa).

Here you can see an usage example where I compile a Brainfuck program which prints an ascii version of a famous fractal.

{asciinema:brainfuck}

For this I have to thanks [Daniel Cristofani](http://www.hevanet.com/cristofd/brainfuck/), an insanely good Brainfuck developer who wrote programs such as the (maybe) shortest possible quine, a Brainfuck interpreter (a.k.a. [Meta-circular evaluator](https://en.wikipedia.org/wiki/Meta-circular_evaluator)) and a Brainfuck to C transpiler.

This project has a lot of possible improvements...