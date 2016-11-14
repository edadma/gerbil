![alt text](https://github.com/edadma/gerbil/blob/master/logo5-200.jpg)

The Gerbil Programming Language
===============================

*gerbil* is a small programming language that tends to result in very compact code. It is a Polish (prefix) notation language where the need for parentheses and brackets, as they are normally used has been eliminated. Parentheses and brackets do appear in the language but they are not used for grouping.


Examples
--------

Here are some examples of what the language looks like.


### Example 1: The Obligatory "Hello World" Program

	."Hello World!"

	
### Example 2: The Factorial Function

	=:fact->1/.`*..1%1$.@fact5
	
Here's the same example with spaces and comments added. Admittedly, this is not the complete definition because it's missing the fact (by definition) that 0! = 1.

	=: fact        ## assign the function to variable 'fact'
	  -> 1         ## define lambda function that takes 1 argument
	    /.         ## reduce a sequence to a single value by applying an operator to each element
	      `*       ## the multiplication operator as an object (section) to be used by reduce (/.)
	      ..1 %1   ## sequence of integers from 1 to whatever the argument is
	    $          ## end function
	
	. @fact 5      ## print the result of applying 'fact' to the number 5

This program prints `120` to standard out.


Operators
---------

*gerbil* programming revolves around the idea of an *expression*. An expression is a stretch of code that produces a value. The code being executed is made up of *operators* which generally do not need to be separated by spaces, as the above factorial function example shows. An operator can be thought of as a function that takes arguments and produces a result. Numbers and variables are also considered special operators that don't take any arguments.

A number of conventions that are used in the operator table below are explained in the following table.

Convention | Meaning
---------- | -------
*i*        | expression evaluating to an integer
*b*        | expression evaluating to a boolean
*c*        | expression evaluating to a character string
*f*[/arity]| expression evaluating to a function, whether defined or a built-in operator, possibly requiring a specific arity (number of parameters)
*n*        | expression evaluating to a number
*s*        | expression evaluating to a (Scala) sequence
*v*        | the name of a variable
*e*        | any expression whatsoever
*l*        | expression evaluating to a (Scala) list
*v* = *e*  | *v* gets the value *e*
*e* :: *l* | a Scala list where *e* is the head and *l* is the tail
Nil        | the Scala list terminator
\<number\> | string of decimal digits denoting a number literal
\<alpha\>  | string of alpha (letter) characters denoting a variable
...        | arbitrary string of characters, not including new-line
N/A        | the column is not applicable to the symbol
           | a blank in the 'input' column just means no input, but is not the same as N/A because the symbol is considered to be a normal operator
\<inputs\> | inputs, if any, to a function or operator that's being called indirectly
\<output>\ | output to a function or operator that's being called indirectly
\<symbol>  | string of symbol (non-alphanumeric and non-space) characters

The following table explains the built-in operator symbols. Some of the operators have multiple meanings depending of the type of arguments they are presented with, so the same operator can appear more than once with different meanings. In the 'example' and 'result' columns, assume variable `a` is equal to 5.

Symbol       | Input(s)       | Output                 | Meaning            | Example                | Result
--------     | ---------      | ------                 | -------            | -------                | ------
\<number\>   |                | the number \<number\>  | integer literal    | `123`                  | 123
\<alpha\>    |                | the variable \<alpha\> | variable           | `a`                    | 5
`"`...`"`    |                | the string ...         | string literal     | `"abc"`                | "abc"
`##`...      | N/A            | N/A                    | comment            | `## does nothing`      | N/A
`+`          | *n*<sub>1</sub>&nbsp;*n*<sub>2</sub> | *n*<sub>1</sub> + *n*<sub>2</sub> | addition | `+ 3 4`              | 7
`+`          | *c*<sub>1</sub>&nbsp;*c*<sub>2</sub> | *c*<sub>1</sub> joined to *c*<sub>2</sub> | concatenation | `+ "ab" "cd"`        | "abcd"
`+:`         | *v*            | *v*                    | *v* = *v* + 1      | `+: a`                 | 5
`+.`         | *v*            | *v* + 1                | *v* = *v* + 1      | `+. a`                 | 6
`-`          | *n*<sub>1</sub>&nbsp;*n*<sub>2</sub> | *n*<sub>1</sub> - *n*<sub>2</sub> | subtraction | `- 5 3`              | 2
`-:`         | *v*            | *v*                    | *v* = *v* - 1      | `-: a`                 | 5
`-.`         | *v*            | *v* - 1                | *v* = *v* - 1      | `-. a`                 | 4
`~`          | *n*            | -*n*                   | unary negation     | `~a`                   | -5
`*`          | *n*<sub>1</sub>&nbsp;*n*<sub>2</sub> | *n*<sub>1</sub> * *n*<sub>2</sub> | multiplicationtion | `* 3 4`              | 12
`*`          | *c* *i*        | *c* repeated *i* times | repeat string      | `"a"*5`                | "aaaaa"
`/`          | *n*<sub>1</sub>&nbsp;*n*<sub>2</sub> | *n*<sub>1</sub> / *n*<sub>2</sub> | division | `4/2`; `3/2`; `3.0/2`| 2 (integer); 3/2 (rational); 1.5
`^`          | *n*<sub>1</sub>&nbsp;*n*<sub>2</sub> | *n*<sub>1</sub><sup>*n*<sub>2</sub></sup> | exponentiation | `3^4`                | 81
`=:`         | *v*&nbsp;*e*   | ()                     | *v* = *e*          | `=: b 123`             | () (with the side effect of assigning 123 to 'b')
`?`          | *b*            | N/A                    | start conditional  | <code>?<3a&nbsp;"y"&nbsp;:&nbsp;"n"?.</code>   | "y"
`:`          | N/A            | N/A                    | 'else' part        | `?>3a "y" : "n"?.`     | "n"
`?.`         | N/A            | previous expression    | end conditional    | see above              | N/A
`,`          | *e*&nbsp;*l*   | *e* :: *l*             | list constructor   | `,1,2,3;`              | List(1, 2, 3) (using Scala syntax)
`;`          |                | Nil                    | end or empty list  | `;`                    | List() (empty list using Scala syntax)
`.`          | *e*            | ()                     | print *e* with \n  | `."this gets printed"` | () (with the side effect of printing "this gets printed\n")
`.:`         | *e*            | ()                     | print *e*          | `.:"no new-line"`      | () (with the side effect of printing "no new-line")
`@`          | *f*&nbsp;\<inputs> | \<output>          | apply *f* to \<inputs> | <code>=:&nbsp;f&nbsp;->2&nbsp;+%1%2$&nbsp;@f&nbsp;1&nbsp;2</code> | 3
`#`          | *s*            | length of *s*          | sequence length    | `#,1,2,3;`             | 3
`->`         | *n*            | N/A                    | lambda function    | see `@` example        | see `@` example
`%`          | *n*            | argument *n*           | get argument       | see `@` example        | see `@` example
`%:`         |                | arguments sequence     | get all arguments  | `@->3%:$1 2 3`         | List(1, 2, 3)
`%%`         | *n*            | outer argument *n*     | get outer argument |                        |
`%%%`        | *n*            | 2nd outer argument *n* | get outer argument |                        |
`$`          | N/A            | previous expression    | end lambda function| see `@` example        | see `@` example
`` ` ``      | \<symbol>      | operator for \<symbol> | get operator       | ``/.`+,1,2,3;``        | 6
`/.`         | *f*/2 *s*      | *s* reduced using *f*/2| left reduce        | see `` ` `` example    | see `` ` `` example