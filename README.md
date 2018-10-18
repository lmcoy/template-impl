# template-impl
This project contains different implementations of a
simple templating engine.

## Template

`Template` is representation of a template containing variables of the form `${...}`.

A variable must have the form ${name} where name can contain any character
except `'}'`. If no closing `'}'` is found for a `"${"` sequence that would start
a variable, the whole text is read as normal text. If a `'$'` without a following
`'{'` is found, it is read as normal text not as start of a variable.
Using `"$${name}"` will be read as text `"${name}"` and not as variable. A `"$$"` will
be replace by a single `'$'`.

Examples:
* `Template("hello ${s} ll ${y}").replace(Map("s" -> "test", "y" -> "test2"))  == "hello test ll test2"`
* `Template("${x}").replace(Map())  == "${x}"`
* `Template("${x}").replace(Map("x" -> "text"))  == "text"`
* `Template("$${x}").replace(Map("x" -> "test"))  == "${x}"`
* `Template("$abb").replace(Map("x" -> "test"))  == "$abb"`
* `Template("$x").replace(Map("x" -> "test"))  == "$x"`
* `Template("hello $x ll $y").replace(Map("x" -> "test", "y" -> "test2"))  == "hello $x ll $y"`
* `Template("hello $x ll $").replace(Map("x" -> "test", "y" -> "test2"))  == "hello $x ll $"`
* `Template("test ${x").replace(Map("x" -> "test"))  == "test ${x"`
* `Template("hello $$ hello").replace(Map())  == "hello $ hello"`
* `Template("hello $$$$ hello").replace(Map())  == "hello $$ hello"`

## Implementations

### TemplateReplace

Simple implementation based on `replaceAll`. This implementation does not
meet the specification.

### TemplateImperative

Imperative implementation.

### TemplateFunc1

Implementation that has no obvious mutable state.

### TemplateIO

Use a very simple implementation of an IO Monad that is not stackoverflow safe.

### TemplateIOCats

Use the IO monad from cats.



