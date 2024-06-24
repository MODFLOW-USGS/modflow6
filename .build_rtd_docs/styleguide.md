# Fortran Style Guide

The goal of this guide is to provide a standard for MODFLOW 6 contributors to follow, in pursuit of consistent, readable, well-organized, and unsurprising source code.

MODFLOW 6 is written in Fortran &mdash; largely Fortran 2003, with sparse use of a few 2008 language features. This guide assumes familiarity with the Fortran language.

## Amendments

Suggestions to change or extend the style conventions are welcome. Suggestions should be accompanied by a good case for the change. Bear in mind that a goal of this guide is to minimize time spent thinking about questions of style.

## Conventions

```{contents} Table of Contents
:local:
```

### Modules
* __Use `implicit none` in all modules.__
  > In old FORTRAN variables types were implicitly assigned to variables depending on the variable name.
  > It easy to create a bug using this old way because the developer is responsible of keeping track of what type each variable is. Using `implicit none` forces the developer to explicitly assign a type to a variable. This makes it possible to catch certain mistakes during the compilation step e.g. unintended type conversion
* __Make modules `private` by default. Mark `public` types and procedures explicitly.__
  > Instead of declaring everything public it is better to have it declared private by default and explicitly define what is public.
  > This makes it clear to whom is using your module which types and procedures they may use. 
  > 
  > Furthermore as the developer of the module it also makes it clear which procedures are internal and you can change without affecting any external code. On the other hand if its public changes should be made with care.
* __Prefer explicitly specified imports with `use ..., only: ...`, rather than merely `use ...`.__
  > When only using the `use ...` syntax you pull in everything a module defines. This can cause name collision. Therefore always use the `use ..., only: ...` syntax

### Types

### Procedures
* Avoid `implicit none` in procedures except where necessary (e.g. interface bodies).
* Don't end procedures with a `return` statement; use `return` only to return early.
* Avoid `goto` statements.
* Don't use implicit dummy arguments or local variables.
* Specify precision for logicals, integers and reals with the data types defined in `src/Utilities/kind.f90`.
* Name type-bound procedures' first dummy argument `this`. A suitable docstring is `!< this instance`.
* Avoid deeply nested control structures where possible.
* Prefer verbose names, except where the meaning is obvious from context or precedent. E.g., well-known index variables (`i`, `j`, `m`, `n`).
* * Use named constants. Avoid [magic numbers](https://en.wikipedia.org/wiki/Magic_number_(programming)).

### Naming
* Use `CamelCase` for source file names.
* Use `CamelCase` for module and derived type names.
* Use a noun or noun phrase for module and derived type names.
* Use `snake_case` for procedure names.
* Use a verb or verb phrase for procedure names.
* End module names with `...Module`.
* Derived type names may, but need not, end with `...Type`.
* If a source file exists primarily to host a module, name the source file and module identically, except for trailing `...Module`.
* If a module exists primarily to host a type, name the module and type identically, except for trailing `...Module` and `...Type`.
* Include the module/subroutine/function name in `end module`, `end subroutine` and `end function` statements.

### Comments
* Avoid empty comments.
* Avoid comments starting with `--`.

### Formatting
* Include blank lines between:
  * module declaration and `use...` statements
  * `use...` statements and procedure declarations
  * derived type declaration and member variables
  * member variables and `contains` statements
* Prefer importing items used throughout a module with a module-scoped `use` statement, rather than separately in multiple procedures.
* Use [Doxygen format](https://www.doxygen.nl/manual/docblocks.html#fortranblocks) for docstrings. For dummy arguments, use either `@param ...` above the signature or `!< ...` next to the dummy argument.

## Sample Module

Below is a minimal module demonstrating some (but not all) of the conventions.

```f90
module SampleModule

  use KindModule, only: DP, I4B, LGP
  use ConstantsModule, only: DPI

  implicit none
  private
  public :: run_sample

contains

  subroutine run_sample(verbose)
    logical(LGP), intent(in) :: verbose
    integer(I4B) :: answer
    real(DP) :: pi

    answer = 42
    pi = DPI

    if (verbose) then
      print *, 'pi: ', pi
      print *, 'answer to the ultimate question of life,'&
               ' the universe, and everything: ', answer
    end if
  end subroutine run_sample

end module SampleModule
```
