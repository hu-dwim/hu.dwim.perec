# hu.dwim.perec

## What

It's a Common Lisp
[ORM](https://en.wikipedia.org/wiki/Object%E2%80%93relational_mapping)
(a fake object database on top of an SQL backend).

Notably, it has a
[DSL](https://en.wikipedia.org/wiki/Domain-specific_language) for
writing queries for the object graph. [This query
language](test/query) is compiled to SQL, or it may run as lisp code
if total compilation was not possible. Partial compilation is also
supported where parts of the query run in SQL, while the rest is
evaluated in lisp. It properly interacts with macros like
[Iterate](https://iterate.common-lisp.dev/), or local `macrolet`s.

It has finegrained support for pre-fetching values, and all in all it
allowed for a very slick development. With a little care it's easy to
ensure that the potentially slow queries compile all the way down to
SQL, while the occasional queries could very well be as complex as
needed and be executed on the lisp side.

## Where

At the project's [github page](https://github.com/hu-dwim/hu.dwim.perec).

## Status

This codebase has been mostly abandoned since around 2011. It was
pretty stable with the PostgreSQL backend when we last worked on it.

Davit Lichteblau and Tomas Hlavaty used it on top of Oracle
databases. Sadly, that branch has been lost.

There's a `stable` branch. The `main` branch contains a few half-baked
commits at the time of this writing.
