Parallel B-Trees
================

B-Trees are a self-balancing search tree data structure that features fast and
consistent operations like look-ups, deletions and insertions into a data set.
They are often used in databases and ever since the growth of the internet the
amount of data that has to be handled have only grown rapidly.

This paper explores how to handle such large amounts of data using a
self-balancing search tree, namely a B-Tree implemented on a PRAM architechture
using Futhark, a pure functional language that doesn't allow for recursion but
in exchange gives potentially incredible performance if done right.

We focus on construction from a large data set and bulk-searching of such trees,
and compare sequential and parallel runtimes.

In this we also explore how to represent recursive datastructures as a flat
array in a language without recursion, as well as writing high-level performant
code in a pure language with respect to low-level architectural properties such
as thread divergence and cache coalescence.
