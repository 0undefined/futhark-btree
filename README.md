Parallel B-Trees
================

## Project Description

B-Trees are a self-balancing search tree data structure that features fast and
consistent operations like lookups, deletions and insertions into a dataset.
They are often used in databases and since the growth of the internet the amount
of data that has to be handled have only grown rapidly.

In order to handle the large amount of data I wish to explore the possibility of
using General Purpose Processing Units (GPGPU) to achieve fast parallel
operations using a B-Tree data structure in Futhark, using a parallel split and
parallel join operations as described in the paper by Akhremtsev and Sanders
"Fast Parallel Operations on Search Trees".

By doing this I wish to learn the following:
* Explore how to represent a recursive datastructure as a flat array in a
  language without recursion.
* Implementation of operations with an expected asymptotic complexity in a pure
  language.
* Write high level performant code in a function language with respect to
  low-level architechtural properties, such as thread divergence and cache
  coalescence.
