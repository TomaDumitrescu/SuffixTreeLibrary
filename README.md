Copyright 2024 Toma-Ioan Dumitrescu

## Description

The project consists in representating suffix trees in Racket and
performing basic functions on them that further apply in fast
implementations of string operations. The ST representation
is actually a list of nested lists, but the ST functions do
not depend on this. So, changing the constructors from
suffix-tree.rkt for a new ST representation will not
require redefining common-functions.rkt or
suffix-tree-applications.rkt.
