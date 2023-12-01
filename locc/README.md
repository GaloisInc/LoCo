# `locc`

`locc`, a pun on `cc`, `gcc`, etc., encapsulates a series of efforts to develop
tooling to reason about documents, formats, and the interplay between them
(e.g., parsing). A motivating use case of these tools was to detect cavities and
overlaps in both documents and formats, to aid format authors when designing or
debugging formats.

This work is incomplete - statically reasoning about formats in the presence of
data dependencies is a challenging problem, and we both shifted direction and
found meaningful subsets of this work to carve out.

One shift was towards on-demand, information-sharing computation - this work can
be found in [`optimal`](../optimal). Another was a region-based parsing
language, though with more limited static reasoning ability - this work can be
found in [`pear`](../pear).
