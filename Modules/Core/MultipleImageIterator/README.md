# MultipleImageIterator

In-tree ITK module providing `itk::MultipleImageIterator`, a single
iterator that walks any number of co-registered images in lockstep.
The iterator returns a `std::vector` of pixel values (or references)
at each position, simplifying multi-channel processing without the
boilerplate of synchronizing N separate iterators.

## Origin

Ingested from the standalone remote module
[**KitwareMedical/MultipleImageIterator**](https://github.com/KitwareMedical/MultipleImageIterator)
on 2026-05-12, following the v4 ingestion guidelines defined in
InsightSoftwareConsortium/ITK#6204. Placed under `Modules/Core/` to
match the placement of other ITK iterators (`itkImageRegionIterator`,
`itkNeighborhoodIterator`). The upstream repository will be archived
read-only after this PR merges; it remains reachable at the URL above
for historical reference.

## References

- Schaerer J. *A MultipleImageIterator for iterating over multiple
  images simultaneously.* The Insight Journal. December 2014.
  <https://doi.org/10.54294/e5lmyz>
