# IOMeshSTL

In-tree ITK module providing read/write support for the
stereolithography (STL) mesh format, in both ASCII and binary
encodings. STL is the canonical interchange format for 3D-printing,
CAD, and surgical-planning workflows.

The flagship classes are `itk::STLMeshIO` and the `STLMeshIOFactory`
plug-in registration that lets `itk::MeshFileReader` /
`itk::MeshFileWriter` discover STL automatically.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKIOMeshSTL**](https://github.com/InsightSoftwareConsortium/ITKIOMeshSTL)
on 2026-05-04 via the v4 ingestion pipeline. The upstream repository
will be archived read-only after this PR merges; it remains
reachable at the URL above for historical reference.

## References

- Mueller D. *STL file format reader and writer for ITK.* The Insight Journal. 2014. <https://doi.org/10.54294/jdvcg7>
