# IOMeshSWC

In-tree ITK module for reading and writing meshes stored in the SWC
file format, a widely used representation of neuron morphology
(reconstructed dendritic and axonal trees). The module registers an
`itk::SWCMeshIO` factory so SWC files are handled transparently by
`itk::MeshFileReader` and `itk::MeshFileWriter`.

## Origin

Ingested from the standalone remote module
[**InsightSoftwareConsortium/ITKIOMeshSWC**](https://github.com/InsightSoftwareConsortium/ITKIOMeshSWC)
via the v4 ingestion pipeline. The upstream repository is archived
read-only after this ingest; it remains reachable at the URL above for
historical reference.
