To build new Windows ICU packages, see [MSVCBuildICU](https://github.com/InsightSoftwareConsortium/MSVCBuildICU)

It should have no trouble building a version of DCMTK libraries linkable to
ITK.

The vendored DCMTK source is pinned by `DCMTK_GIT_TAG` in
[`DCMTKGitTag.cmake`](DCMTKGitTag.cmake), which references a
`for/itk-dcmtk-*` branch of the
[InsightSoftwareConsortium/DCMTK](https://github.com/InsightSoftwareConsortium/DCMTK)
fork. The fork's `welcome` branch documents the update workflow; see also
[Third-Party Fork Conventions](../../../Documentation/Maintenance/ThirdPartyForkConventions.md).
