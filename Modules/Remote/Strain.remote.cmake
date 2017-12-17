# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(Strain
"Filters to estimate a strain tensor field from a displacement field or a
spatial transformation.

For more information, see:

  McCormick M.
  N-Dimensional Computation of Strain Tensor Images in the Insight Toolkit
  The Insight Journal. January-December. 2017.
  http://hdl.handle.net/10380/3573
  http://insight-journal.org/browse/publication/984
"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKStrain.git
  GIT_TAG 337f5c26efaf851a06e24beb6d8dbdd3f21f9853
  )
