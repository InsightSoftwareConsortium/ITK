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
  GIT_REPOSITORY ${git_protocol}://github.com/KitwareMedical/ITKStrain.git
  GIT_TAG f309a9efe43f472f14de4bf376b22e99ed377b79
  )
