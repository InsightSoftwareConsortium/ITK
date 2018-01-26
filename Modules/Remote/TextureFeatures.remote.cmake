# Contact: Jean-Baptiste VIMORT <jb.vimort@kitware.com>
itk_fetch_module(TextureFeatures
"Filters to estimate texture feature maps from N-dimensional grayscale
images. This includes first-order texture features, grey level co-occurrence
matrix (GLCM) features, and grey level run-length matrix (GLRLM) features.

For more information, see:

  Vimort J., McCormick M., Budin F., Paniagua B.
  Computing Textural Feature Maps for N-Dimensional images
  The Insight Journal. January-December. 2017.
  http://hdl.handle.net/10380/3574
  http://insight-journal.org/browse/publication/985
"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKTextureFeatures.git
  GIT_TAG d16cf18c962bd4f588d8b9db1fce42e5860f73de
  )
