# Contact: Dženan Zukić <dzenan.zukic@kitware.com>
itk_fetch_module(RLEImage
"Run-length encoded memory compression scheme for an itk::Image.

Insight Journal article:
http://www.insight-journal.org/browse/publication/978
http://hdl.handle.net/10380/3562

This work is supported by NIH grant R01 EB014346
'Continued development and maintenance of the ITK-SNAP 3D image segmentation software'."
  GIT_REPOSITORY ${git_protocol}://github.com/KitwareMedical/ITKRLEImage.git
  GIT_TAG d187a261ce3f53608ef49c2d42e4ce3d1e9ca678
  )
