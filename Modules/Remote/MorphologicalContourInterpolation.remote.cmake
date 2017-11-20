# Contact: Dženan Zukić <dzenan.zukic@kitware.com>
itk_fetch_module(MorphologicalContourInterpolation
"nD morphological contour interpolation

Insight Journal article:
http://www.insight-journal.org/browse/publication/977
http://hdl.handle.net/10380/3563

An ITK-based implementation of morphological contour interpolation based off the paper:
Albu AB, Beugeling T, Laurendeau D.
'A morphology-based approach for interslice interpolation of anatomical slices from volumetric images.'
IEEE Trans Biomed Eng. 2008 Aug;55(8):2022-38. doi: 10.1109/TBME.2008.921158.

This work is supported by NIH grant R01 EB014346
'Continued development and maintenance of the ITK-SNAP 3D image segmentation software'."
  GIT_REPOSITORY ${git_protocol}://github.com/KitwareMedical/ITKMorphologicalContourInterpolation.git
  GIT_TAG 608853faf597c38132eaff8586f1f9c5c31f70aa
  )
