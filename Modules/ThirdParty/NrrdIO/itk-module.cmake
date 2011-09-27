set(DOCUMENTATION "This module contains the third party <a
href=\"http://teem.sourceforge.net/nrrd/lib.html>NRRD</a> image file format.")

itk_module(ITKNrrdIO
  DEPENDS
    ITKZLIB
  DESCRIPTION
    "${DOCUMENTATION}"
)
