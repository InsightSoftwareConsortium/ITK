set(DOCUMENTATION "This module contains the third party <a
href=\"http://www.libtiff.org/\">Tag Image File Format (TIFF)</a>
image file format library.")

itk_module(ITKTIFF
  DEPENDS
    ITKKWIML
    ITKZLIB
    ITKJPEG
  DESCRIPTION
    "${DOCUMENTATION}"
)
