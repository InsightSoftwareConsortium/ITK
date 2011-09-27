set(DOCUMENTATION "This module contains the third party <a
href=\"http://www.libpng.org/pub/png/libpng.html/\">Portable Network Graphics
(PNG)</a> image file format library.")

itk_module(ITKPNG
  DEPENDS
    ITKZLIB
  DESCRIPTION
    "${DOCUMENTATION}"
)
