set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.libpng.org/pub/png/libpng.html/\">Portable Network Graphics
(PNG)</a> image file format library."
)

if(ITK_USE_SYSTEM_PNG)
  itk_module(ITKPNG DESCRIPTION "${DOCUMENTATION}")
else()
  itk_module(ITKPNG DEPENDS ITKZLIB DESCRIPTION "${DOCUMENTATION}")
endif()
