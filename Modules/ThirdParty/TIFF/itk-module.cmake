set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.libtiff.org/\">Tag Image File Format (TIFF)</a>
image file format library."
)

if(ITK_USE_SYSTEM_TIFF)
  itk_module(ITKTIFF DESCRIPTION "${DOCUMENTATION}")
else()
  itk_module(
    ITKTIFF
    DEPENDS
      ITKZLIB
      ITKJPEG
    DESCRIPTION "${DOCUMENTATION}"
  )
endif()
