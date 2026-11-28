set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"https://libtiff.gitlab.io/libtiff\">Tag Image File Format (TIFF)</a>
image file format library."
)

if(ITK_USE_SYSTEM_TIFF)
  itk_module(ITKTIFF DESCRIPTION "${DOCUMENTATION}")
else()
  itk_module(
    ITKTIFF
    PRIVATE_DEPENDS
      ITKZLIB
      ITKJPEG
    DESCRIPTION "${DOCUMENTATION}"
  )
endif()
