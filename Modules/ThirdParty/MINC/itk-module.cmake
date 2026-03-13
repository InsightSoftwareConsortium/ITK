set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.bic.mni.mcgill.ca/ServicesSoftware/MINC\">MINC</a>
image file format library."
)

if(ITK_USE_SYSTEM_MINC)
  itk_module(
    ITKMINC
    DESCRIPTION "${DOCUMENTATION}"
    EXCLUDE_FROM_DEFAULT
    SPDX_LICENSE "LGPL-2.1-only"
    SPDX_DOWNLOAD_LOCATION "https://github.com/BIC-MNI/libminc"
    SPDX_COPYRIGHT "Copyright McConnell Brain Imaging Centre"
  )
else()
  itk_module(
    ITKMINC
    DEPENDS
      ITKHDF5
      ITKKWSys
      ITKZLIB
    DESCRIPTION "${DOCUMENTATION}"
    EXCLUDE_FROM_DEFAULT
    SPDX_LICENSE "LGPL-2.1-only"
    SPDX_DOWNLOAD_LOCATION "https://github.com/BIC-MNI/libminc"
    SPDX_COPYRIGHT "Copyright McConnell Brain Imaging Centre"
  )
endif()
