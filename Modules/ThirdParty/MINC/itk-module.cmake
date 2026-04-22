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
    SPDX_LICENSE
    "LGPL-2.1-only"
    SPDX_VERSION
    "2.4.06"
    SPDX_DOWNLOAD_LOCATION
    "https://github.com/BIC-MNI/libminc"
    SPDX_COPYRIGHT
    "Copyright McConnell Brain Imaging Centre"
    SPDX_PURL
    "pkg:github/BIC-MNI/libminc@2.4.06"
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
    SPDX_LICENSE
    "LGPL-2.1-only"
    SPDX_VERSION
    "2.4.06"
    SPDX_DOWNLOAD_LOCATION
    "https://github.com/BIC-MNI/libminc"
    SPDX_COPYRIGHT
    "Copyright McConnell Brain Imaging Centre"
    SPDX_PURL
    "pkg:github/BIC-MNI/libminc@2.4.06"
  )
endif()
