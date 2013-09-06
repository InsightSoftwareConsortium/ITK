set(DOCUMENTATION "This module contains the third party <a
href=\"http://www.bic.mni.mcgill.ca/ServicesSoftware/MINC\">MINC</a>
image file format library.")

itk_module(ITKMINC
  DEPENDS
    ITKHDF5
    ITKKWSys
    ITKZLIB
    ITKJPEG
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
