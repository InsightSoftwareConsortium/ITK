set(DOCUMENTATION "This modules contains an ImageIO class to read or write the
  MGH file format that is an integral part of FreeSurfer based tools.")

itk_module(ITKIOMGH
  DEPENDS
    ITKIOImageBase
    ITKZLIB
  TEST_DEPENDS
    ITKTestKernel
    ITKTransform
  DESCRIPTION
    "${DOCUMENTATION}"
)
