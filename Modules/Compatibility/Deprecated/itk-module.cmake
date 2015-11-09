set(DOCUMENTATION "This is a collection of classes that are intended to be
removed from the toolkit.")

if(NOT ITKV3_COMPATIBILITY ) ## Deprecated code only work with ITKV3_COMPATIBILITY
  set(EXCLUDE_DEPRECATED_LEGACY_CODE "EXCLUDE_FROM_DEFAULT")
else()
  set(EXCLUDE_DEPRECATED_LEGACY_CODE "")
endif()
itk_module(ITKDeprecated
  PRIVATE_DEPENDS
    ITKCommon
    ITKDICOMParser
    ITKIOGDCM
    ITKIOImageBase
    ITKNIFTI
    ITKZLIB
  COMPILE_DEPENDS
    ITKV3Compatibility      ## Note: Deprecated requires the ITKV3 compatibility layer
    ITKMesh
  TEST_DEPENDS
    ITKTestKernel
    ITKDICOMParser
    ITKGDCM
    ITKIOBioRad
    ITKIOLSM
    ITKIOStimulate
    ITKRegistrationCommon
  ${EXCLUDE_DEPRECATED_LEGACY_CODE}
  DESCRIPTION
    "${DOCUMENTATION}"
)
