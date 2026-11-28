itk_wrap_module(ITKRegistrationCommon)
set(displacement_field_adaptor_templates)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  list(
    APPEND
    displacement_field_adaptor_templates
    "${ITKM_DFTD${d}} # ${ITKT_DFTD${d}}"
  )
endforeach()
set(gaussian_displacement_field_adaptor_templates)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  list(
    APPEND
    gaussian_displacement_field_adaptor_templates
    "GSOUDFT${ITKM_D}${d} # itk::GaussianSmoothingOnUpdateDisplacementFieldTransform< ${ITKT_D}, ${d} >"
  )
endforeach()
list(
  APPEND
  displacement_field_adaptor_templates
  ${gaussian_displacement_field_adaptor_templates}
)
set(
  WRAPPER_SUBMODULE_ORDER
  itkTransformParametersAdaptorBase
  itkTransformParametersAdaptor
  itkDisplacementFieldTransformParametersAdaptor
)
itk_auto_load_and_end_wrap_submodules()
