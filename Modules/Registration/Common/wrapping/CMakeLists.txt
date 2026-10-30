itk_wrap_module(ITKRegistrationCommon)
set(displacement_field_adaptor_templates)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  list(
    APPEND
    displacement_field_adaptor_templates
    "${ITKM_DFTD${d}} # ${ITKT_DFTD${d}}"
  )
endforeach()
itk_auto_load_and_end_wrap_submodules()
