itk_wrap_module(VariationalRegistration)

set(
  WRAPPER_SUBMODULE_ORDER
  itkContinuousBorderWarpImageFilter
  itkVariationalRegistrationFunction
  itkVariationalRegistrationDemonsFunction
  itkVariationalRegistrationNCCFunction
  itkVariationalRegistrationFastNCCFunction
  itkVariationalRegistrationSSDFunction
  itkVariationalRegistrationRegularizer
  itkVariationalRegistrationCurvatureRegularizer
  itkVariationalRegistrationDiffusionRegularizer
  itkVariationalRegistrationElasticRegularizer
  itkVariationalRegistrationGaussianRegularizer
  itkVariationalRegistrationFilter
  itkVariationalDiffeomorphicRegistrationFilter
  itkVariationalSymmetricDiffeomorphicRegistrationFilter
  itkVariationalRegistrationMultiResolutionFilter
  itkVariationalRegistrationStopCriterion
)

# Displacement fields must have as many components as the image has dimensions.
# Macros, not functions: itk_wrap_template appends to the caller's variables.
macro(variational_registration_wrap_images_and_field)
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    foreach(s ${WRAP_ITK_REAL})
      foreach(v ${WRAP_ITK_VECTOR_REAL})
        itk_wrap_template(
          "${ITKM_I${s}${d}}${ITKM_I${s}${d}}${ITKM_I${v}${d}${d}}"
          "${ITKT_I${s}${d}}, ${ITKT_I${s}${d}}, ${ITKT_I${v}${d}${d}}"
        )
      endforeach()
    endforeach()
  endforeach()
endmacro()

macro(variational_registration_wrap_field dimensions)
  foreach(d ${dimensions})
    foreach(v ${WRAP_ITK_VECTOR_REAL})
      itk_wrap_template("${ITKM_I${v}${d}${d}}" "${ITKT_I${v}${d}${d}}")
    endforeach()
  endforeach()
endmacro()

itk_auto_load_submodules()
itk_end_wrap_module()
