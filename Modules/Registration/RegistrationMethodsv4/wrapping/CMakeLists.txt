itk_wrap_module(ITKRegistrationMethodsv4)

set(WRAPPER_SUBMODULE_ORDER
   itkImageRegistrationMethodv4
   itkSyNImageRegistrationMethod
   itkBSplineSyNImageRegistrationMethod
   itkTimeVaryingVelocityFieldImageRegistrationMethodv4
   itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod
)

# was getting custom rule already exists errors if below was included
itk_auto_load_and_end_wrap_submodules()
