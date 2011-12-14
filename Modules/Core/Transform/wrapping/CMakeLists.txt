itk_wrap_module(ITKTransform)

set(WRAPPER_LIBRARY_GROUPS
  itkTransformBase
  itkMatrixOffsetTransformBase
  itkEuler2DTransform
  itkEuler3DTransform
  itkVersorTransform
  itkVersorRigid3DTransform
  itkSimilarity2DTransform
  itkSimilarity3DTransform
  itkAffineTransform
  itkScalableAffineTransform
  itkScaleTransform
  itkKernelTransform
)
itk_auto_load_submodules()
itk_end_wrap_module()
