itk_python_expression_add_test(NAME itkStrainImageFilterTestPython
  EXPRESSION "instance = itk.StrainImageFilter[itk.Image[itk.Vector[itk.D,2],2],itk.D,itk.D].New()")
itk_python_expression_add_test(NAME itkTransformToStrainFilterTestPython
  EXPRESSION "instance = itk.TransformToStrainFilter[itk.Transform[itk.D,3,3]].New()")
