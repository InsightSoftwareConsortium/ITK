itk_python_expression_add_test(NAME itkParametricPathPythonTest
      EXPRESSION "path = itk.ParametricPath[2].New()")
itk_python_expression_add_test(NAME itkPolyLineParametricPathPythonTest
      EXPRESSION "path = itk.PolyLineParametricPath[2].New()")
itk_python_expression_add_test(NAME itkImageToPathPythonTest
      EXPRESSION "PathType = itk.PolyLineParametricPath[2];
                  ImageType = itk.Image[itk.UC][2];
                  filter = itk.ImageToPathFilter[ImageType, PathType].New();")
itk_python_expression_add_test(NAME itkPathToImageFilterPythonTest
      EXPRESSION "PathType = itk.PolyLineParametricPath[2];
                  ImageType = itk.Image[itk.UC][2];
                  filter = itk.PathToImageFilter[PathType, ImageType].New();")
