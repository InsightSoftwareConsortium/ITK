if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkANTSNeighborhoodCorrelationImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.ANTSNeighborhoodCorrelationImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkCorrelationImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.CorrelationImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkDemonsImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.DemonsImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkEuclideanDistancePointSetToPointSetMetricv4PythonTest
    EXPRESSION "instance = itk.EuclideanDistancePointSetToPointSetMetricv4.New()")

  itk_python_expression_add_test(NAME itkExpectationBasedPointSetToPointSetMetricv4PythonTest
    EXPRESSION "instance = itk.ExpectationBasedPointSetToPointSetMetricv4.New()")

  itk_python_expression_add_test(NAME itkImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.ImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkJensenHavrdaCharvatTsallisPointSetToPointSetMetricv4PythonTest
    EXPRESSION "instance = itk.JensenHavrdaCharvatTsallisPointSetToPointSetMetricv4.New()")

  itk_python_expression_add_test(NAME itkJointHistogramMutualInformationImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.JointHistogramMutualInformationImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkLabeledPointSetToPointSetMetricv4PythonTest
    EXPRESSION "instance = itk.LabeledPointSetToPointSetMetricv4.New()")

  itk_python_expression_add_test(NAME itkMattesMutualInformationImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.MattesMutualInformationImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkMeanSquaresImageToImageMetricv4PythonTest
    EXPRESSION "instance = itk.MeanSquaresImageToImageMetricv4.New()")

  itk_python_expression_add_test(NAME itkPointSetToPointSetMetricv4PythonTest
    EXPRESSION "instance = itk.PointSetToPointSetMetricv4.New()")

  itk_python_expression_add_test(NAME itkPointSetToPointSetMetricWithIndexv4PythonTest
    EXPRESSION "instance = itk.PointSetToPointSetMetricWithIndexv4.New()")
endif()
