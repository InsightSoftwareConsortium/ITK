if(ITK_WRAP_PYTHON)
  itk_python_expression_add_test(NAME itkAmoebaOptimizerv4PythonTest
  EXPRESSION "instance = itk.AmoebaOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkCommandIterationUpdatev4PythonTest
  EXPRESSION "instance = itk.CommandIterationUpdatev4.New()")

  itk_python_expression_add_test(NAME itkExhaustiveOptimizerv4PythonTest
  EXPRESSION "instance = itk.ExhaustiveOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkGradientDescentLineSearchOptimizerv4TemplatePythonTest
  EXPRESSION "instance = itk.GradientDescentLineSearchOptimizerv4Template.New()")

  itk_python_expression_add_test(NAME itkGradientDescentOptimizerv4PythonTest
  EXPRESSION "instance = itk.GradientDescentOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkLBFGSBOptimizerv4PythonTest
  EXPRESSION "instance = itk.LBFGSBOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkLBFGSOptimizerBasev4PythonTest
  EXPRESSION "instance = itk.LBFGSOptimizerBasev4.New()")

  # LBFGSOptimizerBaseHelperv4 is not wrapped with POINTER so cannot use .New()
  itk_python_expression_add_test(NAME itkLBFGSOptimizerBaseHelperv4PythonTest
  EXPRESSION "inst = itk.LBFGSOptimizerBaseHelperv4")

  itk_python_expression_add_test(NAME itkMultiGradientOptimizerv4TemplatePythonTest
  EXPRESSION "instance = itk.MultiGradientOptimizerv4Template.New()")

  itk_python_expression_add_test(NAME itkMultiStartOptimizerv4TemplatePythonTest
  EXPRESSION "instance = itk.MultiStartOptimizerv4Template.New()")

  itk_python_expression_add_test(NAME itkOnePlusOneEvolutionaryOptimizerv4PythonTest
  EXPRESSION "instance = itk.OnePlusOneEvolutionaryOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkPowellOptimizerv4PythonTest
  EXPRESSION "instance = itk.PowellOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkQuasiNewtonOptimizerv4TemplatePythonTest
  EXPRESSION "instance = itk.QuasiNewtonOptimizerv4Template.New()")

  itk_python_expression_add_test(NAME itkRegistrationParameterScalesFromIndexShiftPythonTest
  EXPRESSION "instance = itk.RegistrationParameterScalesFromIndexShift.New()")

  itk_python_expression_add_test(NAME itkRegistrationParameterScalesFromJacobianPythonTest
  EXPRESSION "instance = itk.RegistrationParameterScalesFromJacobian.New()")

  itk_python_expression_add_test(NAME itkRegistrationParameterScalesFromPhysicalShiftPythonTest
  EXPRESSION "instance = itk.RegistrationParameterScalesFromPhysicalShift.New()")

  itk_python_expression_add_test(NAME itkRegularStepGradientDescentOptimizerv4PythonTest
  EXPRESSION "instance = itk.RegularStepGradientDescentOptimizerv4.New()")

  itk_python_expression_add_test(NAME itkSingleValuedNonLinearVnlOptimizerv4PythonTest
  EXPRESSION "instance = itk.SingleValuedNonLinearVnlOptimizerv4.New()")
endif()
