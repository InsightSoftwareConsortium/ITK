itk_python_expression_add_test(NAME itkSingleValuedNonLinearVnlOptimizerv4PythonTest
EXPRESSION "instance = itk.SingleValuedNonLinearVnlOptimizerv4.New()")

itk_python_expression_add_test(NAME itkLBFGSOptimizerBasev4PythonTest
EXPRESSION "instance = itk.LBFGSOptimizerBasev4.New()")

# LBFGSOptimizerBaseHelperv4 is not wrapped with POINTER so cannot use .New()
itk_python_expression_add_test(NAME itkLBFGSOptimizerBaseHelperv4PythonTest
EXPRESSION "inst = itk.LBFGSOptimizerBaseHelperv4")

itk_python_expression_add_test(NAME itkLBFGSBOptimizerv4PythonTest
EXPRESSION "instance = itk.LBFGSBOptimizerv4.New()")
