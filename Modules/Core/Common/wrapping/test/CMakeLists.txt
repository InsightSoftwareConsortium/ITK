# This test doesn't compile.  It exercises the bug I ran into if you multiply 2 vector images; if you
# try to compile it the compile fails.
# itk_add_test(NAME itkVectorMultiplyTest COMMAND ITKCommon2TestDriver itkVectorMultiplyTest)
if(ITK_WRAP_PYTHON)
  itk_python_add_test(NAME itkMetaDataDictionaryPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkMetaDataDictionaryTest.py)
  itk_python_add_test(NAME itkDirectoryPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkDirectoryTest.py)
  itk_python_expression_add_test(NAME itkObjectPythonTest EXPRESSION "itkObject = itk.Object.New()")
  itk_python_add_test(NAME itkIndexOffsetPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkIndexOffsetTest.py)
  itk_python_add_test(NAME itkImageDuplicatorPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkImageDuplicatorTest.py)
  itk_python_add_test(NAME itkImagePythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkImageTest.py)
  itk_python_add_test(NAME itkMatrixPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkMatrixTest.py)
  itk_python_add_test(NAME itkVariableLengthVectorPythonTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkVariableLengthVectorTest.py)
  itk_python_add_test(NAME itkPointSetSerializationTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkPointSetSerializationTest.py)
  itk_python_add_test(NAME itkSpatialOrientationAdapterTest COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/itkSpatialOrientationAdapterTest.py)

  set(test_input_dir ${CMAKE_CURRENT_LIST_DIR}/../../test/Input)
  set(test_baseline_dir ${CMAKE_CURRENT_LIST_DIR}/../../test/Baseline)

  itk_python_expression_add_test(NAME PythonInstantiateGaussianInterpolateImageFunctionPA3DSCI
    EXPRESSION "iFunc = itk.GaussianInterpolateImageFunction[itk.PhasedArray3DSpecialCoordinatesImage[itk.F], itk.D].New()"
    )

  itk_python_add_test(NAME PythonPhasedArray3DResampleNearestTest
    TEST_DRIVER_ARGS
    --compareIntensityTolerance 1
    --compare
      DATA{${test_baseline_dir}/ScanConvertPhasedArray3DITKNearestNeighborTestOutput.mha}
      ${ITK_TEST_OUTPUT_DIR}/ScanConvertPhasedArray3DITKNearestNeighborTestOutput.mha
    COMMAND itkPhasedArray3DResampleTest.py
      -i DATA{${test_input_dir}/ScanConvertPhasedArray3DTestInput.mha}
      -o ${ITK_TEST_OUTPUT_DIR}/ScanConvertPhasedArray3DITKNearestNeighborTestOutput.mha
      --azimuth-angular-separation 0.0872665
      --elevation-angular-separation 0.0174533
      --radius-sample-size 0.2
      --first-sample-distance 8.0
      --interpolation-type nearest
    )

  itk_python_add_test(NAME PythonPhasedArray3DResampleLinearTest
    TEST_DRIVER_ARGS
    --compareIntensityTolerance 1
    --compare
      DATA{${test_baseline_dir}/ScanConvertPhasedArray3DTestOutput.mha}
      ${ITK_TEST_OUTPUT_DIR}/ScanConvertPhasedArray3DTestOutput.mha
    COMMAND itkPhasedArray3DResampleTest.py
      -i DATA{${test_input_dir}/ScanConvertPhasedArray3DTestInput.mha}
      -o ${ITK_TEST_OUTPUT_DIR}/ScanConvertPhasedArray3DTestOutput.mha
      --azimuth-angular-separation 0.0872665
      --elevation-angular-separation 0.0174533
      --radius-sample-size 0.2
      --first-sample-distance 8.0
      # --interpolation-type linear  # linear is the default
    )

  itk_python_add_test(NAME PythonPhasedArray3DResampleSincTest
    TEST_DRIVER_ARGS
    --compareIntensityTolerance 1
    --compare
      DATA{${test_baseline_dir}/ScanConvertPhasedArray3DITKWindowedSincTestOutput.mha}
      ${ITK_TEST_OUTPUT_DIR}/ScanConvertPhasedArray3DITKWindowedSincTestOutput.mha
    COMMAND itkPhasedArray3DResampleTest.py
      -i DATA{${test_input_dir}/ScanConvertPhasedArray3DTestInput.mha}
      -o ${ITK_TEST_OUTPUT_DIR}/ScanConvertPhasedArray3DITKWindowedSincTestOutput.mha
      --azimuth-angular-separation 0.0872665
      --elevation-angular-separation 0.0174533
      --radius-sample-size 0.2
      --first-sample-distance 8.0
      --interpolation-type sinc
    )

endif()
