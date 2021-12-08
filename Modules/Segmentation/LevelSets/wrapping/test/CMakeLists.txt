if(ITK_WRAP_PYTHON)
  set(test_input_dir ${itk-module_SOURCE_DIR}/test/Input)

  itk_python_add_test(NAME PythonLazyLoadingImage COMMAND LazyLoadingImageTest.py)

  list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
  if(ITK_WRAP_float AND wrap_2_index GREATER -1)
  # TODO: reenable the geodesic test once we get why the result is not the
  # same than with c++
  # keep just one to be sure it run, but don't compare the images

    itk_python_add_test(NAME PythonGeodesicActiveContourLeftVentricleTest
  #     --compare GeodesicActiveContourLeftVentricleTest.png
  #             DATA{${WrapITK_SOURCE_DIR}/images/GeodesicActiveContourLeftVentricleTest.png}
      COMMAND GeodesicActiveContourImageFilterTest.py
        DATA{${test_input_dir}/BrainProtonDensitySlice.png}
        ${ITK_TEST_OUTPUT_DIR}/GeodesicActiveContourLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
      )

    itk_python_add_test(NAME PythonThresholdSegmentationLevelSetVentricleTest
      TEST_DRIVER_ARGS
      --compare ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetVentricleTest.png
                DATA{Baseline/PythonThresholdSegmentationLevelSetVentricleTest.png}
      COMMAND ThresholdSegmentationLevelSetImageFilterTest.py
        DATA{${test_input_dir}/BrainProtonDensitySlice.png}
        ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetVentricleTest.png
        81 112 5 210 250
      )

    itk_python_add_test(NAME PythonThresholdSegmentationLevelSetWhiteMatterTest
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/ThresholdSegmentationLevelSetWhiteMatterTest.png
          DATA{Baseline/PythonThresholdSegmentationLevelSetWhiteMatterTest.png}
      COMMAND ThresholdSegmentationLevelSetImageFilterTest.py
        DATA{${test_input_dir}/BrainProtonDensitySlice.png}
        ${ITK_TEST_OUTPUT_DIR}/ThresholdSegmentationLevelSetWhiteMatterTest.png
        60 116 5 150 180
      )

    itk_python_add_test(NAME PythonThresholdSegmentationLevelSetGrayMatterTest
      TEST_DRIVER_ARGS
        --compare ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetGrayMatterTest.png
                  DATA{Baseline/PythonThresholdSegmentationLevelSetGrayMatterTest.png}
      COMMAND ThresholdSegmentationLevelSetImageFilterTest.py
        DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
        ${ITK_TEST_OUTPUT_DIR}/PythonThresholdSegmentationLevelSetGrayMatterTest.png
        107 69 5 180 210
      )

    itk_python_expression_add_test(NAME itkBinaryMaskToNarrowBandPointSetFilterPythonTest
      EXPRESSION "filt = itk.BinaryMaskToNarrowBandPointSetFilter.New()")
  endif()
endif()
