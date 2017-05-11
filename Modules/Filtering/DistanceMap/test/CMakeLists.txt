itk_module_test()
set(ITKDistanceMapTests
itkDanielssonDistanceMapImageFilterTest.cxx
itkDanielssonDistanceMapImageFilterTest1.cxx
itkDanielssonDistanceMapImageFilterTest2.cxx
itkSignedDanielssonDistanceMapImageFilterTest.cxx
itkSignedDanielssonDistanceMapImageFilterTest1.cxx
itkSignedDanielssonDistanceMapImageFilterTest2.cxx
itkContourMeanDistanceImageFilterTest.cxx
itkContourDirectedMeanDistanceImageFilterTest.cxx
itkFastChamferDistanceImageFilterTest.cxx
itkHausdorffDistanceImageFilterTest.cxx
itkReflectiveImageRegionIteratorTest.cxx
itkSignedMaurerDistanceMapImageFilterTest.cxx
itkApproximateSignedDistanceMapImageFilterTest.cxx
itkIsoContourDistanceImageFilterTest.cxx
itkSignedMaurerDistanceMapImageFilterTest11.cxx
itkSignedDanielssonDistanceMapImageFilterTest11.cxx
)

CreateTestDriver(ITKDistanceMap  "${ITKDistanceMap-Test_LIBRARIES}" "${ITKDistanceMapTests}")

itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest11
      COMMAND ITKDistanceMapTestDriver itkSignedMaurerDistanceMapImageFilterTest11)

itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest11
      COMMAND ITKDistanceMapTestDriver itkSignedDanielssonDistanceMapImageFilterTest11)

itk_add_test(NAME itkDanielssonDistanceMapImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkDanielssonDistanceMapImageFilterTest)
itk_add_test(NAME itkDanielssonDistanceMapImageFilterTest1
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDanielssonDistanceMapImageFilterTest1.mhd,itkDanielssonDistanceMapImageFilterTest1.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkDanielssonDistanceMapImageFilterTest1.mhd
    itkDanielssonDistanceMapImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/BinaryImageWithVariousShapes01.png} ${ITK_TEST_OUTPUT_DIR}/itkDanielssonDistanceMapImageFilterTest1.mhd)
itk_add_test(NAME itkDanielssonDistanceMapImageFilterTest2
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkDanielssonDistanceMapImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkDanielssonDistanceMapImageFilterTest2.png
    itkDanielssonDistanceMapImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/BinaryImageWithVariousShapes01.png} ${ITK_TEST_OUTPUT_DIR}/itkDanielssonDistanceMapImageFilterTest2.png)
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkSignedDanielssonDistanceMapImageFilterTest)
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest1
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedDanielssonDistanceMapImageFilterTest1.mhd,itkSignedDanielssonDistanceMapImageFilterTest1.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest1.mhd
    itkSignedDanielssonDistanceMapImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/BinaryImageWithVariousShapes01.png} ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest1.mhd 2)
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest2
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedDanielssonDistanceMapImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest2.png
    itkSignedDanielssonDistanceMapImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/BinaryImageWithVariousShapes01.png} ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest2.png)
# Test the distance filter on a 3D volume.
# The output should be the same as the output of the Maurer distance map filter.
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest3D
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest3D.mhd,itkSignedMaurerDistanceMapImageFilterTest3D.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest3D.mhd
    itkSignedDanielssonDistanceMapImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/BinarySquare3D.mhd,BinarySquare3D.zraw} ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest3D.mhd 3)
# Test the distance filter on a 4D volume.
# The middle 3D volume of the output 4D volume should be exactly the same as the
# 3D volume of the previous test.
# The output 4D volume should be the same as the output of the Maurer distance map filter.
itk_add_test(NAME itkSignedDanielssonDistanceMapImageFilterTest4D
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest4D.mhd,itkSignedMaurerDistanceMapImageFilterTest4D.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest4D.mhd
    itkSignedDanielssonDistanceMapImageFilterTest1
      DATA{${ITK_DATA_ROOT}/Input/BinarySquare4D.mhd,BinarySquare4D.zraw}
      ${ITK_TEST_OUTPUT_DIR}/itkSignedDanielssonDistanceMapImageFilterTest4D.mhd 4)

itk_add_test(NAME itkContourMeanDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkContourMeanDistanceImageFilterTest)
itk_add_test(NAME itkContourDirectedMeanDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkContourDirectedMeanDistanceImageFilterTest)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest1
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 1)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest2
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 2)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest3
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 3)
itk_add_test(NAME itkFastChamferDistanceImageFilterTest4
      COMMAND ITKDistanceMapTestDriver itkFastChamferDistanceImageFilterTest 4)
itk_add_test(NAME itkHausdorffDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkHausdorffDistanceImageFilterTest)
itk_add_test(NAME itkReflectiveImageRegionIteratorTest
      COMMAND ITKDistanceMapTestDriver itkReflectiveImageRegionIteratorTest)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest1
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest1.mhd,itkSignedMaurerDistanceMapImageFilterTest1.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
    itkSignedMaurerDistanceMapImageFilterTest DATA{${ITK_DATA_ROOT}/Input/SquareBinary201.png} ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest1.mhd 2)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest2
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest2.mhd,itkSignedMaurerDistanceMapImageFilterTest2.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
    itkSignedMaurerDistanceMapImageFilterTest DATA{${ITK_DATA_ROOT}/Input/BrainSliceBinary.png} ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest2.mhd 2)
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest3
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest3.mhd,itkSignedMaurerDistanceMapImageFilterTest3.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
    itkSignedMaurerDistanceMapImageFilterTest DATA{${ITK_DATA_ROOT}/Input/LungSliceBinary.png} ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3.mhd 2)
# Test the distance filter on a 3D volume.
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest3D
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest3D.mhd,itkSignedMaurerDistanceMapImageFilterTest3D.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3D.mhd
    itkSignedMaurerDistanceMapImageFilterTest DATA{${ITK_DATA_ROOT}/Input/BinarySquare3D.mhd,BinarySquare3D.zraw} ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest3D.mhd 3)
# Test the distance filter on a 4D volume.
# The middle 3D volume of the output 4D volume should be exactly the same as the
# 3D volume of the previous test.
itk_add_test(NAME itkSignedMaurerDistanceMapImageFilterTest4D
      COMMAND ITKDistanceMapTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkSignedMaurerDistanceMapImageFilterTest4D.mhd,itkSignedMaurerDistanceMapImageFilterTest4D.zraw}
              ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest4D.mhd
    itkSignedMaurerDistanceMapImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/BinarySquare4D.mhd,BinarySquare4D.zraw}
      ${ITK_TEST_OUTPUT_DIR}/itkSignedMaurerDistanceMapImageFilterTest4D.mhd
      4)

itk_add_test(NAME itkApproximateSignedDistanceMapImageFilterTest0
      COMMAND ITKDistanceMapTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest0.mhd "dad6e6dd8b849b884d15e91954f808d1"
    itkApproximateSignedDistanceMapImageFilterTest 100 ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest0.mhd)
itk_add_test(NAME itkApproximateSignedDistanceMapImageFilterTest1
      COMMAND ITKDistanceMapTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest1.mhd "dad6e6dd8b849b884d15e91954f808d1"
    itkApproximateSignedDistanceMapImageFilterTest 1 ${ITK_TEST_OUTPUT_DIR}/itkApproximateSignedDistanceMapImageFilterTest1.mhd)
itk_add_test(NAME itkIsoContourDistanceImageFilterTest
      COMMAND ITKDistanceMapTestDriver itkIsoContourDistanceImageFilterTest)
