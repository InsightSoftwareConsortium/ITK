itk_module_test()

add_compile_options(-D_SCL_SECURE_NO_WARNINGS) # disable non-standard warning on MSVC

set(MontageTests
  itkMontagePCMTestSynthetic.cxx
  itkMontagePCMTestFiles.cxx
  itkMontageTestTiles.cxx
  itkMontageTestOMC.cxx
  itkMontageTestRGB.cxx
  itkMontageTestTi7.cxx
  itkMontageGenericTests.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

set(TESTING_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/../Testing/Temporary")

itk_add_test(NAME itkMontageGenericTests
  COMMAND MontageTestDriver itkMontageGenericTests)

itk_add_test(NAME itkMontagePCMTestSynthetic_2cc
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2cc
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2cc.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2cc.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_2ff
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2ff
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2ff.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2ff.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_2dd
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2dd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2dd.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2dd.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_2cf
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2cf
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2cf.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_2fd
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2fd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_2cf.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_3cc
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    3cc
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3cc.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3cc.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_3ff
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    3ff
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3ff.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3ff.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_3dd
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    3dd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3dd.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3dd.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_3cf
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    3cf
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3cf.tfm
  )

itk_add_test(NAME itkMontagePCMTestSynthetic_3fd
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    3fd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3fd.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_3fd.tfm
  )


itk_add_test(NAME itkMontagePCMTestSynthetic_ShouldFail
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2cc
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_ShouldNotExist.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_ShouldNotExist.tfm
    0.9 1.1
  )
set_tests_properties(itkMontagePCMTestSynthetic_ShouldFail PROPERTIES WILL_FAIL TRUE)

itk_add_test(NAME itkMontagePCMTestFiles14
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/OMC/FlatField/14/100.tif}
    DATA{Input/OMC/FlatField/14/101.tif}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles14.nrrd
    1136.0
    0.0
    12.0651
    4.6711893
  )

# itk_add_test(NAME itkMontagePCMTestFiles16
  # COMMAND MontageTestDriver
  # itkMontagePCMTestFiles
    # DATA{Input/OMC/FlatField/16/107.tif}
    # DATA{Input/OMC/FlatField/16/108.tif}
    # ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles16.nrrd
    # 1136.0
    # 0.0
    # 12.3862
    # 5.3629
  # )

itk_add_test(NAME itkMontagePCMTestFilesSynthetic1
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/Rect1.png}
    DATA{Input/Synthetic/Rect2.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFilesSynthetic1.nrrd
    0.0
    0.0
    -21.0
    35.0
  )

itk_add_test(NAME itkMontagePCMTestFilesSynthetic1N
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/Rect1N.png}
    DATA{Input/Synthetic/Rect2N.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFilesSynthetic1N.nrrd
    0.0
    0.0
    -21.0
    35.0
  )

itk_add_test(NAME itkMontagePCMTestFilesSynthetic3D
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/Cuboid1.nrrd}
    DATA{Input/Synthetic/Cuboid2.nrrd}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFilesSynthetic3D.nrrd
    0.0
    0.0
    0.0
    -21.0
    35.0
    1.0
  )

function(AddTestOMC slicerNumber)
  itk_add_test(NAME itkMontageTestOMC${slicerNumber}
    COMMAND MontageTestDriver
    itkMontageTestOMC
      DATA{${CMAKE_CURRENT_LIST_DIR}/Input/OMC/FlatField/${slicerNumber}/,REGEX:.*}
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestOMC${slicerNumber}_
      ${TESTING_OUTPUT_PATH}/itkMontageTestOMC${slicerNumber}_
    )
endfunction()

AddTestOMC(14)
AddTestOMC(15)
AddTestOMC(16)
AddTestOMC(17)
AddTestOMC(18)

itk_add_test(NAME itkMontageTestRGB
  COMMAND MontageTestDriver
  --compare DATA{${CMAKE_CURRENT_LIST_DIR}/Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${TESTING_OUTPUT_PATH}/itkMontageTestRGB0_1.mha
  itkMontageTestRGB
    DATA{${CMAKE_CURRENT_LIST_DIR}/Input/VisibleHumanRGB/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMockMontageTestRGB
    ${TESTING_OUTPUT_PATH}/itkMontageTestRGB
  )

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Ti7)
  itk_add_test(NAME itkMontagePCMTestFiles36
    COMMAND MontageTestDriver
    itkMontagePCMTestFiles
      DATA{Input/Ti7/FlatFielded/1004.tif}
      DATA{Input/Ti7/FlatFielded/1005.tif}
      ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles36.nrrd
      480.0
      0.0
      0.5186747297993
      1.48522189017301
    )

  itk_add_test(NAME itkMontageTestTi7Slice36
    COMMAND MontageTestDriver
    itkMontageTestTi7
      ${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/FlatFielded/36
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi7Slice36
      ${TESTING_OUTPUT_PATH}/itkMontageTestTi7Slice36
    )
endif()
  
if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles)
  itk_add_test(NAME itkMontageTestTiles
    COMMAND MontageTestDriver
    itkMontageTestTiles
      ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTiles
      ${TESTING_OUTPUT_PATH}/itkMontageTestTiles
    )
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles)
  itk_add_test(NAME itkMontageTestNoisyTiles
    COMMAND MontageTestDriver
    itkMontageTestTiles
      ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestNoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMontageTestNoisyTiles
      NoisyImage
    )
endif()
