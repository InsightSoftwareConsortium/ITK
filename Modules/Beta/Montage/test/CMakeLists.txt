itk_module_test()

add_compile_options(-D_SCL_SECURE_NO_WARNINGS) # disable non-standard warning on MSVC

set(MontageTests
  itkInMemoryMontageTest2D.cxx
  itkMontagePCMTestSynthetic.cxx
  itkMontagePCMTestFiles.cxx
  itkMontageGenericTests.cxx
  itkMontageTest2D.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

set(TESTING_OUTPUT_PATH "${CMAKE_BINARY_DIR}/Testing/Temporary")

itk_add_test(NAME itkMontageGenericTests
  COMMAND MontageTestDriver itkMontageGenericTests)

function(AddTestSynthetic dimTTcode)
  itk_add_test(NAME itkMontagePCMTestSynthetic_${dimTTcode}
    COMMAND MontageTestDriver
    itkMontagePCMTestSynthetic
      ${dimTTcode}
      ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_${dimTTcode}.nrrd
      ${TESTING_OUTPUT_PATH}/itkMontagePCMTestSynthetic_${dimTTcode}.tfm
    )
endfunction()

AddTestSynthetic(2cc)
AddTestSynthetic(2ff)
AddTestSynthetic(2dd)
AddTestSynthetic(2cf)
AddTestSynthetic(2fd)
AddTestSynthetic(3cc)
AddTestSynthetic(3ff)
AddTestSynthetic(3dd)
AddTestSynthetic(3cf)
AddTestSynthetic(3fd)


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

itk_add_test(NAME itkMontagePCMTestFiles15same
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/OMC/FlatField/15/107.tif}
    DATA{Input/OMC/FlatField/15/107.tif}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles15same.nrrd
    0.0
    0.0
    0.0
    0.0
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
    itkMontageTest2D
      DATA{Input/OMC/FlatField/${slicerNumber}/,REGEX:.*}
      ${TESTING_OUTPUT_PATH}/itkMontageTestOMC${slicerNumber}_
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestOMC${slicerNumber}_
      1 -1 0 1
    )
endfunction()

AddTestOMC(14)
AddTestOMC(15)
AddTestOMC(16)
AddTestOMC(17)
AddTestOMC(18)

itk_add_test(NAME itkMontageTestRGB
  COMMAND MontageTestDriver
  --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${TESTING_OUTPUT_PATH}/itkMontageTestRGB0_1.mha
  itkMontageTest2D
    DATA{Input/VisibleHumanRGB/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTestRGB
    ${TESTING_OUTPUT_PATH}/itkMockMontageTestRGB
    1 -1 1 1
  )

function(AddInMemoryMontageTest variation willFail)
  itk_add_test(NAME itkInMemoryMontageTest${variation}
    COMMAND MontageTestDriver
      --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                     ${TESTING_OUTPUT_PATH}/itkInMemoryMontageTestRGB${variation}.mha
      --ignoreInputInformation # ignore different spacing
      itkInMemoryMontageTest2D
        DATA{Input/VisibleHumanRGB/,REGEX:.*}
        ${TESTING_OUTPUT_PATH}/itkInMemoryMontageTestRGB
        ${variation}
    )
  set_tests_properties(itkInMemoryMontageTest${variation} PROPERTIES WILL_FAIL ${willFail})
endfunction()

AddInMemoryMontageTest(0 TRUE)
AddInMemoryMontageTest(1 TRUE)
AddInMemoryMontageTest(2 FALSE)
AddInMemoryMontageTest(3 FALSE)
AddInMemoryMontageTest(4 FALSE)
AddInMemoryMontageTest(5 FALSE)
AddInMemoryMontageTest(6 FALSE)
AddInMemoryMontageTest(7 FALSE)

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/FlatFielded/36/1000.tif)

  set(flatFolder "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/FlatFielded/36")
  set(rawFolder "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Raw/Ti-7Al_Region #2_150_Mosaic_36.tif_Files")
  set(rawPrefix "${rawFolder}/Ti-7Al_Region #2_150_Mosaic_36_p")

  itk_add_test(NAME itkMontagePCMTestFiles36_1004
    COMMAND MontageTestDriver
    itkMontagePCMTestFiles
      ${flatFolder}/1004.tif
      ${flatFolder}/1005.tif
      ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles36_1004.nrrd
      480.0
      0.0
      0.5186747297993
      1.48522189017301
    )

  # itk_add_test(NAME itkMontagePCMTestFiles36_088
    # COMMAND MontageTestDriver
    # itkMontagePCMTestFiles
      # ${rawPrefix}088.tif
      # ${rawPrefix}089.tif
      # ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles36raw088_089.nrrd
      # 480.0
      # 0.0
      # 1.0798103830525
      # -1.4601269970894
    # )

  itk_add_test(NAME itkMontageTestTi7Slice36flat
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${flatFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTestTi7flat36_
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi7flat36_
      0 1 0 5
    )

  itk_add_test(NAME itkMontageTestTi7Slice36raw
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${rawFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTestTi7raw36_
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi7raw36_
      0 1 0 5
    )
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles/Image_10_10.tif)
  itk_add_test(NAME itkMontageTestTiles
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles
      ${TESTING_OUTPUT_PATH}/itkMontageTestTiles
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTiles
      0 -1 1 1
    )
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles/NoisyImage_10_10.tif)
  itk_add_test(NAME itkMontageTestNoisyTiles
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMontageTestNoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestNoisyTiles
      0 -1 1 1
    )
endif()
