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

itk_add_test(NAME itkMontagePCMTestFiles16
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/OMC/FlatField/16/107.tif}
    DATA{Input/OMC/FlatField/16/108.tif}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles16.nrrd
    1136.0
    0.0
    12.3862
    5.3629
  )

itk_add_test(NAME itkMontagePCMTestFilesMNML0304crop
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/MNML3_03bottom.png}
    DATA{Input/Synthetic/MNML3_04top.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFilesMNML0304crop.nrrd
    0.0
    111.0
    1.0413443838811
    2.550816245965091
  )

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

function(AddTestOMC slicerNumber inMemory)
  itk_add_test(NAME itkMontageTestOMC${slicerNumber}
    COMMAND MontageTestDriver
    itkMontageTest2D
      DATA{Input/OMC/FlatField/${slicerNumber}/,REGEX:.*}
      ${TESTING_OUTPUT_PATH}/itkMontageTestOMC${slicerNumber}_
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestOMC${slicerNumber}_
      1 -1 ${inMemory} 1 0 0
    )
endfunction()

AddTestOMC(14 1)
AddTestOMC(15 0)
AddTestOMC(16 1)
AddTestOMC(17 0)
AddTestOMC(18 1)

itk_add_test(NAME itkMontageTestRGBinMemory
  COMMAND MontageTestDriver
  --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${TESTING_OUTPUT_PATH}/itkMontageTestRGBim0_1.mha
  itkMontageTest2D
    DATA{Input/VisibleHumanRGB/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTestRGBim
    ${TESTING_OUTPUT_PATH}/itkMockMontageTestRGBim
    1 -1 1 1 0 1
  )
itk_add_test(NAME itkMontageTestRGBpairs
  COMMAND MontageTestDriver
  --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${TESTING_OUTPUT_PATH}/itkMontageTestRGBp0_1.mha
  itkMontageTest2D
    DATA{Input/VisibleHumanRGB/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTestRGBp
    ${TESTING_OUTPUT_PATH}/itkMockMontageTestRGBp
    1 -1 0 1 1 1
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

option(ITKMontage_EnableFailingTests "Should we enable tests which are failing due to not-yet-resolved issues?" OFF)

if (ITKMontage_EnableFailingTests)

itk_add_test(NAME itkMontageTest-10-129-C_2
  COMMAND MontageTestDriver
  itkMontageTest2D
    DATA{Input/10-129-C_2/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTest-10-129-C_2
    ${TESTING_OUTPUT_PATH}/itkPairsMontageTest-10-129-C_2
    0 2 0 1 0 1
  )

itk_add_test(NAME itkMontageTest-S200-6-C
  COMMAND MontageTestDriver
  itkMontageTest2D
    DATA{Input/S200-6-C/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTest-S200-6-C
    ${TESTING_OUTPUT_PATH}/itkPairsMontageTest-S200-6-C
    0 2 1 1 0 0
  )

function(AddTestTi64flat slicerNumber)
  set(inDir "${CMAKE_CURRENT_LIST_DIR}/Input/Ti64Cylinder/FlatField/1000${slicerNumber}/")
  if (EXISTS ${inDir}10099.tif)   
    itk_add_test(NAME itkMontageTestTi64flat${slicerNumber}
      COMMAND MontageTestDriver
      itkMontageTest2D
        ${inDir}
        ${TESTING_OUTPUT_PATH}/itkMontageTestTi64flat${slicerNumber}_
        ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi64flat${slicerNumber}_
        0 2 1 1 0 0
      )
  endif()
endfunction()

AddTestTi64flat(0)
AddTestTi64flat(1)
AddTestTi64flat(2)
AddTestTi64flat(3)
AddTestTi64flat(4)

function(AddTestTi64raw slicerNumber)
  set(inDir "${CMAKE_CURRENT_LIST_DIR}/Input/Ti64Cylinder/Raw/LEROY_0041_Region #1_${slicerNumber}_Mosaic.tif_Files/")
  if (EXISTS "${inDir}LEROY_0041_Region #1_${slicerNumber}_Mosaic_p099.tif")
    itk_add_test(NAME itkMontageTestTi64raw${slicerNumber}
      COMMAND MontageTestDriver
      itkMontageTest2D
        ${inDir}
        ${TESTING_OUTPUT_PATH}/itkMontageTestTi64raw${slicerNumber}_
        ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi64raw${slicerNumber}_
        0 2 1 1 0 0
      )
  endif()
endfunction()

AddTestTi64raw(0)
AddTestTi64raw(1)
AddTestTi64raw(2)
AddTestTi64raw(3)
AddTestTi64raw(4)

itk_add_test(NAME itkMontageTestMNML3
  COMMAND MontageTestDriver
  itkMontageTest2D
    DATA{Input/MNML_3_200x_701/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTestMNML3_
    ${TESTING_OUTPUT_PATH}/itkMontageTestMNML3Pairs
    1 -1 0 1 0 0
  )

itk_add_test(NAME itkMontageTestMNML5
  COMMAND MontageTestDriver
  itkMontageTest2D
    DATA{Input/MNML_5_500x_101/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageTestMNML5_
    ${TESTING_OUTPUT_PATH}/itkMontageTestMNML5Pairs
    0 1 1 1 0 0
  )

itk_add_test(NAME itkMontagePCMTestFilesFocus
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/FocusSample.png}
    DATA{Input/Synthetic/FocusBackground.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFilesFocus.nrrd
    0.0
    0.0
    0.0
    0.0
  )

itk_add_test(NAME itkMontagePCMTestFilesMNML0304
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/MNML_3_200x_701/MNML-3_200x_701-Raw_p03.png}
    DATA{Input/MNML_3_200x_701/MNML-3_200x_701-Raw_p04.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFilesMNML0304.nrrd
    0.0
    919.0
    1.0413443838811
    2.550816245965091
  )

itk_add_test(NAME itkMontagePCMTestFilesDzZ_T1_identity
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/DzZ_T1/DzZ_T1_sup.nrrd}
    DATA{Input/DzZ_T1/DzZ_T1_inf.nrrd}
    ${TESTING_OUTPUT_PATH}/DzZ_T1_identity.nrrd
    0.0
    100.0
    0.0
    0.0
    40.904
    0.0
  )

endif(ITKMontage_EnableFailingTests)

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

  if (ITKMontage_EnableFailingTests)
  
  itk_add_test(NAME itkMontagePCMTestFiles36_088
    COMMAND MontageTestDriver
    itkMontagePCMTestFiles
      ${rawPrefix}088.tif
      ${rawPrefix}089.tif
      ${TESTING_OUTPUT_PATH}/itkMontagePCMTestFiles36raw088_089.nrrd
      480.0
      0.0
      1.0798103830525
      -1.4601269970894
    )

  itk_add_test(NAME itkMontageTestTi7Slice36flat
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${flatFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTestTi7flat36_
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi7flat36_
      0 1 1 5 0 0
    )

  itk_add_test(NAME itkMontageTestTi7Slice36raw
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${rawFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTestTi7raw36_
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTi7raw36_
      0 1 1 5 0 0
    )
    
    endif(ITKMontage_EnableFailingTests)
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles/Image_10_10.tif)
  itk_add_test(NAME itkMontageTestTiles
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles
      ${TESTING_OUTPUT_PATH}/itkMontageTestTiles
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestTiles
      0 -1 1 1 0 0
    )
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles/NoisyImage_10_10.tif)
  itk_add_test(NAME itkMontageTestNoisyTiles
    COMMAND MontageTestDriver
    itkMontageTest2D
      ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMontageTestNoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMockMontageTestNoisyTiles
      0 -1 1 1 0 0
    )
endif()
