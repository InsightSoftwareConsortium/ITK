itk_module_test()

add_compile_options(-D_SCL_SECURE_NO_WARNINGS) # disable non-standard warning on MSVC

set(MontageTests
  itkInMemoryMontageTest2D.cxx
  itkMontagePCMTestSynthetic.cxx
  itkMontagePCMTestFiles.cxx
  itkMontageGenericTests.cxx
  itkMontageTest.cxx
  itkMontageTruthCreator.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

set(TESTING_OUTPUT_PATH "${CMAKE_BINARY_DIR}/Testing/Temporary")

itk_add_test(NAME itkMontageGenericTests
  COMMAND MontageTestDriver itkMontageGenericTests)

set(SyntheticOutputPath "${TESTING_OUTPUT_PATH}/synthetic")
file(MAKE_DIRECTORY ${SyntheticOutputPath})

function(AddTestSynthetic dimTTcode startSize endSize)
  itk_add_test(NAME itkMontagePCMSynthetic_${dimTTcode}
    COMMAND MontageTestDriver
    itkMontagePCMTestSynthetic
      ${dimTTcode}
      ${SyntheticOutputPath}/itkMontagePCMSynthetic_${dimTTcode}.nrrd
      ${SyntheticOutputPath}/itkMontagePCMSynthetic_${dimTTcode}.tfm
      ${startSize}
      ${endSize}
    )
endfunction()

AddTestSynthetic(2cc  21 23)
AddTestSynthetic(2ff  17 19)
AddTestSynthetic(2dd  17 51)
AddTestSynthetic(2cf  11 13)
AddTestSynthetic(2fd  51 71)
AddTestSynthetic(3cc  21 23)
AddTestSynthetic(3ff  17 19)
AddTestSynthetic(3dd  31 47)
AddTestSynthetic(3cf  11 13)
AddTestSynthetic(3fd  19 21)


itk_add_test(NAME itkMontagePCMSynthetic_ShouldFail
  COMMAND MontageTestDriver
  itkMontagePCMTestSynthetic
    2cc
    ${TESTING_OUTPUT_PATH}/itkMontagePCMSynthetic_ShouldNotExist.nrrd
    ${TESTING_OUTPUT_PATH}/itkMontagePCMSynthetic_ShouldNotExist.tfm
    17 19
    0.9 1.1
  )
set_tests_properties(itkMontagePCMSynthetic_ShouldFail PROPERTIES WILL_FAIL TRUE)

itk_add_test(NAME itkMontagePCMFiles14
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/OMC/FlatField/14/100.tif}
    DATA{Input/OMC/FlatField/14/101.tif}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFiles14.nrrd
    400.755808
    0.0
    4.256301848
    1.647892819
  )

itk_add_test(NAME itkMontagePCMFiles15same
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/OMC/FlatField/15/107.tif}
    DATA{Input/OMC/FlatField/15/107.tif}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFiles15same.nrrd
    0.0
    0.0
    0.0
    0.0
  )

itk_add_test(NAME itkMontagePCMFiles16
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/OMC/FlatField/16/107.tif}
    DATA{Input/OMC/FlatField/16/108.tif}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFiles16.nrrd
    400.755808
    0.0
    4.369578864
    1.891913136
  )

itk_add_test(NAME itkMontagePCMFilesMNML0304crop
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/MNML3_03bottom.png}
    DATA{Input/Synthetic/MNML3_04top.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesMNML0304crop.nrrd
    0.0
    111.0
    1.0413443838811
    2.550816245965091
  )

itk_add_test(NAME itkMontagePCMFilesSynthetic1
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/Rect1.png}
    DATA{Input/Synthetic/Rect2.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesSynthetic1.nrrd
    0.0
    0.0
    -21.0
    35.0
  )

itk_add_test(NAME itkMontagePCMFilesSynthetic1N
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/Rect1N.png}
    DATA{Input/Synthetic/Rect2N.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesSynthetic1N.nrrd
    0.0
    0.0
    -21.0
    35.0
  )

itk_add_test(NAME itkMontagePCMFilesSynthetic3D
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/Cuboid1.nrrd}
    DATA{Input/Synthetic/Cuboid2.nrrd}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesSynthetic3D.nrrd
    0.0
    0.0
    0.0
    -21.0
    35.0
    1.0
  )

function(AddTestOMC slicerNumber inMemory tolerance)
  itk_add_test(NAME itkMontageOMC${slicerNumber}
    COMMAND MontageTestDriver
    itkMontageTest
      DATA{Input/OMC/FlatField/${slicerNumber}/,REGEX:.*}
      ${TESTING_OUTPUT_PATH}/itkMontageOMC${slicerNumber}_
      ${TESTING_OUTPUT_PATH}/itkMontageOMC${slicerNumber}Pairs_
      1 -1 ${inMemory} 1 0 0 0 ${tolerance} 1
    )
endfunction()

AddTestOMC(14 1 15)
AddTestOMC(15 0 15)
AddTestOMC(16 1 0)
AddTestOMC(17 0 0)
AddTestOMC(18 1 0)

# this test should not crash, but is expected to fail
itk_add_test(NAME itkMontageSingleLayer3D
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/SingleLayer3D/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageSingleLayer3D
    ${TESTING_OUTPUT_PATH}/itkMontageSingleLayer3DPairs
    0 2 1 1 0 0 0 0 1
  )
set_tests_properties(itkMontageSingleLayer3D PROPERTIES WILL_FAIL TRUE)

itk_add_test(NAME itkMontageRGBinMemory
  COMMAND MontageTestDriver
  --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${SyntheticOutputPath}/itkMontageRGBim0_1.mha
  itkMontageTest
    DATA{Input/VisibleHumanRGB/,REGEX:.*}
    ${SyntheticOutputPath}/itkMontageRGBim
    ${SyntheticOutputPath}/itkMontageRGBimPairs
    1 -1 1 1 0 1 0 0 1
  )

itk_add_test(NAME itkMontageRGBpairsTol
  COMMAND MontageTestDriver
  --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${SyntheticOutputPath}/itkMontageRGBim0_1.mha
  itkMontageTest
    DATA{Input/VisibleHumanRGB/,REGEX:.*}
    ${SyntheticOutputPath}/itkMontageRGBpTol
    ${SyntheticOutputPath}/itkMontageRGBpTolPairs
    1 -1 0 1 1 1 0 10 1
  )

itk_add_test(NAME itkMontageRGBpairs
  COMMAND MontageTestDriver
  --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                 ${SyntheticOutputPath}/itkMontageRGBp0_1.mha
  itkMontageTest
    DATA{Input/VisibleHumanRGB/,REGEX:.*}
    ${SyntheticOutputPath}/itkMontageRGBp
    ${SyntheticOutputPath}/itkMontageRGBpPairs
    1 -1 0 1 1 1 0 0 1
  )

function(AddInMemoryMontageTest variation willFail)
  itk_add_test(NAME itkInMemoryMontage${variation}
    COMMAND MontageTestDriver
      --compare DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png}
                     ${SyntheticOutputPath}/itkInMemoryMontageRGB${variation}.mha
      --ignoreInputInformation # ignore different spacing
      itkInMemoryMontageTest2D
        DATA{Input/VisibleHumanRGB/,REGEX:.*}
        ${SyntheticOutputPath}/itkInMemoryMontageRGB
        ${variation}
    )
  set_tests_properties(itkInMemoryMontage${variation} PROPERTIES WILL_FAIL ${willFail})
endfunction()

AddInMemoryMontageTest(0 TRUE)
AddInMemoryMontageTest(1 TRUE)
AddInMemoryMontageTest(2 FALSE)
AddInMemoryMontageTest(3 FALSE)
AddInMemoryMontageTest(4 FALSE)
AddInMemoryMontageTest(5 FALSE)
AddInMemoryMontageTest(6 FALSE)
AddInMemoryMontageTest(7 FALSE)

itk_add_test(NAME itkMontageMediumCarbonSteel
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/MediumCarbonSteel/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageMediumCarbonSteel
    ${TESTING_OUTPUT_PATH}/itkMontageMediumCarbonSteelPairs
    1 -1 1 4 1 0 0 0 1
  )

itk_add_test(NAME itkMontageCMUrun2_64
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/05MAR09_run2_64-Raw/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageCMUrun2_64_
    ${TESTING_OUTPUT_PATH}/itkMontageCMUrun2_64Pairs_
    0 1 1 0 0 0 0 0 1
  )

itk_add_test(NAME itkMontageCMUrun2_64_comb
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/05MAR09_run2_64-Raw/,REGEX:.*}
    ${SyntheticOutputPath}/itkMontageCMUrun2_64_comb
    ${SyntheticOutputPath}/itkMontageCMUrun2_64_combPairs
    1 -1 1 4 1 1 0 0 1
  )

function(AddTestTi64flat slicerNumber)
  set(inDir "${CMAKE_CURRENT_LIST_DIR}/Input/Ti64Cylinder/FlatField/1000${slicerNumber}/")
  if (EXISTS ${inDir}10099.tif)
    itk_add_test(NAME itkMontageTi64flat${slicerNumber}
      COMMAND MontageTestDriver
      itkMontageTest
        ${inDir}
        ${TESTING_OUTPUT_PATH}/itkMontageTi64flat${slicerNumber}_
        ${TESTING_OUTPUT_PATH}/itkMontageTi64flat${slicerNumber}Pairs_
        0 2 1 1 0 0 1 0 1
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
    itk_add_test(NAME itkMontageTi64raw${slicerNumber}
      COMMAND MontageTestDriver
      itkMontageTest
        ${inDir}
        ${TESTING_OUTPUT_PATH}/itkMontageTi64raw${slicerNumber}_
        ${TESTING_OUTPUT_PATH}/itkMontageTi64raw${slicerNumber}Pairs_
        0 2 1 1 0 0 1 0 1
      )
  endif()
endfunction()

AddTestTi64raw(0)
AddTestTi64raw(1)
AddTestTi64raw(2)
AddTestTi64raw(3)
AddTestTi64raw(4)

option(Module_Montage_EnableFailingTests "Should we enable tests which are failing due to not-yet-resolved issues?" OFF)
# we should remove this option once we resolve https://github.com/InsightSoftwareConsortium/ITK/issues/4613

# have uniform options for GroundTruthTests
set(varyingPaddingMethods 0)
set(peakMethod -1)
set(loadIntoMemory 1)
set(streamSubdivisions 1)
set(doPairs 0)
set(writeTransforms 0)
set(allowDrift 1)
set(positionTolerance 0)
set(writeImage 1)
  
function(GroundTruthTest2D tempDir inputFile intensityTolerance) # other command-line parameters
  set(outDir ${TESTING_OUTPUT_PATH}/${tempDir}/)
  file(MAKE_DIRECTORY ${outDir})
  itk_add_test(NAME itkMontageGroundTruthMake${tempDir}
    COMMAND MontageTestDriver
    itkMontageTruthCreator ${inputFile} ${outDir} ${ARGN}
    )
  set(regressionPart "")
  if (Module_Montage_EnableFailingTests)
    set(regressionPart --compare ${inputFile} ${TESTING_OUTPUT_PATH}/itkMontageGT${tempDir}_2_1.mha)
  endif()


  itk_add_test(NAME itkMontageGroundTruthRun${tempDir}
    COMMAND MontageTestDriver
    --compareRadiusTolerance 1
    --compareIntensityTolerance ${intensityTolerance}
    --compareNumberOfPixelsTolerance 1500
    ${regressionPart}
    itkMontageTest
      ${outDir}
      ${TESTING_OUTPUT_PATH}/itkMontageGT${tempDir}_
      ${TESTING_OUTPUT_PATH}/itkMontageGT${tempDir}Pairs_
      ${varyingPaddingMethods}
      ${peakMethod}
      ${loadIntoMemory}
      ${streamSubdivisions}
      ${doPairs}
      ${writeTransforms}
      ${allowDrift}
      ${positionTolerance}
      ${writeImage}
    )
  set_tests_properties(itkMontageGroundTruthRun${tempDir}
    PROPERTIES DEPENDS itkMontageGroundTruthMake${tempDir})
endfunction()

GroundTruthTest2D(10-129 DATA{Input/10-129-C_2/10-129-C_2_p00.jpg} 16 6 4 15 25)
GroundTruthTest2D(run2 DATA{Input/05MAR09_run2_64-Raw/05MAR09_run2_64-Raw_p0.bmp} 16 4 5 15 10)
GroundTruthTest2D(MediumCarbonSteel DATA{Input/MediumCarbonSteel/1701701_000000_Fused.tif} 4096 11 3 20 20)
GroundTruthTest2D(MNML3 DATA{Input/MNML_3_200x_701/MNML-3-ITK.png} 16 4 5 5 5)
GroundTruthTest2D(MNML5 DATA{Input/MNML_5_500x_101/MNML_5-ITK.png} 16 6 9 5 5)
GroundTruthTest2D(VisibleHuman DATA{Input/VisibleHumanRGB/VisibleHumanMale1608.png} 16 5 4 15 15)
GroundTruthTest2D(OMC14 DATA{Input/OMC/FlatField/14/OMC14-ITK.tif} 4096 8 6 10 10)


itk_add_test(NAME itkMontage-10-129-C_2
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/10-129-C_2/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontage-10-129-C_2
    ${TESTING_OUTPUT_PATH}/itkMontage-10-129-C_2Pairs
    0 2 0 1 0 0 1 0 1
  )

itk_add_test(NAME itkMontage-S200-small
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/S200-small/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontage-S200-small
    ${TESTING_OUTPUT_PATH}/itkMontage-S200-smallPairs
    0 1 0 1 0 0 75 0 1
  )

itk_add_test(NAME itkMontage-S200-6-C
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/S200-6-C/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontage-S200-6-C
    ${TESTING_OUTPUT_PATH}/itkMontage-S200-6-C-Pairs
    0 2 0 10 0 0 1 25 0
  )

itk_add_test(NAME itkMontageMNML3
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/MNML_3_200x_701/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageMNML3_
    ${TESTING_OUTPUT_PATH}/itkMontageMNML3Pairs
    1 -1 0 5 0 0 0 5 1
  )

itk_add_test(NAME itkMontageMNML5
  COMMAND MontageTestDriver
  itkMontageTest
    DATA{Input/MNML_5_500x_101/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMontageMNML5_
    ${TESTING_OUTPUT_PATH}/itkMontageMNML5Pairs
    0 1 0 10 0 0 1 10 1
  )

itk_add_test(NAME itkMontagePCMFiles-10-129
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/10-129-C_2/10-129-C_2_p00.jpg}
    DATA{Input/10-129-C_2/10-129-C_2_p01.jpg}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFiles-10-129.nrrd
    0.0
    295.0904267
    -6.143508728
    -10.488
  )

if (Module_Montage_EnableFailingTests)
itk_add_test(NAME itkMontagePCMFilesS200_32_33
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/S200-small/S200-6-320x_p032.jpg}
    DATA{Input/S200-small/S200-6-320x_p033.jpg}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesS200_32_33.nrrd
    2381.0
    0.0
    74.09566
    7.614863315
  )
endif() # Module_Montage_EnableFailingTests

itk_add_test(NAME itkMontagePCMFilesS200_33_67
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/S200-small/S200-6-320x_p033.jpg}
    DATA{Input/S200-small/S200-6-320x_p067.jpg}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesS200_33_67.nrrd
    0.0
    1742.66
    -6.05019
    16.063841685
  )

itk_add_test(NAME itkMontagePCMFilesS200_134_168
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/S200-small/S200-6-320x_p134.jpg}
    DATA{Input/S200-small/S200-6-320x_p168.jpg}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesS200_134_168.nrrd
    0.0
    1742.06
    -6.6058
    21.370794
  )

itk_add_test(NAME itkMontagePCMFilesFocus
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/Synthetic/FocusSample.png}
    DATA{Input/Synthetic/FocusBackground.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesFocus.nrrd
    0.0
    0.0
    0.0
    0.0
  )

itk_add_test(NAME itkMontagePCMFilesMNML0304
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/MNML_3_200x_701/MNML-3_200x_701-Raw_p03.png}
    DATA{Input/MNML_3_200x_701/MNML-3_200x_701-Raw_p04.png}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesMNML0304.nrrd
    0.0
    917.0
    1.0
    1.0
  )

itk_add_test(NAME itkMontagePCMFilesDzZ_T1_identity
  COMMAND MontageTestDriver
  itkMontagePCMTestFiles
    DATA{Input/DzZ_T1/DzZ_T1_sup.nrrd}
    DATA{Input/DzZ_T1/DzZ_T1_inf.nrrd}
    ${TESTING_OUTPUT_PATH}/itkMontagePCMFilesDzZ_T1_identity.nrrd
    0.0
    140.904
    0.0
    0.0
    0.0
    0.0
  )

# 3D ground truth test case
set(tempDir DzZ_T1)
set(outDir ${TESTING_OUTPUT_PATH}/${tempDir}/)
file(MAKE_DIRECTORY ${outDir})
itk_add_test(NAME itkMontageGroundTruthMake${tempDir}
  COMMAND MontageTestDriver
  itkMontageTruthCreator DATA{Input/DzZ_T1/DzZ_T1_orig.nhdr,DzZ_T1_orig.raw.gz} ${outDir} 3 6 3 25 15 50
  )
itk_add_test(NAME itkMontageGroundTruthRun${tempDir}
  COMMAND MontageTestDriver
  # a lot of tolerances are required because the reconstruction is not perfect
    --compareRadiusTolerance 1
    --compareIntensityTolerance 128
    --compareNumberOfPixelsTolerance 300
    --compare DATA{Input/DzZ_T1/DzZ_T1_orig.nhdr}
      ${TESTING_OUTPUT_PATH}/itkMontageGT${tempDir}_2_1.mha
  itkMontageTest
    ${outDir}
    ${TESTING_OUTPUT_PATH}/itkMontageGT${tempDir}_
    ${TESTING_OUTPUT_PATH}/itkMontageGT${tempDir}Pairs_
    ${varyingPaddingMethods}
    ${peakMethod}
    ${loadIntoMemory}
    ${streamSubdivisions}
    ${doPairs}
    ${writeTransforms}
    ${allowDrift}
    ${positionTolerance}
    ${writeImage}
  )
set_tests_properties(itkMontageGroundTruthRun${tempDir}
  PROPERTIES DEPENDS itkMontageGroundTruthMake${tempDir})

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region2_150_Mosaic36Flat/36/1323.tif)

  set(flatFolder "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region2_150_Mosaic36Flat/36")
  set(rawFolder "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region2_150_Mosaic36/Ti-7Al_Region #2_150_Mosaic_36.tif_Files")
  set(rawPrefix "${rawFolder}/Ti-7Al_Region #2_150_Mosaic_36_p")

  itk_add_test(NAME itkMontagePCMFiles36_1004
    COMMAND MontageTestDriver
    itkMontagePCMTestFiles
      ${flatFolder}/1004.tif
      ${flatFolder}/1005.tif
      ${TESTING_OUTPUT_PATH}/itkMontagePCMFiles36_1004.nrrd
      480.0
      0.0
      0.5186747297993
      1.48522189017301
    )

  itk_add_test(NAME itkMontageTi7R2_150S36flat
    COMMAND MontageTestDriver
    itkMontageTest
      ${flatFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R2_150S36flat
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R2_150S36flatPairs
      0 1 1 5 0 0 1 0 1
    )

  itk_add_test(NAME itkMontageTi7R2_150S36raw
    COMMAND MontageTestDriver
    itkMontageTest
      ${rawFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R2_150S36raw
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R2_150S36rawPairs
      0 1 1 5 0 0 1 0 1
    )

  itk_add_test(NAME itkMontagePCMFiles36_088
    COMMAND MontageTestDriver
    itkMontagePCMTestFiles
      ${rawPrefix}088.tif
      ${rawPrefix}089.tif
      ${TESTING_OUTPUT_PATH}/itkMontagePCMFiles36raw088_089.nrrd
      81.28
      0.0
      0.0
      0.0
  )
endif()

if (EXISTS "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region1_10_Mosaic36/Ti-7Al_Region #1_10_Mosaic_36_p323.tif")
  set(rawFolder "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region1_10_Mosaic36")
  set(rawPrefix "${rawFolder}/Ti-7Al_Region #1_10_Mosaic_36_p")

  itk_add_test(NAME itkMontageTi7R1_10S36
    COMMAND MontageTestDriver
    itkMontageTest
      ${rawFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R1_10S36_
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R1_10S36Pairs_
      0 1 1 5 0 0 1 0 1
    )
endif()

if (EXISTS "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region1_10_Mosaic180/Ti-7Al_Region #1_10_Mosaic_180_p323.tif")
  set(rawFolder "${CMAKE_CURRENT_LIST_DIR}/Input/Ti7/Region1_10_Mosaic180")
  set(rawPrefix "${rawFolder}/Ti-7Al_Region #1_10_Mosaic_180_p")

  itk_add_test(NAME itkMontageTi7R1_10S180
    COMMAND MontageTestDriver
    itkMontageTest
      ${rawFolder}
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R1_10S180_
      ${TESTING_OUTPUT_PATH}/itkMontageTi7R1_10S180Pairs_
      0 1 1 5 0 0 1 0 1
    )
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles/Image_10_10.tif)
  itk_add_test(NAME itkMontageTiles
    COMMAND MontageTestDriver --ignoreInputInformation
    #  --compare ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles/itkMontageTiles2_0.mha ${TESTING_OUTPUT_PATH}/itkMontageTiles2_0.mha
    itkMontageTest
      ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles
      ${TESTING_OUTPUT_PATH}/itkMontageTiles
      ${TESTING_OUTPUT_PATH}/itkMontageTilesPairs
      0 -1 1 1 1 0 0 0 1
    )
    GroundTruthTest2D(Tiles ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles/itkMontageTiles2_0.mha 10 10 10 10)
endif()

if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles/NoisyImage_10_10.tif)
  itk_add_test(NAME itkMontageNoisyTiles
    COMMAND MontageTestDriver
    itkMontageTest
      ${CMAKE_CURRENT_LIST_DIR}/Input/NoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMontageNoisyTiles
      ${TESTING_OUTPUT_PATH}/itkMontageNoisyTilesPairs
      0 -1 1 1 0 0 0 0 1
    )
endif()
