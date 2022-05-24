itk_module_test()
set(ITKFFTTests
itkComplexToComplex1DFFTImageFilterTest.cxx
itkComplexToComplexFFTImageFilterTest.cxx
itkFFT1DImageFilterTest.cxx
itkFFTPadImageFilterTest.cxx
itkFFTShiftImageFilterTest.cxx
itkForward1DFFTImageFilterTest.cxx
itkForwardInverseFFTImageFilterTest.cxx
itkFullToHalfHermitianImageFilterTest.cxx
itkHalfToFullHermitianImageFilterTest.cxx
itkInverse1DFFTImageFilterTest.cxx
itkVnlFFTTest.cxx
itkVnlRealFFTTest.cxx
itkVnlComplexToComplexFFTImageFilterTest.cxx
)

if(ITK_USE_FFTWF)
  list(APPEND ITKFFTTests
    itkFFTWF_FFTTest.cxx
    itkFFTWF_RealFFTTest.cxx
    itkVnlFFTWF_FFTTest.cxx
    itkVnlFFTWF_RealFFTTest.cxx
  )
endif()

if(ITK_USE_FFTWD)
  list(APPEND ITKFFTTests
    itkFFTWD_FFTTest.cxx
    itkFFTWD_RealFFTTest.cxx
    itkVnlFFTWD_FFTTest.cxx
    itkVnlFFTWD_RealFFTTest.cxx
  )
endif()

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  list(APPEND ITKFFTTests
    itkFFTWComplexToComplexFFTImageFilterTest.cxx
  )
endif()

CreateTestDriver(ITKFFT  "${ITKFFT-Test_LIBRARIES}" "${ITKFFTTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkVnlFFTTest
      COMMAND ITKFFTTestDriver  --redirectOutput ${TEMP}/itkVnlFFTTest.txt
    itkVnlFFTTest)
set_tests_properties(itkVnlFFTTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkVnlFFTTest.txt)

itk_add_test(NAME itkVnlRealFFTTest
      COMMAND ITKFFTTestDriver --redirectOutput ${TEMP}/itkVnlRealFFTTest.txt
    itkVnlRealFFTTest)
set_tests_properties(itkVnlRealFFTTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkVnlRealFFTTest.txt)

if(ITK_USE_FFTWF)
  itk_add_test(NAME itkFFTWF_FFTTest
    COMMAND ITKFFTTestDriver itkFFTWF_FFTTest ${ITK_TEST_OUTPUT_DIR})
  itk_add_test(NAME itkFFTWF_RealFFTTest
    COMMAND ITKFFTTestDriver itkFFTWF_RealFFTTest ${ITK_TEST_OUTPUT_DIR})
  itk_add_test(NAME itkVnlFFTWF_FFTTest
    COMMAND ITKFFTTestDriver  itkVnlFFTWF_FFTTest)
  itk_add_test(NAME itkVnlFFTWF_RealFFTTest
    COMMAND ITKFFTTestDriver  itkVnlFFTWF_RealFFTTest)
  set_tests_properties(itkVnlFFTWF_FFTTest itkVnlFFTWF_RealFFTTest PROPERTIES ENVIRONMENT
    "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WRITE_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_FILE=${ITK_TEST_OUTPUT_DIR}/.wisdom_from_ITK_FFTW_WISDOM_CACHE_FILE;ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE")
endif()

if(ITK_USE_FFTWD)
  itk_add_test(NAME itkFFTWD_FFTTest
    COMMAND ITKFFTTestDriver itkFFTWD_FFTTest ${ITK_TEST_OUTPUT_DIR})
  itk_add_test(NAME itkFFTWD_RealFFTTest
    COMMAND ITKFFTTestDriver itkFFTWD_RealFFTTest ${ITK_TEST_OUTPUT_DIR})
  itk_add_test(NAME itkVnlFFTWD_FFTTest
    COMMAND  ITKFFTTestDriver  itkVnlFFTWD_FFTTest)
  itk_add_test(NAME itkVnlFFTWD_RealFFTTest
    COMMAND  ITKFFTTestDriver  itkVnlFFTWD_RealFFTTest)
  set_tests_properties(itkVnlFFTWD_FFTTest itkVnlFFTWD_RealFFTTest PROPERTIES ENVIRONMENT
    "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_BASE=${ITK_TEST_OUTPUT_DIR};ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE;ITK_FFTW_WRITE_WISDOM_CACHE=oN")
endif()

itk_add_test(NAME itkFFTShiftImageFilterTestOdd0
      COMMAND ITKFFTTestDriver
    --compare DATA{Baseline/itkFFTShiftImageFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png
    itkFFTShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/BrainSliceBinary.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png 0)
itk_add_test(NAME itkFFTShiftImageFilterTestOdd1
      COMMAND ITKFFTTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/BrainSliceBinary.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png
    itkFFTShiftImageFilterTest DATA{Baseline/itkFFTShiftImageFilterTest0.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png 1)
itk_add_test(NAME itkFFTShiftImageFilterTestEven0
      COMMAND ITKFFTTestDriver
    --compare DATA{Baseline/itkFFTShiftImageFilterTestEven0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png
    itkFFTShiftImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png 0)
itk_add_test(NAME itkFFTShiftImageFilterTestEven1
      COMMAND ITKFFTTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png
    itkFFTShiftImageFilterTest DATA{Baseline/itkFFTShiftImageFilterTestEven0.png} ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png 1)
itk_add_test(NAME itkHalfToFullHermitianImageFilterTestEvenEven
  COMMAND ITKFFTTestDriver
  itkHalfToFullHermitianImageFilterTest 10 10)
itk_add_test(NAME itkHalfToFullHermitianImageFilterTestEvenOdd
  COMMAND ITKFFTTestDriver
  itkHalfToFullHermitianImageFilterTest 10 15)
itk_add_test(NAME itkHalfToFullHermitianImageFilterTestOddEven
  COMMAND ITKFFTTestDriver
  itkHalfToFullHermitianImageFilterTest 15 10)
itk_add_test(NAME itkHalfToFullHermitianImageFilterTestOddOdd
  COMMAND ITKFFTTestDriver
  itkHalfToFullHermitianImageFilterTest 15 15)
itk_add_test(NAME itkFullToHalfHermitianImageFilterTestEvenEven
  COMMAND ITKFFTTestDriver
  itkFullToHalfHermitianImageFilterTest 10 10)
itk_add_test(NAME itkFullToHalfHermitianImageFilterTestEvenOdd
  COMMAND ITKFFTTestDriver
  itkFullToHalfHermitianImageFilterTest 10 15)
itk_add_test(NAME itkFullToHalfHermitianImageFilterTestOddEven
  COMMAND ITKFFTTestDriver
  itkFullToHalfHermitianImageFilterTest 15 10)
itk_add_test(NAME itkFullToHalfHermitianImageFilterTestOddOdd
  COMMAND ITKFFTTestDriver
  itkFullToHalfHermitianImageFilterTest 15 15)
itk_add_test(NAME itkForwardInverseFFTImageFilterTest1
  COMMAND ITKFFTTestDriver
  itkForwardInverseFFTImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png})
itk_add_test(NAME itkForwardInverseFFTImageFilterTest2
  COMMAND ITKFFTTestDriver
  itkForwardInverseFFTImageFilterTest DATA{${ITK_DATA_ROOT}/Input/Shapes.png})

itk_add_test(NAME itkComplexToComplexFFTImageFilter2DFloatTest
  COMMAND ITKFFTTestDriver
    --compare
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DFloatTest.mha
    itkComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DFloatTest.mha
      float)
# The data contained in ${ITK_DATA_ROOT}/Input/DicomSeries/
# is required by mri3D.mhd:
# ElementDataFile = DicomSeries/Image%04d.dcm 75 77 1
# The data file and its associated files are in different
# directories, respectively ${ITK_DATA_ROOT}/Input/ and
# ${ITK_DATA_ROOT}/Input/DicomSeries/
# Since DATA{} does not allow to specify an associated
# file that is in a different directory, we call ExternalData_Expand_Arguments
# manually to download the required files.
ExternalData_Expand_Arguments(ITKData
                              ExpandedData
                              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
                             )
itk_add_test(NAME itkComplexToComplexFFTImageFilter3DFloatTest
  COMMAND ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DFloatTest.mha
    itkComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DFloatTest.mha
      float)
itk_add_test(NAME itkComplexToComplexFFTImageFilter2DDoubleTest
  COMMAND ITKFFTTestDriver
    --compare
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DDoubleTest.mha
    itkComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DDoubleTest.mha
      double)
itk_add_test(NAME itkComplexToComplexFFTImageFilter3DDoubleTest
  COMMAND ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DDoubleTest.mha
    itkComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DDoubleTest.mha
      double)

itk_add_test(NAME itkVnlComplexToComplexFFTImageFilter2DFloatTest
  COMMAND ITKFFTTestDriver
    --compare
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter2DFloatTest.mha
    itkVnlComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter2DFloatTest.mha
      float)
itk_add_test(NAME itkVnlComplexToComplexFFTImageFilter3DFloatTest
  COMMAND ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter3DFloatTest.mha
    itkVnlComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter3DFloatTest.mha
      float)
itk_add_test(NAME itkVnlComplexToComplexFFTImageFilter2DDoubleTest
  COMMAND ITKFFTTestDriver
    --compare
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter2DDoubleTest.mha
    itkVnlComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter2DDoubleTest.mha
      double)
itk_add_test(NAME itkVnlComplexToComplexFFTImageFilter3DDoubleTest
  COMMAND ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter3DDoubleTest.mha
    itkVnlComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplexFFTImageFilter3DDoubleTest.mha
      double)

if(ITK_USE_FFTWF)
  itk_add_test(NAME itkFFTWComplexToComplexFFTImageFilter2DFloatTest
    COMMAND ITKFFTTestDriver
      --compare
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DFloatTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DFloatTest.mha
        float)
  itk_add_test(NAME itkFFTWComplexToComplexFFTImageFilter3DFloatTest
    COMMAND ITKFFTTestDriver
      --compare
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DFloatTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DFloatTest.mha
        float)
endif()
if(ITK_USE_FFTWD)
  itk_add_test(NAME itkFFTWComplexToComplexFFTImageFilter2DDoubleTest
    COMMAND ITKFFTTestDriver
      --compare
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DDoubleTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DDoubleTest.mha
        double)
  itk_add_test(NAME itkFFTWComplexToComplexFFTImageFilter3DDoubleTest
    COMMAND ITKFFTTestDriver
      --compare
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DDoubleTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DDoubleTest.mha
        double)
endif()

foreach(padMethod ZeroFluxNeumann Zero Wrap) # Mirror
  foreach(gpf 5 13)
    itk_add_test(NAME itkFFTPadImageFilterTest${padMethod}${gpf}
          COMMAND ITKFFTTestDriver
        --compare DATA{Baseline/itkFFTPadImageFilterTest${padMethod}${gpf}.png}
                  ${ITK_TEST_OUTPUT_DIR}/itkFFTPadImageFilterTest${padMethod}${gpf}.png
        itkFFTPadImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1-257.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTPadImageFilterTest${padMethod}${gpf}.png ${gpf} ${padMethod})
  endforeach()
endforeach()

# Test header files circular dependencies
add_executable(ITKFFTTestCircularDependency itkTestCircularDependency.cxx)
target_link_libraries(ITKFFTTestCircularDependency ${ITKFFT-Test_LIBRARIES})
itk_add_test(NAME itkFFTTestCircularDependency COMMAND ITKFFTTestCircularDependency)

# Get test data input directory for test parameter
itk_add_test(NAME itkComplexToComplex1DFFTImageFilterTest
  COMMAND ITKFFTTestDriver
  --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplex1DFFTImageFilterTestOutput.mha
  itkComplexToComplex1DFFTImageFilterTest
      DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd}
      DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplex1DFFTImageFilterTestOutput.mha
    )
# use a forward fft, then inverse fft for this instead because of the FullMatrix
# issue
itk_add_test(NAME itkFFT1DImageFilterTest
  COMMAND ITKFFTTestDriver
  --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFT1DImageFilterTestOutput.mha
  itkFFT1DImageFilterTest
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFT1DImageFilterTestOutput.mha
    )
itk_add_test(NAME itkVnlComplexToComplex1DFFTImageFilterTest
  COMMAND ITKFFTTestDriver
  --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplex1DFFTImageFilterTestOutput.mhd
  itkComplexToComplex1DFFTImageFilterTest
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlComplexToComplex1DFFTImageFilterTestOutput.mhd
    1
    )
itk_add_test(NAME itkVnlForward1DFFTImageFilterTest
  COMMAND ITKFFTTestDriver
  --compare
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlForward1DFFTImageFilterTestOutputReal.mha
  --compare
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlForward1DFFTImageFilterTestOutputImaginary.mha
  itkForward1DFFTImageFilterTest
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlForward1DFFTImageFilterTestOutput
    1
    )
itk_add_test(NAME itkVnlInverse1DFFTImageFilterTest
  COMMAND ITKFFTTestDriver
  --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlInverse1DFFTImageFilterTestOutput.mhd
  itkInverse1DFFTImageFilterTest
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlInverse1DFFTImageFilterTestOutput.mhd
    1
    )
itk_add_test(NAME itkVnlFFT1DImageFilterTest
  COMMAND ITKFFTTestDriver
  --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlFFT1DImageFilterTestOutput.mha
  itkFFT1DImageFilterTest
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkVnlFFT1DImageFilterTestOutput.mha
    1
    )

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  itk_add_test(NAME itkFFTWForward1DFFTImageFilterTest
    COMMAND ITKFFTTestDriver
    --compare
      DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWForward1DFFTImageFilterTestOutputReal.mha
    --compare
      DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWForward1DFFTImageFilterTestOutputImaginary.mha
    itkForward1DFFTImageFilterTest
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWForward1DFFTImageFilterTestOutput
      2
      )
  itk_add_test(NAME itkFFTWInverse1DFFTImageFilterTest
    COMMAND ITKFFTTestDriver
    --compare
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWInverse1DFFTImageFilterTestOutput.mha
    itkInverse1DFFTImageFilterTest
      DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
      DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWInverse1DFFTImageFilterTestOutput.mha
      2
      )
  itk_add_test(NAME itkFFTWComplexToComplex1DFFTImageFilterTest
    COMMAND ITKFFTTestDriver
    --compare
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplex1DFFTImageFilterTestOutput.mha
    itkComplexToComplex1DFFTImageFilterTest
      DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd}
      DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplex1DFFTImageFilterTestOutput.mha
      2
      )
  itk_add_test(NAME itkFFTW1DImageFilterTest
    COMMAND ITKFFTTestDriver
    --compare
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTW1DImageFilterTestOutput.mha
    itkFFT1DImageFilterTest
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTW1DImageFilterTestOutput.mha
      2
      )
endif()
