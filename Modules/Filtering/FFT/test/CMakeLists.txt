itk_module_test()
set(
  ITKFFTTests
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
  itkPocketFFTTest.cxx
  itkPocketFFTRealFFTTest.cxx
  itkPocketFFTComplexToComplexFFTImageFilterTest.cxx
)

if(ITK_USE_FFTWF)
  list(
    APPEND
    ITKFFTTests
    itkFFTWF_FFTTest.cxx
    itkFFTWF_RealFFTTest.cxx
    itkPocketFFTFFTWF_FFTTest.cxx
    itkPocketFFTFFTWF_RealFFTTest.cxx
  )
endif()

if(ITK_USE_FFTWD)
  list(
    APPEND
    ITKFFTTests
    itkFFTWD_FFTTest.cxx
    itkFFTWD_RealFFTTest.cxx
    itkPocketFFTFFTWD_FFTTest.cxx
    itkPocketFFTFFTWD_RealFFTTest.cxx
  )
endif()

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  list(APPEND ITKFFTTests itkFFTWComplexToComplexFFTImageFilterTest.cxx)
endif()

createtestdriver(ITKFFT "${ITKFFT-Test_LIBRARIES}" "${ITKFFTTests}")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(
  NAME itkPocketFFTTest
  COMMAND
    ITKFFTTestDriver
    --redirectOutput
    ${TEMP}/itkPocketFFTTest.txt
    itkPocketFFTTest
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${TEMP}/itkPocketFFTTest.txt
)
set_tests_properties(
  itkPocketFFTTest
  PROPERTIES
    ATTACHED_FILES_ON_FAIL
      ${TEMP}/itkPocketFFTTest.txt
)

itk_add_test(
  NAME itkPocketFFTRealFFTTest
  COMMAND
    ITKFFTTestDriver
    --redirectOutput
    ${TEMP}/itkPocketFFTRealFFTTest.txt
    itkPocketFFTRealFFTTest
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${TEMP}/itkPocketFFTRealFFTTest.txt
)
set_tests_properties(
  itkPocketFFTRealFFTTest
  PROPERTIES
    ATTACHED_FILES_ON_FAIL
      ${TEMP}/itkPocketFFTRealFFTTest.txt
)

if(ITK_USE_FFTWF)
  itk_add_test(
    NAME itkFFTWF_FFTTest
    COMMAND
      ITKFFTTestDriver
      itkFFTWF_FFTTest
      ${ITK_TEST_OUTPUT_DIR}
  )
  itk_add_test(
    NAME itkFFTWF_RealFFTTest
    COMMAND
      ITKFFTTestDriver
      itkFFTWF_RealFFTTest
      ${ITK_TEST_OUTPUT_DIR}
  )
  itk_add_test(
    NAME itkPocketFFTFFTWF_FFTTest
    COMMAND
      ITKFFTTestDriver
      itkPocketFFTFFTWF_FFTTest
  )
  itk_add_test(
    NAME itkPocketFFTFFTWF_RealFFTTest
    COMMAND
      ITKFFTTestDriver
      itkPocketFFTFFTWF_RealFFTTest
  )
  set_tests_properties(
    itkPocketFFTFFTWF_FFTTest
    itkPocketFFTFFTWF_RealFFTTest
    PROPERTIES
      ENVIRONMENT
        "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WRITE_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_FILE=${ITK_TEST_OUTPUT_DIR}/.wisdom_from_ITK_FFTW_WISDOM_CACHE_FILE;ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE"
  )
endif()

if(ITK_USE_FFTWD)
  itk_add_test(
    NAME itkFFTWD_FFTTest
    COMMAND
      ITKFFTTestDriver
      itkFFTWD_FFTTest
      ${ITK_TEST_OUTPUT_DIR}
  )
  itk_add_test(
    NAME itkFFTWD_RealFFTTest
    COMMAND
      ITKFFTTestDriver
      itkFFTWD_RealFFTTest
      ${ITK_TEST_OUTPUT_DIR}
  )
  itk_add_test(
    NAME itkPocketFFTFFTWD_FFTTest
    COMMAND
      ITKFFTTestDriver
      itkPocketFFTFFTWD_FFTTest
  )
  itk_add_test(
    NAME itkPocketFFTFFTWD_RealFFTTest
    COMMAND
      ITKFFTTestDriver
      itkPocketFFTFFTWD_RealFFTTest
  )
  set_tests_properties(
    itkPocketFFTFFTWD_FFTTest
    itkPocketFFTFFTWD_RealFFTTest
    PROPERTIES
      ENVIRONMENT
        "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_BASE=${ITK_TEST_OUTPUT_DIR};ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE;ITK_FFTW_WRITE_WISDOM_CACHE=oN"
  )
endif()

itk_add_test(
  NAME itkFFTShiftImageFilterTestOdd0
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Baseline/itkFFTShiftImageFilterTest0.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png
    itkFFTShiftImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/BrainSliceBinary.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png
    0
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest0.png
)
itk_add_test(
  NAME itkFFTShiftImageFilterTestOdd1
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/BrainSliceBinary.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png
    itkFFTShiftImageFilterTest
    DATA{Baseline/itkFFTShiftImageFilterTest0.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTest1.png
)
itk_add_test(
  NAME itkFFTShiftImageFilterTestEven0
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Baseline/itkFFTShiftImageFilterTestEven0.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png
    itkFFTShiftImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png
    0
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven0.png
)
itk_add_test(
  NAME itkFFTShiftImageFilterTestEven1
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png
    itkFFTShiftImageFilterTest
    DATA{Baseline/itkFFTShiftImageFilterTestEven0.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFFTShiftImageFilterTestEven1.png
)
itk_add_test(
  NAME itkHalfToFullHermitianImageFilterTestEvenEven
  COMMAND
    ITKFFTTestDriver
    itkHalfToFullHermitianImageFilterTest
    10
    10
)
itk_add_test(
  NAME itkHalfToFullHermitianImageFilterTestEvenOdd
  COMMAND
    ITKFFTTestDriver
    itkHalfToFullHermitianImageFilterTest
    10
    15
)
itk_add_test(
  NAME itkHalfToFullHermitianImageFilterTestOddEven
  COMMAND
    ITKFFTTestDriver
    itkHalfToFullHermitianImageFilterTest
    15
    10
)
itk_add_test(
  NAME itkHalfToFullHermitianImageFilterTestOddOdd
  COMMAND
    ITKFFTTestDriver
    itkHalfToFullHermitianImageFilterTest
    15
    15
)
itk_add_test(
  NAME itkFullToHalfHermitianImageFilterTestEvenEven
  COMMAND
    ITKFFTTestDriver
    itkFullToHalfHermitianImageFilterTest
    10
    10
)
itk_add_test(
  NAME itkFullToHalfHermitianImageFilterTestEvenOdd
  COMMAND
    ITKFFTTestDriver
    itkFullToHalfHermitianImageFilterTest
    10
    15
)
itk_add_test(
  NAME itkFullToHalfHermitianImageFilterTestOddEven
  COMMAND
    ITKFFTTestDriver
    itkFullToHalfHermitianImageFilterTest
    15
    10
)
itk_add_test(
  NAME itkFullToHalfHermitianImageFilterTestOddOdd
  COMMAND
    ITKFFTTestDriver
    itkFullToHalfHermitianImageFilterTest
    15
    15
)
itk_add_test(
  NAME itkForwardInverseFFTImageFilterTest1
  COMMAND
    ITKFFTTestDriver
    itkForwardInverseFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
)
itk_add_test(
  NAME itkForwardInverseFFTImageFilterTest2
  COMMAND
    ITKFFTTestDriver
    itkForwardInverseFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/Shapes.png}
)

itk_add_test(
  NAME itkComplexToComplexFFTImageFilter2DFloatTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DFloatTest.mha
    itkComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DFloatTest.mha
    float
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DFloatTest.mha
)
# The data contained in ${ITK_DATA_ROOT}/Input/DicomSeries/
# is required by mri3D.mhd:
# ElementDataFile = DicomSeries/Image%04d.dcm 75 77 1
# The data file and its associated files are in different
# directories, respectively ${ITK_DATA_ROOT}/Input/ and
# ${ITK_DATA_ROOT}/Input/DicomSeries/
# Since DATA{} does not allow to specify an associated
# file that is in a different directory, we call ExternalData_Expand_Arguments
# manually to download the required files.
ExternalData_Expand_Arguments(
  ITKData
  ExpandedData
  DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
)
itk_add_test(
  NAME itkComplexToComplexFFTImageFilter3DFloatTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DFloatTest.mha
    itkComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DFloatTest.mha
    float
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DFloatTest.mha
)
itk_add_test(
  NAME itkComplexToComplexFFTImageFilter2DDoubleTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DDoubleTest.mha
    itkComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DDoubleTest.mha
    double
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter2DDoubleTest.mha
)
itk_add_test(
  NAME itkComplexToComplexFFTImageFilter3DDoubleTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DDoubleTest.mha
    itkComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DDoubleTest.mha
    double
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplexFFTImageFilter3DDoubleTest.mha
)

itk_add_test(
  NAME itkPocketFFTComplexToComplexFFTImageFilter2DFloatTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter2DFloatTest.mha
    itkPocketFFTComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter2DFloatTest.mha
    float
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter2DFloatTest.mha
)
itk_add_test(
  NAME itkPocketFFTComplexToComplexFFTImageFilter3DFloatTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter3DFloatTest.mha
    itkPocketFFTComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter3DFloatTest.mha
    float
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter3DFloatTest.mha
)
itk_add_test(
  NAME itkPocketFFTComplexToComplexFFTImageFilter2DDoubleTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter2DDoubleTest.mha
    itkPocketFFTComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter2DDoubleTest.mha
    double
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter2DDoubleTest.mha
)
itk_add_test(
  NAME itkPocketFFTComplexToComplexFFTImageFilter3DDoubleTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter3DDoubleTest.mha
    itkPocketFFTComplexToComplexFFTImageFilterTest
    DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter3DDoubleTest.mha
    double
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplexFFTImageFilter3DDoubleTest.mha
)
if(ITK_USE_FFTWF)
  itk_add_test(
    NAME itkFFTWComplexToComplexFFTImageFilter2DFloatTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DFloatTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DFloatTest.mha
      float
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DFloatTest.mha
  )
  itk_add_test(
    NAME itkFFTWComplexToComplexFFTImageFilter3DFloatTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DFloatTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DFloatTest.mha
      float
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DFloatTest.mha
  )
endif()
if(ITK_USE_FFTWD)
  itk_add_test(
    NAME itkFFTWComplexToComplexFFTImageFilter2DDoubleTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DDoubleTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DDoubleTest.mha
      double
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter2DDoubleTest.mha
  )
  itk_add_test(
    NAME itkFFTWComplexToComplexFFTImageFilter3DDoubleTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DDoubleTest.mha
      itkFFTWComplexToComplexFFTImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/mri3D.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DDoubleTest.mha
      double
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplexFFTImageFilter3DDoubleTest.mha
  )
endif()

foreach(padMethod ZeroFluxNeumann Zero Wrap) # Mirror
  foreach(gpf 5 13)
    itk_add_test(
      NAME itkFFTPadImageFilterTest${padMethod}${gpf}
      COMMAND
        ITKFFTTestDriver
        --compare
        DATA{Baseline/itkFFTPadImageFilterTest${padMethod}${gpf}.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTPadImageFilterTest${padMethod}${gpf}.png
        itkFFTPadImageFilterTest
        DATA{${ITK_DATA_ROOT}/Input/cthead1-257.png}
        ${ITK_TEST_OUTPUT_DIR}/itkFFTPadImageFilterTest${padMethod}${gpf}.png
        ${gpf}
        ${padMethod}
    )
  endforeach()
endforeach()

# Test header files circular dependencies
add_executable(ITKFFTTestCircularDependency itkTestCircularDependency.cxx)
target_link_options(
  ITKFFTTestCircularDependency
  PRIVATE
    "$<$<AND:$<C_COMPILER_ID:AppleClang>,$<VERSION_GREATER_EQUAL:$<C_COMPILER_VERSION>,15.0>>:LINKER:-no_warn_duplicate_libraries>"
)
target_link_libraries(ITKFFTTestCircularDependency ${ITKFFT-Test_LIBRARIES})
itk_add_test(
  NAME itkFFTTestCircularDependency
  COMMAND
    ITKFFTTestCircularDependency
)

# Get test data input directory for test parameter
itk_add_test(
  NAME itkComplexToComplex1DFFTImageFilterTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplex1DFFTImageFilterTestOutput.mha
    itkComplexToComplex1DFFTImageFilterTest
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd}
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd}
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplex1DFFTImageFilterTestOutput.mha
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkComplexToComplex1DFFTImageFilterTestOutput.mha
)
# use a forward fft, then inverse fft for this instead because of the FullMatrix
# issue
itk_add_test(
  NAME itkFFT1DImageFilterTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFT1DImageFilterTestOutput.mha
    itkFFT1DImageFilterTest
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkFFT1DImageFilterTestOutput.mha
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkFFT1DImageFilterTestOutput.mha
)
itk_add_test(
  NAME itkPocketFFTComplexToComplex1DFFTImageFilterTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplex1DFFTImageFilterTestOutput.mhd
    itkComplexToComplex1DFFTImageFilterTest
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplex1DFFTImageFilterTestOutput.mhd
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTComplexToComplex1DFFTImageFilterTestOutput.{mhd,raw}
)
itk_add_test(
  NAME itkPocketFFTForward1DFFTImageFilterTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTForward1DFFTImageFilterTestOutputReal.mha
    --compare
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTForward1DFFTImageFilterTestOutputImaginary.mha
    itkForward1DFFTImageFilterTest
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTForward1DFFTImageFilterTestOutput
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTForward1DFFTImageFilterTestOutputReal.mha
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTForward1DFFTImageFilterTestOutputImaginary.mha
)
itk_add_test(
  NAME itkPocketFFTInverse1DFFTImageFilterTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTInverse1DFFTImageFilterTestOutput.mhd
    itkInverse1DFFTImageFilterTest
    DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
    DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTInverse1DFFTImageFilterTestOutput.mhd
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTInverse1DFFTImageFilterTestOutput.{mhd,raw}
)
itk_add_test(
  NAME itkPocketFFTFFT1DImageFilterTest
  COMMAND
    ITKFFTTestDriver
    --compare
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTFFT1DImageFilterTestOutput.mha
    itkFFT1DImageFilterTest
    DATA{Input/TreeBarkTexture.png}
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTFFT1DImageFilterTestOutput.mha
    1
  ITK_REMOVE_TEMPORARY_TEST_FILES
    ${ITK_TEST_OUTPUT_DIR}/itkPocketFFTFFT1DImageFilterTestOutput.mha
)

if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  itk_add_test(
    NAME itkFFTWForward1DFFTImageFilterTest
    COMMAND
      ITKFFTTestDriver
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
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWForward1DFFTImageFilterTestOutputReal.mha
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWForward1DFFTImageFilterTestOutputImaginary.mha
  )
  itk_add_test(
    NAME itkFFTWInverse1DFFTImageFilterTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWInverse1DFFTImageFilterTestOutput.mha
      itkInverse1DFFTImageFilterTest
      DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd,itkForward1DFFTImageFilterTestBaselineRealFull.raw}
      DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd,itkForward1DFFTImageFilterTestBaselineImaginaryFull.raw}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWInverse1DFFTImageFilterTestOutput.mha
      2
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWInverse1DFFTImageFilterTestOutput.mha
  )
  itk_add_test(
    NAME itkFFTWComplexToComplex1DFFTImageFilterTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplex1DFFTImageFilterTestOutput.mha
      itkComplexToComplex1DFFTImageFilterTest
      DATA{Input/itkForward1DFFTImageFilterTestBaselineRealFull.mhd}
      DATA{Input/itkForward1DFFTImageFilterTestBaselineImaginaryFull.mhd}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplex1DFFTImageFilterTestOutput.mha
      2
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTWComplexToComplex1DFFTImageFilterTestOutput.mha
  )
  itk_add_test(
    NAME itkFFTW1DImageFilterTest
    COMMAND
      ITKFFTTestDriver
      --compare
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTW1DImageFilterTestOutput.mha
      itkFFT1DImageFilterTest
      DATA{Input/TreeBarkTexture.png}
      ${ITK_TEST_OUTPUT_DIR}/itkFFTW1DImageFilterTestOutput.mha
      2
    ITK_REMOVE_TEMPORARY_TEST_FILES
      ${ITK_TEST_OUTPUT_DIR}/itkFFTW1DImageFilterTestOutput.mha
  )
endif()

# GTests for FFTW factory registration verification
if(ITK_USE_FFTWF OR ITK_USE_FFTWD)
  set(ITKFFTGTests itkFFTWFactoryRegistrationGTest.cxx)
  creategoogletestdriver(ITKFFT "${ITKFFT-Test_LIBRARIES}" "${ITKFFTGTests}")
endif()
