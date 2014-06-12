itk_module_test()
set(ITKFFTTests
itkFFTShiftImageFilterTest.cxx
itkHalfToFullHermitianImageFilterTest.cxx
itkFullToHalfHermitianImageFilterTest.cxx
itkVnlFFTTest.cxx
itkVnlRealFFTTest.cxx
itkForwardInverseFFTImageFilterTest.cxx
)

if (ITK_USE_FFTWF)
  set( ITKFFTTests ${ITKFFTTests}
    itkFFTWF_FFTTest.cxx
    itkFFTWF_RealFFTTest.cxx
    itkVnlFFTWF_FFTTest.cxx
    itkVnlFFTWF_RealFFTTest.cxx
)
endif()

if (ITK_USE_FFTWD)
  set( ITKFFTTests ${ITKFFTTests}
    itkFFTWD_FFTTest.cxx
    itkFFTWD_RealFFTTest.cxx
    itkVnlFFTWD_FFTTest.cxx
    itkVnlFFTWD_RealFFTTest.cxx
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
    COMMAND ITKFFTTestDriver itkFFTWF_FFTTest ${ITK_TEST_OUTPUT_DIR} )
  itk_add_test(NAME itkFFTWF_RealFFTTest
    COMMAND ITKFFTTestDriver itkFFTWF_RealFFTTest ${ITK_TEST_OUTPUT_DIR} )
  itk_add_test(NAME itkVnlFFTWF_FFTTest
    COMMAND ITKFFTTestDriver  itkVnlFFTWF_FFTTest )
  itk_add_test(NAME itkVnlFFTWF_RealFFTTest
    COMMAND ITKFFTTestDriver  itkVnlFFTWF_RealFFTTest )
  set_tests_properties(itkVnlFFTWF_FFTTest itkVnlFFTWF_RealFFTTest PROPERTIES ENVIRONMENT
    "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WRITE_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_FILE=${ITK_TEST_OUTPUT_DIR}/.wisdom_from_ITK_FFTW_WISDOM_CACHE_FILE;ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE")
endif()

if(ITK_USE_FFTWD)
  itk_add_test(NAME itkFFTWD_FFTTest
    COMMAND ITKFFTTestDriver itkFFTWD_FFTTest ${ITK_TEST_OUTPUT_DIR} )
  itk_add_test(NAME itkFFTWD_RealFFTTest
    COMMAND ITKFFTTestDriver itkFFTWD_RealFFTTest ${ITK_TEST_OUTPUT_DIR} )
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
