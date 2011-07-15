itk_module_test()
set(ITKFFTTests
itkFFTHeaderTest.cxx
itkVnlFFTTest.cxx
)

if (USE_FFTWF)
  set( ITKFFTTests ${ITKFFTTests} itkFFTWF_FFTTest.cxx itkVnlFFTWF_FFTTest.cxx)
endif()

if (USE_FFTWD)
  set( ITKFFTTests ${ITKFFTTests} itkFFTWD_FFTTest.cxx itkVnlFFTWD_FFTTest.cxx)
endif()


CreateTestDriver(ITKFFT  "${ITKFFT-Test_LIBRARIES}" "${ITKFFTTests}")

itk_add_test(NAME itkFFTHeaderTest
      COMMAND ITKFFTTestDriver itkFFTHeaderTest)

itk_add_test(NAME itkVnlFFTTest
      COMMAND ITKFFTTestDriver itkVnlFFTTest)

if(USE_FFTWF)
       itk_add_test(NAME itkFFTWF_FFTTest
             COMMAND ITKFFTTestDriver itkFFTWF_FFTTest ${ITK_TEST_OUTPUT_DIR} )
       itk_add_test(NAME itkVnlFFTWF_FFTTest
             COMMAND ITKFFTTestDriver  itkVnlFFTWF_FFTTest )
       set_tests_properties(itkVnlFFTWF_FFTTest PROPERTIES ENVIRONMENT
         "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WRITE_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_FILE=${ITK_TEST_OUTPUT_DIR}/.wisdom_from_ITK_FFTW_WISDOM_CACHE_FILE;ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE"
         )
endif(USE_FFTWF)

if(USE_FFTWD)
       itk_add_test(NAME itkFFTWD_FFTTest
             COMMAND ITKFFTTestDriver itkFFTWD_FFTTest ${ITK_TEST_OUTPUT_DIR} )

       itk_add_test(NAME itkVnlFFTWD_FFTTest
             COMMAND  ITKFFTTestDriver  itkVnlFFTWD_FFTTest)
       set_tests_properties(itkVnlFFTWD_FFTTest PROPERTIES ENVIRONMENT
         "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_BASE=${ITK_TEST_OUTPUT_DIR};ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE;ITK_FFTW_WRITE_WISDOM_CACHE=oN")
endif(USE_FFTWD)
