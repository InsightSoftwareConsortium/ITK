itk_module_test()
set(ITK-FFTTests
itkFFTHeaderTest.cxx
itkVnlFFTTest.cxx
)

if (USE_FFTWF)
  set( ITK-FFTTests ${ITK-FFTTests} itkFFTWF_FFTTest.cxx itkVnlFFTWF_FFTTest.cxx)
endif()

if (USE_FFTWD)
  set( ITK-FFTTests ${ITK-FFTTests} itkFFTWD_FFTTest.cxx itkVnlFFTWD_FFTTest.cxx)
endif()


CreateTestDriver(ITK-FFT  "${ITK-FFT-Test_LIBRARIES}" "${ITK-FFTTests}")

itk_add_test(NAME itkFFTHeaderTest
      COMMAND ITK-FFTTestDriver itkFFTHeaderTest)

itk_add_test(NAME itkVnlFFTTest
      COMMAND ITK-FFTTestDriver itkVnlFFTTest)

if(USE_FFTWF)
       itk_add_test(NAME itkFFTWF_FFTTest
             COMMAND ITK-FFTTestDriver itkFFTWF_FFTTest ${ITK_TEST_OUTPUT_DIR} )
       itk_add_test(NAME itkVnlFFTWF_FFTTest
             COMMAND ITK-FFTTestDriver  itkVnlFFTWF_FFTTest )
       set_tests_properties(itkVnlFFTWF_FFTTest PROPERTIES ENVIRONMENT
         "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WRITE_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_FILE=${ITK_TEST_OUTPUT_DIR}/.wisdom_from_ITK_FFTW_WISDOM_CACHE_FILE;ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE"
         )
endif(USE_FFTWF)

if(USE_FFTWD)
       itk_add_test(NAME itkFFTWD_FFTTest
             COMMAND ITK-FFTTestDriver itkFFTWD_FFTTest ${ITK_TEST_OUTPUT_DIR} )

       itk_add_test(NAME itkVnlFFTWD_FFTTest
             COMMAND  ITK-FFTTestDriver  itkVnlFFTWD_FFTTest)
       set_tests_properties(itkVnlFFTWD_FFTTest PROPERTIES ENVIRONMENT
         "ITK_FFTW_READ_WISDOM_CACHE=oN;ITK_FFTW_WISDOM_CACHE_BASE=${ITK_TEST_OUTPUT_DIR};ITK_FFTW_PLAN_RIGOR=FFTW_EXHAUSTIVE;ITK_FFTW_WRITE_WISDOM_CACHE=oN")
endif(USE_FFTWD)
