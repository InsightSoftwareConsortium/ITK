itk_module_test()

if(ITK_USE_GPU AND WITH_OPENCL)
  add_definitions(-DGPU)

  if(GPU_HANDLES_DOUBLE)
    add_definitions(-DWITH_DOUBLE)
  endif()

  if(NVIDIA_GPU)
    add_definitions(-DNVIDIA)
    message(STATUS "NVIDIA platform detected")
  endif()

  include_directories(
    "${ITKSmoothingRecursiveYvvGaussianFilter_SOURCE_DIR}/include"
  )
  include_directories("${ITKSmoothingRecursiveYvvGaussianFilterKernelDir}")

  set(
    ITK${itk-module}Tests
    itkCPURecursiveYvvGaussianImageFilterTest.cxx
    itkGPURecursiveYvvGaussianImageFilterTest.cxx
    itkYvvGpuCpuSimilarityTest.cxx
    itkYvvBenchmark.cxx
    itkYvvWhiteImageTest.cxx
  )
else()
  set(
    ITK${itk-module}Tests
    itkCPURecursiveYvvGaussianImageFilterTest.cxx
    itkYvvBenchmark.cxx
  )
endif()

set(TEST_DATA_ROOT "${${itk-module}_SOURCE_DIR}/Data")

createtestdriver(${itk-module} "${${itk-module}-Test_LIBRARIES}" "${ITK${itk-module}Tests}")

itk_add_test(
  NAME itkCPURecursiveYvvGaussianImageFilterTest
  COMMAND
    ${itk-module}TestDriver
    itkCPURecursiveYvvGaussianImageFilterTest
    ${TEST_DATA_ROOT}/1024ex.jpg
    2
    12.0
    20
)

itk_add_test(
  NAME itkYvvBenchmark
  COMMAND
    ${itk-module}TestDriver
    itkYvvBenchmark
    ${TEST_DATA_ROOT}/1024ex.jpg
    2
    12.0
    30
)

itk_add_test(
  NAME itkYvvWhiteImageTest2D
  COMMAND
    ${itk-module}TestDriver
    itkYvvWhiteImageTest
    2
    12.0
    80
    512
    512
)

itk_add_test(
  NAME itkYvvWhiteImageTest3D
  COMMAND
    ${itk-module}TestDriver
    itkYvvWhiteImageTest
    3
    12.0
    5
    256
    256
    16
)

if(ITK_USE_GPU AND WITH_OPENCL)
  itk_add_test(
    NAME itkGPURecursiveYvvGaussianImageFilterTest
    COMMAND
      ${itk-module}TestDriver
      itkGPURecursiveYvvGaussianImageFilterTest
      ${TEST_DATA_ROOT}/1024ex.jpg
      2
      12.0
      1
  )

  itk_add_test(
    NAME itkYvvGpuCpuSimilarityTest
    COMMAND
      ${itk-module}TestDriver
      itkYvvGpuCpuSimilarityTest
      ${TEST_DATA_ROOT}/1024ex.jpg
      2
      0.5
  )
endif()
