project(ITKv3ImageRegistration)

add_executable(ITKv3ImageRegistration3 ImageRegistration3.cxx)
target_link_libraries(ITKv3ImageRegistration3 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration5 ImageRegistration5.cxx)
target_link_libraries(ITKv3ImageRegistration5 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration6 ImageRegistration6.cxx)
target_link_libraries(ITKv3ImageRegistration6 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration8 ImageRegistration8.cxx)
target_link_libraries(ITKv3ImageRegistration8 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration9 ImageRegistration9.cxx)
target_link_libraries(ITKv3ImageRegistration9 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3DeformableRegistration4 DeformableRegistration4.cxx)
target_link_libraries(ITKv3DeformableRegistration4 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration4 ImageRegistration4.cxx)
target_link_libraries(ITKv3ImageRegistration4 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration7 ImageRegistration7.cxx)
target_link_libraries(ITKv3ImageRegistration7 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration11 ImageRegistration11.cxx)
target_link_libraries(ITKv3ImageRegistration11 "${ITKRegistrationCommon-Test_LIBRARIES}")

# This example in getting stuck and timing out, likely due to spatial
# object changes.
if (TEST_DISABLED)
add_executable(ITKv3ImageRegistration12 ImageRegistration12.cxx)
target_link_libraries(ITKv3ImageRegistration12 "${ITKRegistrationCommon-Test_LIBRARIES}")
endif()

add_executable(ITKv3ImageRegistration13 ImageRegistration13.cxx)
target_link_libraries(ITKv3ImageRegistration13 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration14 ImageRegistration14.cxx)
target_link_libraries(ITKv3ImageRegistration14 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3DeformableRegistration6 DeformableRegistration6.cxx)
target_link_libraries(ITKv3DeformableRegistration6 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3DeformableRegistration7 DeformableRegistration7.cxx)
target_link_libraries(ITKv3DeformableRegistration7 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3DeformableRegistration8 DeformableRegistration8.cxx)
target_link_libraries(ITKv3DeformableRegistration8 "${ITKRegistrationCommon-Test_LIBRARIES}")

if(ITK_USE_FFTWD AND NOT ITK_USE_CUFFTW)
  add_executable(ITKv3DeformableRegistration10 DeformableRegistration10.cxx)
  target_link_libraries(ITKv3DeformableRegistration10 "${ITKRegistrationCommon-Test_LIBRARIES}")
endif()

add_executable(ITKv3MultiResImageRegistration1 MultiResImageRegistration1.cxx)
target_link_libraries(ITKv3MultiResImageRegistration1 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3DeformableRegistration12 DeformableRegistration12.cxx)
target_link_libraries(ITKv3DeformableRegistration12 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3ImageRegistration1 ImageRegistration1.cxx)
target_link_libraries(ITKv3ImageRegistration1 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx)
target_link_libraries(ITKv3MeanSquaresImageMetric1 "${ITKRegistrationCommon-Test_LIBRARIES}")

add_executable(ITKv3DeformationFieldJacobian DeformationFieldJacobian.cxx)
target_link_libraries(ITKv3DeformationFieldJacobian "${ITKRegistrationCommon-Test_LIBRARIES}")

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
