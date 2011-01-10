add_library(ITKAlgorithms ${ITK_LIBRARY_BUILD_TYPE}
  itkFFTWGlobalConfiguration.cxx
  itkWatershedMiniPipelineProgressCommand.cxx
  itkBioCellBase.cxx
  itkBioCellularAggregateBase.cxx
  itkBioGenome.cxx
  itkBioGene.cxx
  itkBioGeneNetwork.cxx
)
target_link_libraries(ITKAlgorithms ITKStatistics ITKNumerics)
if(USE_FFTWD)
        #If FFTW is desired, then fftw3 and fftw3f will be required by any
        #application using the ITKAlgorithms lib
        add_dependencies(ITKAlgorithms fftwd)
        target_link_libraries(ITKAlgorithms fftw3 fftw3_threads)
endif(USE_FFTWD)
if(USE_FFTWF)
        #If FFTW single precision is desired, then fftw3f will be required by any
        #application using the ITKAlgorithms lib
        add_dependencies(ITKAlgorithms fftwf)
        target_link_libraries(ITKAlgorithms fftw3f fftw3f_threads )
endif(USE_FFTWF)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKAlgorithms PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKAlgorithms
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_ALGORITHMS ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_ALGORITHMS ${ITK_INSTALL_INCLUDE_DIR_CM24}/Algorithms)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  install(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_ALGORITHMS}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
