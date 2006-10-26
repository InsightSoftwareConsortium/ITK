ADD_LIBRARY(ITKAlgorithms
  itkWatershedMiniPipelineProgressCommand.cxx
  itkBioCellBase.cxx
  itkBioCellularAggregateBase.cxx
  itkBioGenome.cxx
  itkBioGene.cxx
  itkBioGeneNetwork.cxx
)
TARGET_LINK_LIBRARIES(ITKAlgorithms ITKNumerics ITKStatistics)
IF(USE_FFTWD)
        #If FFTW is desired, then fftw3 and fftw3f will be required by any
        #application using the ITKAlgorithms lib
        TARGET_LINK_LIBRARIES(ITKAlgorithms ${FFTWD_LIB} )
ENDIF(USE_FFTWD)
IF(USE_FFTWF)
        #If FFTW single precision is desired, then fftw3f will be required by any
        #application using the ITKAlgorithms lib
        TARGET_LINK_LIBRARIES(ITKAlgorithms ${FFTWF_LIB} )
ENDIF(USE_FFTWF)

IF(USE_SCSL)
        #If SCSL is desired, then scs will be required by any
        #application using the ITKAlgorithms lib
        TARGET_LINK_LIBRARIES(ITKAlgorithms ${SCSL_LIB} )
ENDIF(USE_SCSL)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKAlgorithms PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKAlgorithms
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  FILE(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  INSTALL(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/Algorithms
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
