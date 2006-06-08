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
  INSTALL_TARGETS(
    ${ITK_INSTALL_LIB_DIR}
    RUNTIME_DIRECTORY ${ITK_INSTALL_BIN_DIR}
    ITKAlgorithms)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${ITK_INSTALL_INCLUDE_DIR}/Algorithms "(\\.h|\\.txx)$")
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
