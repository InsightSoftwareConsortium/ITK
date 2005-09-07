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

INSTALL_TARGETS(/lib/InsightToolkit ITKAlgorithms)
INSTALL_FILES(/include/InsightToolkit/Algorithms "(\\.h|\\.txx)$")
