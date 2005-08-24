ADD_LIBRARY(ITKAlgorithms
  itkWatershedMiniPipelineProgressCommand.cxx
  itkBioCellBase.cxx
  itkBioCellularAggregateBase.cxx
  itkBioGenome.cxx
  itkBioGene.cxx
  itkBioGeneNetwork.cxx
)
TARGET_LINK_LIBRARIES(ITKAlgorithms ITKNumerics ITKStatistics)
IF(USE_FFTW)
        #If FFTW is desired, then fftw3 and fftw3f will be required by any
        #application using the ITKAlgorithms lib
        TARGET_LINK_LIBRARIES(ITKAlgorithms fftw3 fftw3f)
ENDIF(USE_FFTW)

IF(USE_SCSL)
        #If SCSL is desired, then scs will be required by any
        #application using the ITKAlgorithms lib
        TARGET_LINK_LIBRARIES(ITKAlgorithms scs )
ENDIF(USE_SCSL)

INSTALL_TARGETS(/lib/InsightToolkit ITKAlgorithms)
INSTALL_FILES(/include/InsightToolkit/Algorithms "(\\.h|\\.txx)$")
