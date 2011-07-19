
set(WRAP_ITK_LIBRARIES ITKCommon)

# set Iterators library to OFF by default
# it is currently usable only when applying patches to , and will only
# take place and time to build
# TODO: remove this line when iterators will be usable without patch
option(WRAP_Iterators "Build Iterators library." OFF)

# Set FFTW to on if USE_FFTWF or USE_FFTWD are on.
if(USE_FFTWD OR USE_FFTWF)
  option(WRAP_FFTW "Build FFTW library." ON)
else(USE_FFTWD OR USE_FFTWF)
  option(WRAP_FFTW "Build FFTW library." OFF)
endif(USE_FFTWD OR USE_FFTWF)

set(optional_libraries
ITKStatistics
ITKTransform
ITKImageFunction
ITKImageFilterBase
ITKImageGrid
ITKFiniteDifference
ITKCurvatureFlow
ITKAnisotropicSmoothing
ITKMesh
ITKIOBase
ITKSpatialObjects
ITKImageCompose
ITKImageStatistics
ITKPath
ITKImageIntensity
ITKSmoothing
ITKImageGradient
ITKImageFeature
ITKThresholding
ITKConnectedComponents
ITKIOPNG
ITKMathematicalMorphology
ITKNarrowBand
ITKImageCompare
ITKQuadEdgeMesh
ITKFastMarching
ITKLevelSets
ITKAntiAlias
ITKIORAW
ITKClassifiers
ITKDeformationField
ITKDistanceMap
ITKEigen
ITKRegistrationCommon
ITKPDEDeformableRegistration
ITKFFT
ITKImageLabel
ITKIOBioRad
ITKIOBMP
ITKIOGDCM
ITKIOIPL
ITKIOGE
ITKIOGIPL
ITKIOJPEG
ITKIOTIFF
ITKIOLSM
ITKIOMeta
ITKIONIFTI
ITKIONRRD
ITKIOSiemens
ITKIOSpatialObjects
ITKIOStimulate
ITKIOVTK
ITKLabelVoting
ITKQuadEdgeMeshFiltering
ITKRegionGrowing
ITKVoronoi
ITKWatersheds
ITKIOHDF5
ITKReview
)

foreach(library ${optional_libraries})
  option(WRAP_${library} "Build ${library} library." ON)
  mark_as_advanced(WRAP_${library})
  if(WRAP_${library})
    set(WRAP_ITK_LIBRARIES ${WRAP_ITK_LIBRARIES} ${library})
  endif(WRAP_${library})
endforeach(library)
set(WRAP_ITK_LIBRARIES ${WRAP_ITK_LIBRARIES} CACHE INTERNAL "Internal library list.")

foreach(library ${WRAP_ITK_LIBRARIES} ${WRAP_ITK_EXTRA_LIBRARIES})
  add_subdirectory(${library})
endforeach(library)
