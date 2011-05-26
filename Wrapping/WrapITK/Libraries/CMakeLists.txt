
set(WRAP_ITK_LIBRARIES
  VXLNumerics
  Base
)

# set Iterators library to OFF by default
# it is currently usable only when applying patches to ITK, and will only
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
  Numerics
  Transforms
  SpatialObject
  BinaryMorphology
  Calculators
  Compose
  Interpolators  # is required by DeformableTransforms
  Mesh  # is required by DeformableTransforms
  DeformableTransforms
  Denoising
  DistanceMap
  EdgesAndContours
  FFT
  FFTW
  Filtering
  IntensityFilters
  IO
  Iterators
  LevelSet
  Morphology
  #Patented
  PixelMath
  Resize  # required by Registration
  Registration
  Review
  SegmentationAndThreshold
  SegmentationValidation
  SimpleFilters
  UnaryPixelMath
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
