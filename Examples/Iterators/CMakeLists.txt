#
#  Examples on the use of Iterators.
#
PROJECT(IteratorExamples)

ADD_EXECUTABLE(ImageRegionIterator ImageRegionIterator.cxx)
TARGET_LINK_LIBRARIES(ImageRegionIterator ITKCommon ITKIO ITKMetaIO itkzlib itkpng)
ADD_EXECUTABLE(ImageRegionIteratorWithIndex ImageRegionIteratorWithIndex.cxx)
TARGET_LINK_LIBRARIES(ImageRegionIteratorWithIndex ITKCommon ITKIO ITKMetaIO itkzlib itkpng)
