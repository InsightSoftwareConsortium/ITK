
PROJECT(ImageExamples)

ADD_EXECUTABLE(Image1 Image1.cxx )
TARGET_LINK_LIBRARIES(Image1 ITKCommon)

ADD_EXECUTABLE(Image2 Image2.cxx )
TARGET_LINK_LIBRARIES(Image2 ITKCommon ITKIO ITKMetaIO itkzlib itkpng)


