
PROJECT(IOExamples)

ADD_EXECUTABLE(RGBImageReadWrite RGBImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(RGBImageReadWrite ITKCommon ITKIO ITKMetaIO itkzlib itkpng)

ADD_EXECUTABLE(ImageReadWrite ImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(ImageReadWrite ITKCommon ITKIO ITKMetaIO itkzlib itkpng)


