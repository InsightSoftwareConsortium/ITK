
PROJECT(IOExamples)

ADD_EXECUTABLE(RGBImageReadWrite RGBImageReadWrite.cxx )
TARGET_LINK_LIBRARIES(RGBImageReadWrite ITKCommon ITKIO ITKMetaIO itkzlib itkpng)



