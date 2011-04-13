# For now, depend on Video-IO-Base, but may reverse if we implement
# in Video-Common itk::ImageSetSource
itk_module(Video-Common DEPENDS ITK-Common TEST_DEPENDS ITK-TestKernel Video-IO-FileList)
