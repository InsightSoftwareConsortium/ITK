# test regular image to RLE conversion
itk_python_expression_add_test(NAME itkRegionOfInterestImageFilterISS2RLEImageSS2USPythonTest
  EXPRESSION "instance = itk.RegionOfInterestImageFilterISS2RLEImageSS2US.New()")

# test RLE to regular image conversion
itk_python_expression_add_test(NAME itkRegionOfInterestImageFilterRLEImageSS2USISS2PythonTest
  EXPRESSION "instance = itk.RegionOfInterestImageFilterRLEImageSS2USISS2.New()")

# test RLE to RLE (plain RoI operation)
itk_python_expression_add_test(NAME itkRegionOfInterestImageFilterRLEImageSS2USRLEImageSS2USPythonTest
  EXPRESSION "instance = itk.RegionOfInterestImageFilterRLEImageSS2USRLEImageSS2US.New()")
