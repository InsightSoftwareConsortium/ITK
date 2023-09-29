set(DOCUMENTATION
    "This module contains base classes for IO, helper classes for
IO, and classes that function as an ImageSource in an ITK pipeline. Classes for
specific file formats, found in other modules in the IO group, should inherit
from itk::ImageIOBase or itk::StreamingImageIOBase. For an image source or sink in the ITK
pipeline that handles all available file formats, see ImageFileReader,
itk::ImageFileWriter, itk::ImageSeriesReader, or itk::ImageSeriesWriter. Convenience classes
for reading series of files include itk::ArchetypeSeriesFileNames,
itk::NumericSeriesFileNames, and itk::RegularExpressionSeriesFileNames.")

itk_module(
  ITKIOImageBase
  ENABLE_SHARED
  DEPENDS
  ITKCommon
  TEST_DEPENDS
  ITKTestKernel
  ITKIOGDCM
  ITKIOMeta
  ITKImageIntensity
  DESCRIPTION
  "${DOCUMENTATION}")
