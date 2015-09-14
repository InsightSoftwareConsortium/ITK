set(DOCUMENTATION "This module contains base classes for IO, helper classes for
IO, and classes that function as an ImageSource in an ITK pipeline.  Classes for
specific file formats, found in other modules in the IO group, should inherit
from ImageIOBase or StreamingImageIOBase.  For an image source or sink in the ITK
pipeline that handles all available file formats, see ImageFileReader,
ImageFileWriter, ImageSeriesReader, or ImageSeriesWriter.  Convenience classes
for reading series of files include ArchetypeSeriesFileNames,
NumericSeriesFileNames, and RegularExpressionSeriesFileNames.")

itk_module(ITKIOImageBase
  ENABLE_SHARED
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
    ITKGDCM
    ITKImageIntensity
  DESCRIPTION
    "${DOCUMENTATION}"
)
