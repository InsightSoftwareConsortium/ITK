#include <iostream>
#include "itkImageFileReader.h"

#include "itkFDFImageIOFactory.h"
#include "itkFDFImageIO.h"

#include "itkImage.h"

int
itkFDFImageIOTest(int argc, char ** argv)
{
  if (argc < 3)
  {
    std::cerr << "Usage: itkFDFImageIO <output_directory> <inputfile" << std::endl;
    return EXIT_FAILURE;
  }
  typedef float      PixelType;
  const unsigned int Dimension = 2;

  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  // Register FDF Factory
  itk::FDFImageIOFactory::RegisterOneFactory();

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[2]);

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & exp)
  {
    std::cerr << "Exception caught" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
  }
  ImageType::Pointer im = reader->GetOutput();

  std::cerr << im->GetDirection() << std::endl << im->GetOrigin() << std::endl << im->GetSpacing() << std::endl;

  return EXIT_SUCCESS;
}
