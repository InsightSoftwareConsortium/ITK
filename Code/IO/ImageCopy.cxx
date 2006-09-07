
#include "itkWin32Header.h"
#include <iostream>
#include <fstream>
#include "itkNumericTraits.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


int main(int argc, char **argv)
{
  if( argc < 2 )
    {
    std::cerr << "You must supply a filename to be copied" << std::endl;
    return 1;
    }

  const unsigned int Dimension = 2;

  typedef itk::Image< unsigned char, Dimension > ImageType;
  typedef itk::Image< unsigned char, Dimension > OutputType;
  typedef itk::Image< unsigned char, Dimension > DiffOutputType;

  typedef itk::ImageFileReader< ImageType >      ReaderType;
  typedef itk::ImageFileWriter< OutputType >     WriterType;

  // Read the baseline file
  ReaderType::Pointer baselineReader = ReaderType::New();

  baselineReader->SetFileName(argv[1]);
 
  try
    {
    baselineReader->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected while reading " << argv[1];
    std::cerr << " : "  << e.GetDescription();
    return 1;
    }

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(baselineReader->GetOutput());

  ::itk::OStringStream baseName;

  baseName << argv[1] << ".base.png";

  try
    {
    writer->SetFileName( baseName.str().c_str() );
    writer->Update();
    }
  catch (...)
    {
    std::cerr << "Error during write of " << baseName.str() << std::endl;
    return 1;
    }

  return 0;
}


