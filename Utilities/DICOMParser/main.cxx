

#ifdef WIN32
#pragma warning(disable:4786)
#endif

#include <iostream>
#include <vector>

#include "DICOMParser.h"
#include "DICOMCallback.h"
#include "DICOMAppHelper.h"

#include "itkImage.h"
#include "itkDICOMImageIO2.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkDICOMImageIO2Factory.h"
#include "itkPNGImageIO.h"
#include "itkExceptionObject.h"
#include "itkRawImageWriter.h"

#include <stdlib.h>


int main(int argc, char* argv[])
{
    
  if (argc < 2)
    {
    std::cout << std::endl;
    std::cout << "Usage: DICOMApp file1 file2..." << std::endl;
    std::cout << std::endl;
    std::cout << "==================================================================" << std::endl;
    std::cout << "Image data will be written to file1.raw file2.raw and file1.png file2.png" << std::endl;
    std::cout << "WARNING (to be fixed) - PNG images will not be correct for little endian data." << std::endl;
    std::cout << "Some header values will be written to file1.header.txt file2.header.txt" << std::endl;
    std::cout << "==================================================================" << std::endl;
    std::cout << std::endl;
    std::cout.flush();
    return EXIT_FAILURE;
    }
  
  itk::ObjectFactoryBase::RegisterFactory(itk::DICOMImageIO2Factory::New());
  
  itk::DICOMImageIO2::Pointer DICOMImage = 
    itk::DICOMImageIO2::New();

  typedef itk::Image<unsigned short, 2 > imgType;

  itk::PNGImageIO::Pointer pngThing = itk::PNGImageIO::New();
  
  itk::ImageFileReader<imgType>::Pointer fileReader  = itk::ImageFileReader<imgType>::New();

  itk::ImageFileWriter<imgType>::Pointer pngWriter = itk::ImageFileWriter<imgType>::New();
  pngWriter->SetImageIO(pngThing);
  pngWriter->SetInput(fileReader->GetOutput());
  
  itk::RawImageWriter<imgType>::Pointer rawWriter = itk::RawImageWriter<imgType>::New();
  rawWriter->SetInput(fileReader->GetOutput());

  try {
  for (int i = 1; i < argc; i++)
    {
    const char* filename = argv[i];
    
    std::cout << std::endl;
    std::cout << "========== Reading " << filename << " ===========" << std::endl;
    std::cout << std::endl;

    fileReader->SetFileName(filename);
    fileReader->UpdateLargestPossibleRegion();
    
    std::string rawFilename = std::string(filename) + ".raw";
    rawWriter->SetFileName(rawFilename.c_str());
    rawWriter->SetFileTypeToBinary();
    rawWriter->SetByteOrderToLittleEndian();
    std::cout << std::endl;
    std::cout << "Writing: " << rawFilename << std::endl;
    rawWriter->Write();
    std::cout << std::endl;

    std::string outputFilename = std::string(filename) + ".png";
    pngWriter->SetFileName(outputFilename.c_str());
    std::cout << std::endl;
    std::cout << "Writing: " << outputFilename << std::endl;

    pngWriter->Write();
    }
  }  
  catch (itk::ExceptionObject e)
  {
    std::cerr << e << std::endl;
  }
  return EXIT_SUCCESS;
}


