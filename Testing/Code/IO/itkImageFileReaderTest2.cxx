/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReaderTest2.cxx
  Language:  C++
  Date:      $Date$xgoto-l

  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

// This test is designed to test reading and writing of miss matched
// dimensions

int itkImageFileReaderTest2(int argc, char* argv[])
{

  if (argc < 3)
    {
    std::cout << "usage: itkIOTests itkImageFileReaderTest inputFileName outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short, 3> Image3DType;
  typedef itk::Image<short, 4> Image4DType;
  typedef itk::ImageFileReader<Image3DType> Reader3DType;
  typedef itk::ImageFileReader<Image4DType> Reader4DType;

  typedef itk::ImageFileWriter<Image3DType> Writer3DType;
  typedef itk::ImageFileWriter<Image4DType> Writer4DType;


  // we expect the filename to be 2 or 3 dimensions
  // and reading it into a 4D
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(argv[1]);

    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(argv[2]);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  // read the new 4D file into a 3D file
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    // we expect the filename to be 2 or 3 dimensions
    reader->SetFileName(argv[2]);
    reader->Update();

    Writer3DType::Pointer writer = Writer3DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(argv[2]);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  int status = 1;
  // reader the 4D file into a 4D image, then try to stream it as a 3D
  // IORegion 
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(argv[2]);
    reader->Update();
    
    Image4DType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
    
    itk::ImageIORegion ioregion(3);
    for (unsigned int i = 0; i < 3; ++i)
      {
      ioregion.SetIndex(i, 0);
      ioregion.SetSize(i, region.GetSize(i));
      }


    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(argv[2]);
    writer->SetIORegion(ioregion);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {    
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
    }

  if (status) 
    {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
    }
  

  return EXIT_SUCCESS;

}
