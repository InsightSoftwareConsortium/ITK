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

  if (argc < 4)
    {
    std::cout << "usage: itkIOTests itkImageFileReaderTest inputFileName outputDirectory outputExtension" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short, 3> Image3DType;
  typedef itk::Image<short, 4> Image4DType;
  typedef itk::ImageFileReader<Image3DType> Reader3DType;
  typedef itk::ImageFileReader<Image4DType> Reader4DType;

  typedef itk::ImageFileWriter<Image3DType> Writer3DType;
  typedef itk::ImageFileWriter<Image4DType> Writer4DType;


  std::string tempFile1 = std::string( argv[2] ) + std::string( "itkImageFileReaderTest2_1." ) + std::string( argv[3] );
  std::string tempFile2 = std::string( argv[2] ) + std::string( "itkImageFileReaderTest2_2." ) + std::string( argv[3] );
  std::string tempFile3 = std::string( argv[2] ) + std::string( "itkImageFileReaderTest2_3." ) + std::string( argv[3] );
  std::string tempFile4 = std::string( argv[2] ) + std::string( "itkImageFileReaderTest2_4." ) + std::string( argv[3] );
  std::string tempFile5 = std::string( argv[2] ) + std::string( "itkImageFileReaderTest2_5." ) + std::string( argv[3] );

  // we expect the filename to be 2 or 3 dimensions
  // and reading it into a 4D
  std::cout << "testing reading to 4D image" << std::endl;
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(argv[1]);

    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile1);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  // read the new 4D file into a 3D file
  std::cout << "testing reading from 4D image into 3D" << std::endl;
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    // we expect the filename to be 2 or 3 dimensions
    reader->SetFileName(tempFile1);
   

    Writer3DType::Pointer writer = Writer3DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile2);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  // stream read new 3D file into 4D
  std::cout << "testing requested stream reading from 3D image into 4D" << std::endl;
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(tempFile2);

    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile3);
    writer->SetNumberOfStreamDivisions(4);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  // stream read new 4D file into 3D
  std::cout << "testing requested stream reading from 4D image into 3D" << std::endl;
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    reader->SetFileName(tempFile3);
    reader->Update();

    Writer3DType::Pointer writer = Writer3DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile4);
    writer->SetNumberOfStreamDivisions(4);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  int status = 1;
  // read the 4D file into a 4D image, then try to stream it as a 3D
  // IORegion   
  std::cout << "testing requested invalid paste IORegion" << std::endl;
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(tempFile4);
    reader->Update();
    
    Image4DType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
    
    // the dimension of this ioregion is an error
    itk::ImageIORegion ioregion(3);
    for (unsigned int i = 0; i < 3; ++i)
      {
      ioregion.SetIndex(i, 0);
      ioregion.SetSize(i, region.GetSize(i));
      }


    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile5);
    writer->SetIORegion(ioregion);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {    
    // if the writer doesn't support pasting an exception will be
    // thrown too
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
