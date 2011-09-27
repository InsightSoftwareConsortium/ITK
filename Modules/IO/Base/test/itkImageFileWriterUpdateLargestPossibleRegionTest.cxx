/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriterPastingTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkImageFileWriterUpdateLargestPossibleRegionTest(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
    }

  // We remove the output file
  itksys::SystemTools::RemoveFile(argv[2]);

  typedef unsigned char             PixelType;
  typedef itk::Image<PixelType,2>   ImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  typedef itk::ImageFileWriter< ImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(reader->GetOutput());
  writer->SetFileName( argv[2] );

  ImageType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
  ImageType::IndexType index = region.GetIndex();
  ImageType::SizeType size = region.GetSize();

  itk::ImageIORegion ioregion(2);
  ioregion.SetIndex(0, index[0]);
  ioregion.SetIndex(1, index[1]);
  ioregion.SetSize(0, size[0]/2);
  ioregion.SetSize(1, size[1]/2);

  writer->SetIORegion(ioregion);

  // using Update() should fail because the paste feature is not supported by the png writer
  int status = 1;
  try
    {
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

  // but it should succeed with UpdateLargestPossibleRegion() because the paste region
  // is not used
  try
    {
    writer->UpdateLargestPossibleRegion();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
