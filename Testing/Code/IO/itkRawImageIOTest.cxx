/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
//#include <iostream>
#include <fstream>
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRawImageIO.h"
#include "itkImageRegionConstIterator.h"

int itkRawImageIOTest(int argc, char* argv[])
{
  typedef itk::Image<unsigned short,2>    ImageType;
  typedef ImageType::PixelType            PixelType;
  typedef itk::ImageRegionConstIterator< 
                                  ImageType > ImageIteratorType;
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " Output1 Output2\n";
    return EXIT_FAILURE;
    }
  
  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  unsigned long size[2];
  size[0]=128; size[1]=64;
  
  itk::RandomImageSource<ImageType>::Pointer random;
  random = itk::RandomImageSource<ImageType>::New();
  random->SetMin(0);
  random->SetMax(24680);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::RawImageIO<unsigned short,2>::Pointer io;
  io = itk::RawImageIO<unsigned short,2>::New();\
//  io->SetFileTypeToASCII();

  // Write out the image
  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName(argv[1]);
  writer->SetImageIO(io);
  writer->Write();

  // Create a source object (in this case a reader)
  itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(io);
  reader->SetFileName(argv[1]);
  reader->Update();

  // Compare pixel by pixel in memory


  ImageIteratorType it( reader->GetOutput(), 
                        reader->GetOutput()->GetBufferedRegion() );

  ImageIteratorType ot( random->GetOutput(), 
                        random->GetOutput()->GetBufferedRegion() );

  it.GoToBegin();
  ot.GoToBegin();
  while( !it.IsAtEnd() )
    {
    const PixelType iv = it.Get();
    const PixelType ov = ot.Get();
    if( iv != ov ) 
      {
      std::cerr << "Error in read/write of pixel " << it.GetIndex() << std::endl;
      std::cerr << "Read value  is : " << iv << std::endl;
      std::cerr << "it should be   : " << ov << std::endl;
      std::cerr << "Test FAILED ! " << std::endl;
      return EXIT_FAILURE;
      }
    ++it;
    ++ot;
    }

  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());
  writer->Write();

  std::cerr << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}



