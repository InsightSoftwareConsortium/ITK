/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest3.cxx
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
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRawImageIO.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

int itkRawImageIOTest3(int, char*[])
{
  typedef itk::Image<unsigned short,2>    ImageType;
  typedef ImageType::PixelType            PixelType;

  typedef itk::ImageRegionIterator< 
                                  ImageType > ImageIteratorType;

  typedef itk::ImageRegionConstIterator< 
                                  ImageType > ImageConstIteratorType;

  typedef itk::RawImageIO<PixelType,
                          ImageType::ImageDimension> RawImageIOType;


  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  ImageType::SizeType size;
  size[0]=517;  // prime numbers are good bug testers...
  size[1]=293;
  
  ImageType::RegionType region;
  ImageType::IndexType  index;
  index.Fill(0);

  region.SetIndex( index );
  region.SetSize(size);
  
  ImageType::Pointer image = ImageType::New();
  image->SetRegions( region );
  image->Allocate();

  ImageIteratorType ii( image, region );

  PixelType value = itk::NumericTraits< PixelType >::Zero;
  ii.GoToBegin();
  while( !ii.IsAtEnd() )
    {
    ii.Set( value );
    ++value;
    ++ii;
    }

  RawImageIOType::Pointer io = RawImageIOType::New();
  io->SetByteOrderToBigEndian();

  // Write out the image
  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput( image );
  writer->SetFileName("junk.raw");
  writer->SetImageIO(io);
  writer->Write();

  // Create a source object (in this case a reader)
  itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(io);
  reader->SetFileName("junk.raw");
  reader->Update();

  // Compare pixel by pixel in memory


  ImageConstIteratorType it( reader->GetOutput(), 
                             reader->GetOutput()->GetBufferedRegion() );

  ImageConstIteratorType ot( image,
                             image->GetBufferedRegion() );

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
  writer->SetFileName("junk2.raw");
  writer->SetInput(reader->GetOutput());
  writer->Write();

  std::cerr << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}



