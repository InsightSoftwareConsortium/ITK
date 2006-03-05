/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTileImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkTileImageFilter.h"
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkOrientedImage.h"

int itkTileImageFilterTest(int argc, char *argv[] )
{

  typedef itk::RGBPixel<unsigned char> PixelType;
  enum { InputImageDimension = 2 };
  enum { OutputImageDimension = 3 };

  typedef itk::OrientedImage<PixelType,InputImageDimension> InputImageType;
  typedef itk::OrientedImage<PixelType,OutputImageDimension> OutputImageType;
  typedef itk::ImageFileReader< InputImageType > ImageReaderType ;
  typedef itk::TileImageFilter<InputImageType,OutputImageType> TilerType;
  typedef itk::ImageSeriesWriter<OutputImageType,InputImageType> WriterType; 

  if (argc < 6)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  iSize jSize kSize input1 input2 ... inputn output" << std::endl;
    return 1;
    }

  itk::FixedArray<unsigned int,3> layout;
  layout[0] = atoi(argv[1]);
  layout[1] = atoi(argv[2]);
  layout[2] = atoi(argv[3]);

  // Tile the input images
  TilerType::Pointer tiler = TilerType::New();
  int f = 0;
  for (int i=4; i < argc - 1; i++)
    {
    ImageReaderType::Pointer reader = ImageReaderType::New();
    reader->SetFileName (argv[i]);
    reader->Update();
    tiler->SetInput(f++,reader->GetOutput());
    }
  tiler->SetLayout(layout);
  unsigned char yellow[3] = {255, 255, 127};
  itk::RGBPixel<unsigned char> fillPixel = yellow;
  tiler->SetDefaultPixelValue(fillPixel);
  tiler->Update();
  tiler->GetOutput()->Print(std::cout);

  tiler->Print( std::cout );

  WriterType::Pointer writer = WriterType::New();

  writer->SetSeriesFormat (  argv[argc-1] );

  try
    {
    writer->SetInput(tiler->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with SeriesFileNames generator" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;

    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
