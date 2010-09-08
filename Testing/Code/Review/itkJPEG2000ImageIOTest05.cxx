/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReaderStreamingTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkJPEG2000ImageIOFactory.h"
#include "itkJPEG2000ImageIO.h"

#include <fstream>

int itkJPEG2000ImageIOTest05( int argc, char * argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " input outputdir extension" << std::endl;
    return EXIT_FAILURE;
    }


  typedef unsigned char             PixelType;
  typedef itk::Image<PixelType,3>   ImageType;
  typedef itk::Image<PixelType,2>   OutputImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  //reader->SetUseStreaming( true );

  try
    {
    reader->Update();
    reader->GetOutput()->Print(std::cout);
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();

  itk::NumericSeriesFileNames::Pointer fit = itk::NumericSeriesFileNames::New();

  typedef  itk::ImageSeriesWriter<ImageType,OutputImageType> WriterType;

  WriterType::Pointer writer = WriterType::New();


  char format[4096];
  sprintf (format, "%s/series.%%d.%s", argv[2], argv[3]);

  std::cout << "Format = " << format << std::endl;

  ImageType::RegionType region = reader->GetOutput()->GetBufferedRegion();
  ImageType::SizeType   size = region.GetSize();

  fit->SetStartIndex(0);
  fit->SetEndIndex(size[2]-1);  // The number of slices to write
  fit->SetIncrementIndex(1);
  fit->SetSeriesFormat (format);

  writer->SetInput(reader->GetOutput());
  writer->SetFileNames(  fit->GetFileNames() );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the series with SeriesFileNames generator" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
