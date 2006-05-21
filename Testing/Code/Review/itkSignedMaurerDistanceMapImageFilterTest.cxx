/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSignedMaurerDistanceMapImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkSignedMaurerDistanceMapImageFilter.h"

int itkSignedMaurerDistanceMapImageFilterTest( int argc, char * argv[] )
{
   if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage\n";
    return -1;
    }

 const unsigned int      ImageDimension = 2;
  typedef int             InputPixelType;
  typedef double          OutputPixelType;

  typedef itk::Image<InputPixelType,  ImageDimension>  InputImageType;
  typedef itk::Image<OutputPixelType, ImageDimension>  OutputImageType;

  typedef itk::ImageFileReader<InputImageType>    ReaderType;
  typedef itk::ImageFileWriter<OutputImageType>   WriterType;
  typedef InputImageType::SizeType                InputSizeType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();  

  typedef itk::SignedMaurerDistanceMapImageFilter
     <InputImageType, OutputImageType>  FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetSquaredDistance( false );
  filter->SetUseImageSpacing( false );
  filter->SetInsideIsPositive( true );
  filter->Update();

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  return 0;
};
