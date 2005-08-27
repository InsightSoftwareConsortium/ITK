/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ResampleOrientedImageFilter.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkOrientedImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMatrix.h"

#include "itkResampleImageFilter.h"

int main( int argc, char * argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile"; 
    std::cerr << "  direction cosines" << std::endl;
    return EXIT_FAILURE;
    }

  const     unsigned int   Dimension = 2;
  typedef   unsigned char  InputPixelType;
  typedef   unsigned char  OutputPixelType;
  typedef itk::OrientedImage< InputPixelType,  Dimension >   InputImageType;
  typedef itk::OrientedImage< OutputPixelType, Dimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  typedef itk::ResampleImageFilter<InputImageType,OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetDefaultPixelValue( 0 );

  double spacing[ Dimension ];
  spacing[0] = 1.0; // pixel spacing in millimeters along X
  spacing[1] = 1.0; // pixel spacing in millimeters along Y

  filter->SetOutputSpacing( spacing );

  double origin[ Dimension ];
  origin[0] = 0.0;  // X space coordinate of origin
  origin[1] = 0.0;  // Y space coordinate of origin

  filter->SetOutputOrigin( origin );

  OutputImageType::DirectionType direction;
  direction(0,0) = atoi(argv[3]);
  direction(1,0) = atoi(argv[4]);
  direction(0,1) = atoi(argv[5]);
  direction(1,1) = atoi(argv[6]);
  filter->SetOutputDirection(direction);

  InputImageType::SizeType   size;
  
  size[0] = 300;  // number of pixels along X
  size[1] = 300;  // number of pixels along Y

  filter->SetSize( size );
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}

