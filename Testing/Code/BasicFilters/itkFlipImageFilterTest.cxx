/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlipImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFlipImageFilter.h"
#include "itkImage.h"
#include "itkTextOutput.h"
#include "itkCommand.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkFilterWatcher.h"

int itkFlipImageFilterTest(int, char* [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  typedef unsigned char PixelType;
  enum { ImageDimension = 3 };
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::FlipImageFilter<ImageType> FlipperType;


  // define a small input test
  ImageType::IndexType index = {{ 10, 20, 30 }};
  ImageType::SizeType size = {{5,4,3}};
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  double spacing[ImageDimension] = { 1.1, 1.2, 1.3 };
  double origin[ImageDimension] = { 0.5, 0.4, 0.3 };

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->Allocate();

  inputImage->SetSpacing( spacing );
  inputImage->SetOrigin( origin );
 
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator inputIter( inputImage, inputImage->GetBufferedRegion() );

  PixelType counter = 0;
  while ( !inputIter.IsAtEnd() )
    {
    inputIter.Set( counter );
    ++counter;
    ++inputIter;
    }
  

  // permute the image
  FlipperType::Pointer flipper = FlipperType::New();
  FilterWatcher watcher(flipper);

  bool bArray[ImageDimension] = { true, false, true };
  FlipperType::FlipAxesArrayType flipAxes( bArray );
 
  flipper->SetFlipAxes( flipAxes );
  std::cout << "FlipAxes: " << flipper->GetFlipAxes() << std::endl;
  flipper->SetInput( inputImage );
  flipper->Update();

  flipper->Print( std::cout );

  // check the output
  ImageType::Pointer outputImage = flipper->GetOutput();

  const double * inputSpacing  = inputImage->GetSpacing();
  const double * inputOrigin   = inputImage->GetOrigin();
  const double * outputSpacing = outputImage->GetSpacing();
  const double * outputOrigin  = outputImage->GetOrigin();

  inputImage->Print( std::cout );
  outputImage->Print( std::cout );

  typedef ImageType::IndexType IndexType;
  typedef IndexType::IndexValueType IndexValueType;

  inputIter.GoToBegin();
  bool passed = true;
  while ( !inputIter.IsAtEnd() )
    {

    IndexType inputIndex = inputIter.GetIndex();
    IndexType outputIndex;

    for ( int j = 0; j < ImageDimension; j++ )
      {
      if ( flipAxes[j] )
        {
        double temp = - 1 * ( static_cast<double>( inputIndex[j] ) * 
           inputSpacing[j] + inputOrigin[j]);
        outputIndex[j] = static_cast<IndexValueType>( vnl_math_rnd(( temp - outputOrigin[j] ) / 
           outputSpacing[j] ) );
        }
      else
        {
        outputIndex[j] = inputIndex[j];
        }
      }

    if ( inputIter.Get() != outputImage->GetPixel( outputIndex ) )
      {
      passed = false;
      std::cout << "Mismatch at index: " << inputIndex;
      std::cout << " " << outputIndex << std::endl;
      }

    ++inputIter;

    }

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
