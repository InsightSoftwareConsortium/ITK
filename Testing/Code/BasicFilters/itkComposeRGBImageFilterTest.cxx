/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkComposeRGBImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkComposeRGBImageFilter.h"
#include "itkImageRegionIterator.h"



int itkComposeRGBImageFilterTest(int argc, char * argv[] )
{
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, 3 > InputImageType;

  typedef itk::ComposeRGBImageFilter< InputImageType >  FilterType;

  typedef InputImageType::RegionType RegionType;
  typedef InputImageType::SizeType   SizeType;
  typedef InputImageType::IndexType  IndexType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer redImage   = InputImageType::New();
  InputImageType::Pointer greenImage = InputImageType::New();
  InputImageType::Pointer blueImage  = InputImageType::New();
 
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start.Fill( 0 );

  RegionType region;
  region.SetIndex( start );
  region.SetSize(  size  );
  
  redImage->SetRegions( region );
  greenImage->SetRegions( region );
  blueImage->SetRegions( region );

  redImage->Allocate();
  greenImage->Allocate();
  blueImage->Allocate();

  redImage->FillBuffer( 29 );
  greenImage->FillBuffer( 51 );
  blueImage->FillBuffer( 83 );

  filter->SetInput1( redImage );
  filter->SetInput2( greenImage );
  filter->SetInput3( blueImage );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
   {
   std::cerr << "Exception caught !" << std::endl;
   std::cerr << excp << std::endl;
   return EXIT_FAILURE;
   }

  typedef FilterType::OutputImageType  OutputImageType;

  OutputImageType::Pointer rgbImage = filter->GetOutput();
  
  typedef itk::ImageRegionIterator<OutputImageType> OutputIterator;
  typedef itk::ImageRegionIterator<InputImageType>  InputIterator;

  InputIterator ir( redImage,   region );
  InputIterator ig( greenImage, region );
  InputIterator ib( blueImage,  region );

  OutputIterator ot( rgbImage,  region );

  ir.GoToBegin();
  ig.GoToBegin();
  ib.GoToBegin();
  
  ot.GoToBegin();
  
  typedef OutputImageType::PixelType  OutputPixelType;

  while( !ot.IsAtEnd() )
    {
    OutputPixelType outp = ot.Get();
    if( ir.Get() != outp.GetRed() )
      {
      std::cerr << "Error in red component" << std::endl;
      return EXIT_FAILURE;
      }
    if( ig.Get() != outp.GetGreen() )
      {
      std::cerr << "Error in green component" << std::endl;
      return EXIT_FAILURE;
      }
    if( ib.Get() != outp.GetBlue() )
      {
      std::cerr << "Error in blue component" << std::endl;
      return EXIT_FAILURE;
      }
    ++ot;
    ++ir;
    ++ig;
    ++ib;
    }

  std::cout << "Test Passed !" << std::endl;

  return EXIT_SUCCESS;

}

