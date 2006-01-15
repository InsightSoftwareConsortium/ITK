/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompose2DCovariantVectorImageFilterTest.cxx
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

#include <iostream>
#include "itkImage.h"
#include "itkCompose2DCovariantVectorImageFilter.h"
#include "itkImageRegionIterator.h"



int itkCompose2DCovariantVectorImageFilterTest(int , char * [])
{
  typedef unsigned char PixelType;
  typedef itk::Image< PixelType, 3 > InputImageType;

  typedef itk::Compose2DCovariantVectorImageFilter< InputImageType >  FilterType;

  typedef InputImageType::RegionType RegionType;
  typedef InputImageType::SizeType   SizeType;
  typedef InputImageType::IndexType  IndexType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer zeroImage   = InputImageType::New();
  InputImageType::Pointer oneImage = InputImageType::New();
 
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start.Fill( 0 );

  RegionType region;
  region.SetIndex( start );
  region.SetSize(  size  );
  
  zeroImage->SetRegions( region );
  oneImage->SetRegions( region );

  zeroImage->Allocate();
  oneImage->Allocate();

  zeroImage->FillBuffer( 29 );
  oneImage->FillBuffer( 51 );

  filter->SetInput1( zeroImage );
  filter->SetInput2( oneImage );

  try
    {
    filter->Update();
    filter->SetFunctor(filter->GetFunctor());
    }

  catch( itk::ExceptionObject & excp )
   {
   std::cerr << "Exception caught !" << std::endl;
   std::cerr << excp << std::endl;
   return EXIT_FAILURE;
   }

  typedef FilterType::OutputImageType  OutputImageType;

  OutputImageType::Pointer twoVectorImage = filter->GetOutput();
  
  typedef itk::ImageRegionIterator<OutputImageType> OutputIterator;
  typedef itk::ImageRegionIterator<InputImageType>  InputIterator;

  InputIterator i0( zeroImage,   region );
  InputIterator i1( oneImage, region );

  OutputIterator ot( twoVectorImage,  region );

  i0.GoToBegin();
  i1.GoToBegin();
  
  ot.GoToBegin();
  
  typedef OutputImageType::PixelType  OutputPixelType;

  while( !ot.IsAtEnd() )
    {
    OutputPixelType outp = ot.Get();
    if( i0.Get() != outp[0] )
      {
      std::cerr << "Error in zeroth component" << std::endl;
      return EXIT_FAILURE;
      }
    if( i1.Get() != outp[1] )
      {
      std::cerr << "Error in first component" << std::endl;
      return EXIT_FAILURE;
      }
    ++ot;
    ++i0;
    ++i1;
    }

  std::cout << "Test Passed !" << std::endl;

  return EXIT_SUCCESS;

}

