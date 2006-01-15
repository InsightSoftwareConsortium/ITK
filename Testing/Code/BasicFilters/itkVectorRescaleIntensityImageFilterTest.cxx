/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorRescaleIntensityImageFilterTest.cxx
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
#include "itkImageRegionIterator.h"

#include "itkVectorRescaleIntensityImageFilter.h"

int itkVectorRescaleIntensityImageFilterTest(int, char* [] )
{
  std::cout << "itkVectorRescaleIntensityImageFilterTest Start" << std::endl;

  const unsigned int VectorDimension = 3;

  typedef itk::Vector< int,   VectorDimension > InputPixelType;
  typedef itk::Vector< float, VectorDimension > OutputPixelType;

  const unsigned int ImageDimension = 3;

  typedef itk::Image< InputPixelType, ImageDimension > InputImageType;
  typedef itk::Image< OutputPixelType,ImageDimension > OutputImageType;

  InputImageType::Pointer    inputImage  = InputImageType::New();
  InputImageType::RegionType region;
  InputImageType::SizeType   size; 
  InputImageType::IndexType  index; 
  
  size.Fill( 20 );
  index.Fill( 0 );

  region.SetIndex( index );
  region.SetSize( size );

  InputPixelType pixelValue;
  pixelValue[0] = 10;
  pixelValue[1] = 20;
  pixelValue[2] = 30;

  inputImage->SetRegions(region);
  inputImage->Allocate();
  inputImage->FillBuffer( pixelValue );

  typedef itk::VectorRescaleIntensityImageFilter< 
                                     InputImageType, 
                                     OutputImageType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( inputImage );

  const double desiredMaximum =  1.0;

  filter->SetOutputMaximumMagnitude( desiredMaximum );

  try
    {
    filter->Update();
    filter->SetFunctor(filter->GetFunctor());
    }

  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }
  
  OutputImageType::ConstPointer outputImage = filter->GetOutput();
  
  typedef itk::ImageRegionConstIterator< OutputImageType > IteratorType;

  IteratorType ot( outputImage, outputImage->GetBufferedRegion() );

  ot.GoToBegin();

  const double tolerance = 1e-3;

  const double factor = desiredMaximum / static_cast< double >( pixelValue.GetNorm() );
    
  while( !ot.IsAtEnd() )
    {
    const OutputPixelType outputValue = ot.Get();
    for(unsigned int k=0; k < VectorDimension; k++)
      {
      if (outputValue[k] != 0)
        {
        if( fabs( outputValue[k] - pixelValue[k] * factor ) / outputValue[k] - 1.0 > tolerance )
          {
          std::cerr << "Test FAILED !" << std::endl;
          std::cerr << "Input  Pixel Value = " << pixelValue  << std::endl;
          std::cerr << "Output Pixel Value = " << outputValue << std::endl;
          }
        }
      }
    ++ot;
    }
    
  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}

