/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianBlurImageFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkGaussianBlurImageFunction.h"
#include "itkImage.h"

int itkGaussianBlurImageFunctionTest(int, char* [] )
{
  const unsigned int Dimension = 2;
  typedef float  PixelType; 
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::GaussianBlurImageFunction< ImageType > GFunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;
 
  size[0] = 50;
  size[1] = 50;

  start.Fill( 0 ); 
  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue = 0;
  image->FillBuffer( initialValue );

  // Fill the image with a straight line
  for(unsigned int i=0;i<50;i++)
  {
    ImageType::IndexType ind;
    ind[0]=i;
    ind[1]=25;
    image->SetPixel(ind,1);
    ind[1]=26;
    image->SetPixel(ind,1);
  }

  // Test the derivative of Gaussian image function
  GFunctionType::Pointer gaussianFunction = GFunctionType::New();
  gaussianFunction->SetInputImage( image );
  itk::Index<2>   index;
  index.Fill(25);

  // Testing Set/GetVariance()
  std::cout << "Testing Set/GetVariance(): ";  
  gaussianFunction->SetSigma(5.0);
  const double* sigma = gaussianFunction->GetSigma();
  
  for(unsigned int i=0;i<Dimension;i++)
  {
    if( sigma[i] !=  5.0)
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED] " << std::endl;
  
  // Testing Set/GetExtent()
  std::cout << "Testing Set/GetExtent(): ";
    
  gaussianFunction->SetExtent(5.0);
  const double* ext = gaussianFunction->GetExtent();
  
  for(unsigned int i=0;i<Dimension;i++)
  {
    if( ext[i] !=  5.0)
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED] " << std::endl;
  

  GFunctionType::OutputType  blurredvalue_index;
  blurredvalue_index = gaussianFunction->EvaluateAtIndex( index );
  
  GFunctionType::PointType pt;
  pt[0]=25.0;
  pt[1]=25.0;
  GFunctionType::OutputType  blurredvalue_point;
  blurredvalue_point = gaussianFunction->Evaluate( pt );

  
  GFunctionType::ContinuousIndexType continuousIndex;
  continuousIndex.Fill(25);
  GFunctionType::OutputType  blurredvalue_continuousIndex;
  blurredvalue_continuousIndex = gaussianFunction->EvaluateAtContinuousIndex( continuousIndex );
  
  
  std::cout << "Testing Evaluate(), EvaluateAtIndex() and EvaluateIndex: ";
  if( (fabs(blurredvalue_index-blurredvalue_point)>0.01)
     || blurredvalue_point != blurredvalue_continuousIndex)
    {
    std::cerr << "[FAILED] : " 
              << blurredvalue_index << " : " 
              << blurredvalue_point << " : " 
              << blurredvalue_continuousIndex <<  std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] " << std::endl;

  std::cout << "Testing Evaluate() : ";

  if( fabs(blurredvalue_point-0.158)> 0.1)
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
 
  std::cout << "[PASSED] " << std::endl;

  std::cout << "GaussianBlurImageFunctionTest: [DONE] " << std::endl;
  return EXIT_SUCCESS;
}

