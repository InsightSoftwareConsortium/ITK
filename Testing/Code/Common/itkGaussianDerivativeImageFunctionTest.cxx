/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDerivativeImageFunctionTest.cxx
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

#include "itkGaussianDerivativeImageFunction.h"
#include "itkImage.h"

int itkGaussianDerivativeImageFunctionTest(int, char* [] )
{
  const unsigned int Dimension = 2;
  typedef float  PixelType; 
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::GaussianDerivativeImageFunction< ImageType > DoGFunctionType;

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
  DoGFunctionType::Pointer DoG = DoGFunctionType::New();
  DoG->SetInputImage( image );

  std::cout << "Testing Set/GetSigma(): ";
    
  DoG->SetSigma(2.0);
  const double* sigma = DoG->GetSigma();
  for(unsigned int i=0;i<Dimension;i++)
  {
    if( sigma[i] !=  2.0)
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED] " << std::endl;
  

  std::cout << "Testing Set/GetExtent(): ";
    
  DoG->SetExtent(4.0);
  const double* ext = DoG->GetExtent();
  for(unsigned int i=0;i<Dimension;i++)
  {
    if( ext[i] !=  4.0)
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED] " << std::endl;
  
  std::cout << "Testing consistency within Index/Point/ContinuousIndex: "; 
  itk::Index<2>   index;
  index.Fill(25);
  DoGFunctionType::OutputType  gradient_index;
  gradient_index = DoG->EvaluateAtIndex( index );

  DoGFunctionType::PointType pt;
  pt[0]=25.0;
  pt[1]=25.0;
  DoGFunctionType::OutputType  gradient_point;
  gradient_point = DoG->Evaluate( pt );

  
  DoGFunctionType::ContinuousIndexType continuousIndex;
  continuousIndex.Fill(25);
  DoGFunctionType::OutputType  gradient_continuousIndex;
  gradient_continuousIndex = DoG->EvaluateAtContinuousIndex( continuousIndex );

  if( gradient_index !=  gradient_point 
     || gradient_index != gradient_continuousIndex)
    {
    std::cerr << "[FAILED] : " << gradient_index << " : " 
              << gradient_point << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] " << std::endl;
  gradient_point.Normalize(); // normalize the vector;

  std::cout << "Testing Evaluate() : ";
  
  if( (gradient_point[0] > 0.1)  || 
      (fabs(gradient_point[1]+1.0)> 10e-4)
    )
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
 
  std::cout << "[PASSED] " << std::endl;

  pt[0]=25.0;
  pt[1]=26.0;
  gradient_point = DoG->Evaluate( pt );

  gradient_point.Normalize(); // normalize the vector;

  std::cout << "Testing Evaluate() : ";
  
  if( (gradient_point[0] > 0.1)  || 
      (fabs(gradient_point[1]-1.0)> 10e-4)
    )
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
 
  std::cout << "[PASSED] " << std::endl;

  std::cout << "GaussianDerivativeImageFunctionTest: [DONE] " << std::endl;
  return EXIT_SUCCESS;
}

