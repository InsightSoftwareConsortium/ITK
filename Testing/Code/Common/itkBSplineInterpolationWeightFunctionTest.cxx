/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineInterpolationWeightFunctionTest.cxx
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

#include "itkBSplineInterpolationWeightFunction.h"

/*
 * This test exercises methods in the 
 * BSplineInterpolationWeightFunction class.
 */
int itkBSplineInterpolationWeightFunctionTest(int, char* [] )
{
  
  typedef double CoordRepType;
  const unsigned int SpaceDimension= 3;
  const unsigned int SplineOrder = 3;

  typedef itk::BSplineInterpolationWeightFunction<CoordRepType,
    SpaceDimension, SplineOrder> FunctionType;
  typedef FunctionType::ContinuousIndexType ContinuousIndexType;
  typedef FunctionType::IndexType IndexType;
  typedef FunctionType::WeightsType WeightsType;
  typedef FunctionType::SizeType SizeType;

  FunctionType::Pointer function = FunctionType::New();
  function->Print( std::cout );

  SizeType size = function->GetSupportSize();
  unsigned long numberOfWeights = function->GetNumberOfWeights();

  std::cout << "Number Of Weights: " << numberOfWeights << std::endl;

  ContinuousIndexType position;
  WeightsType weights;
  IndexType startIndex;

  position.Fill( 4.15 );
  weights = function->Evaluate( position );

  std::cout << "Position: " << position << std::endl;
  std::cout << "Weights: " << weights << std::endl;

  function->Evaluate( position, weights, startIndex );
  std::cout << "Position: " << position << std::endl;
  std::cout << "Weights: " << weights << std::endl;
  std::cout << "Start Index: " << startIndex << std::endl;

  
  // Check for accuracy
  typedef itk::BSplineKernelFunction<SplineOrder> KernelType;
  KernelType::Pointer kernel = KernelType::New();
 
  typedef itk::Image<char,SpaceDimension> ImageType;
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  region.SetIndex( startIndex );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();
  image->FillBuffer( 0 );

  typedef itk::ImageRegionConstIteratorWithIndex<ImageType>
    IteratorType;
  IteratorType iter( image, image->GetBufferedRegion() );
  unsigned long counter = 0;

  while ( !iter.IsAtEnd() )
    {

    double value = 1.0;
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      value *= kernel->Evaluate( 
        static_cast<double>(iter.GetIndex()[j]) - position[j] );
      }
    if ( vnl_math_abs(weights[counter] - value) > 1e-7 )
      {
      std::cout << "Error at weights[" << counter << "]" << std::endl;
      std::cout << "Compuated value: " << weights[counter] << std::endl;
      std::cout << "Expected value: " << value  << std::endl;
      return EXIT_FAILURE;
      }

    ++counter;
    ++iter;
    }

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;

}
