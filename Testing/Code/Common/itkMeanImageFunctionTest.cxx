/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanImageFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <stdio.h>

#include "itkMeanImageFunction.h"
#include "itkImage.h"

int itkMeanImageFunctionTest(int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef unsigned char   PixelType; 

  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::MeanImageFunction< ImageType > FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;
 
  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

  start.Fill( 0 );
    
  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue = 27;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  function->SetNeighborhoodRadius( 5 );

  ImageType::IndexType    index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  FunctionType::OutputType  mean;

  mean = function->EvaluateAtIndex( index );

  // since the input image is constant 
  // the should be equal to the initial value
  if( vnl_math_abs( initialValue - mean ) > 10e-7 )
    {
    std::cerr << "Error in mean computation" << std::endl;
    return EXIT_FAILURE;
    }
  
  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}

