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

int itkMeanImageFunctionTest(int, char**)
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
  image->FillBuffer( 27 );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  function->SetNeighborhoodRadius( 5 );

  ImageType::IndexType    index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  FunctionType::OutputType  variance;

  variance = function->EvaluateAtIndex( index );

  // since the input image is constant 
  // the variance should be zero
  if( vnl_math_abs( variance ) > 10e-7 )
    {
    std::cerr << "Error in variance computation" << std::endl;
    return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;

}

