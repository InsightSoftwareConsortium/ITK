/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFunctionTest.cxx
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

#include <stdio.h>

#include "itkNeighborhoodOperatorImageFunction.h"

#include "itkImage.h"
#include "itkGaussianOperator.h"

int itkNeighborhoodOperatorImageFunctionTest(int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef float  PixelType; 
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::GaussianOperator<PixelType,3>      NeighborhoodOperatorType;
  typedef itk::NeighborhoodOperatorImageFunction< ImageType,PixelType> FunctionType;

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

  NeighborhoodOperatorType* oper= new NeighborhoodOperatorType;
  oper->CreateToRadius(3);
  
  function->SetOperator(*oper);
  delete oper;

  itk::Index<3>    index;
  index.Fill(25);

  FunctionType::OutputType  Blur;

  Blur = function->EvaluateAtIndex( index );

  // since the input image is constant 
  // the should be equal to the initial value
  if( vnl_math_abs( initialValue - Blur ) > 10e-7 )
    {
    std::cerr << "Error in Blur computation" << std::endl;
    return EXIT_FAILURE;
    }
  
  std::cout << "[PASSED] " << std::endl;
  return EXIT_SUCCESS;

}

