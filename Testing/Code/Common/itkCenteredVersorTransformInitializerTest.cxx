/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredVersorTransformInitializerTest.cxx
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

#include "itkVersorRigid3DTransform.h"
#include "itkCenteredVersorTransformInitializer.h"
#include "itkImage.h"


/** 
 *  This program tests the use of the CenteredVersorTransformInitializer class
 * 
 *  
 */ 

int itkCenteredVersorTransformInitializerTest(int argc, char* argv[] )
{

  bool pass = true;

  const unsigned int dimension = 3;

  // Fixed Image Type
  typedef itk::Image<unsigned char ,dimension>      FixedImageType;

  // Moving Image Type
  typedef itk::Image<unsigned char,dimension>       MovingImageType;

  // Size Type
  typedef MovingImageType::SizeType                 SizeType;
  typedef MovingImageType::SpacingType              SpacingType;
  typedef MovingImageType::PointType                PointType;
  typedef MovingImageType::IndexType                IndexType;


  // Transform Type
  typedef itk::VersorRigid3DTransform< double >     TransformType;
  typedef TransformType::ParametersType             ParametersType;

  SizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] = 100;
  
  PointType fixedOrigin;


  FixedImageType::Pointer     fixedImage    = FixedImageType::New();
  MovingImageType::Pointer    movingImage   = MovingImageType::New();


  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;


}

