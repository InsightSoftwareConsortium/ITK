/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity2DTransformTest.cxx
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

#include "itkSimilarity2DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"


int itkSimilarity2DTransformTest(int argc,char *argv[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Similarity 2D Transform" << std::endl << std::endl;

  const double epsilon = 1e-10;
  const unsigned int N = 2;
  bool Ok = true;

  typedef itk::Similarity2DTransform<double>  SimilarityTransformType;
  SimilarityTransformType::Pointer transform = SimilarityTransformType::New();
  
  // 15 degrees in radians
  const double angle = 15.0 * atan( 1.0f ) / 45.0; 
  const double sinth = sin( angle );
  const double costh = cos( angle );


  std::cout << "Testing Rotation:";
  transform->SetAngle(angle);

  // Rotate an itk::Point
  SimilarityTransformType::InputPointType::ValueType pInit[2] = {10,10};
  SimilarityTransformType::InputPointType p = pInit;
  SimilarityTransformType::InputPointType q;

  q[0] =  p[0] * costh - p[1] * sinth;
  q[1] =  p[0] * sinth + p[1] * costh;

  SimilarityTransformType::OutputPointType r;
  r = transform->TransformPoint( p );
  for(unsigned int i=0; i<N; i++)
  {
     if( fabs( q[i]- r[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error rotating point   : " << p << std::endl;
    std::cerr << "Result should be       : " << q << std::endl;
    std::cerr << "Reported Result is     : " << r << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }

  
  std::cout << "Testing Translation:";

  transform->SetAngle(0);
  
  SimilarityTransformType::OffsetType::ValueType ioffsetInit[2] = {1,4};
  SimilarityTransformType::OffsetType ioffset = ioffsetInit;

  transform->SetOffset( ioffset );

  q = p + ioffset;
      
  r = transform->TransformPoint( p );
  for(unsigned int i=0; i<N; i++)
  {
    if( fabs( q[i]- r[i] ) > epsilon )
    {
      Ok = false;
      break;    
    }
  }
  if( !Ok )
  { 
    std::cerr << "Error translating point: " << p << std::endl;
    std::cerr << "Result should be       : " << q << std::endl;
    std::cerr << "Reported Result is     : " << r << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }

  return EXIT_SUCCESS;

}
