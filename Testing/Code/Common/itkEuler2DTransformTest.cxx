/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuler2DTransformTest.cxx
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

#include "itkEuler2DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"


int itkEuler2DTransformTest(int argc,char * argv[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Euler Angles 2D Transform" << std::endl << std::endl;

  const double epsilon = 1e-10;
  const unsigned int N = 2;
  bool Ok = true;

  typedef itk::Euler2DTransform<double>  EulerTransformType;
  EulerTransformType::Pointer eulerTransform = EulerTransformType::New();
  
  // 15 degrees in radians
  const double angle = 15.0 * atan( 1.0f ) / 45.0; 
  const double sinth = sin( angle );
  const double costh = cos( angle );


  std::cout << "Testing Rotation:";
  eulerTransform->SetRotation(angle);

  // Rotate an itk::Point
  EulerTransformType::InputPointType::ValueType pInit[2] = {10,10};
  EulerTransformType::InputPointType p = pInit;
  EulerTransformType::InputPointType q;

  q[0] =  p[0] * costh - p[1] * sinth;
  q[1] =  p[0] * sinth + p[1] * costh;

  EulerTransformType::OutputPointType r;
  r = eulerTransform->TransformPoint( p );
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

  eulerTransform->SetRotation(0);
  
  EulerTransformType::OffsetType::ValueType ioffsetInit[2] = {1,4};
  EulerTransformType::OffsetType ioffset = ioffsetInit;

  eulerTransform->SetOffset( ioffset );

  q = p + ioffset;
      
  r = eulerTransform->TransformPoint( p );
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
