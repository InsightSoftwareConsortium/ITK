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


int itkSimilarity2DTransformTest(int ,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Similarity 2D Transform" << std::endl << std::endl;

  const double epsilon = 1e-10;
  const unsigned int N = 2;
  bool Ok = true;

  typedef itk::Similarity2DTransform<double>  SimilarityTransformType;
  SimilarityTransformType::Pointer transform = SimilarityTransformType::New();

  // Test the identity transform
  std::cout << "Testing Identity:";
  transform->SetIdentity();
 
  SimilarityTransformType::InputPointType::ValueType pInit[2] = {10,10};
  SimilarityTransformType::InputPointType p = pInit;
  SimilarityTransformType::OutputPointType r;


  r = transform->TransformPoint( p );
  for(unsigned int i=0; i<N; i++)
  {
     if( fabs( p[i]- r[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error with Identity transform" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }

  // Test the Set/Get Parameters
  std::cout << "Testing Set/GetParameters():" << std::endl;
  SimilarityTransformType::ParametersType params(6);

  for(unsigned int i=0;i<6;i++)
    {
    params[i]=i+1;
    }

  std::cout << "Input Parameters = " << params << std::endl;

  transform->SetParameters(params);
  SimilarityTransformType::ParametersType outputParams(6);

  outputParams = transform->GetParameters();

  std::cout << "Output Parameters = " << outputParams << std::endl;

  for(unsigned int i=0; i<4; i++) // do not test for the offset
  {
     if( fabs( outputParams[i]-params[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error with Set/GetParameters:" << std::endl;
    std::cerr << "Input:" << params << std::endl;
    std::cerr << "Output:" << outputParams << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }


  
  // 15 degrees in radians
  transform->SetIdentity();
  const double angle = 15.0 * atan( 1.0f ) / 45.0; 
  const double sinth = sin( angle );
  const double costh = cos( angle );


  std::cout << "Testing Rotation:";
  transform->SetAngle(angle);

  // Rotate an itk::Point
  SimilarityTransformType::InputPointType q;
  p = pInit;
  q[0] =  p[0] * costh - p[1] * sinth;
  q[1] =  p[0] * sinth + p[1] * costh;
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



  // Testing the Jacobian
  std::cout << "Testing Jacobian:";
  SimilarityTransformType::JacobianType jacobian =  transform->GetJacobian(p);

  if( 
    (jacobian[0][0] != 10) || (jacobian[0][1] != -10) || (jacobian[0][2] != 0) || (jacobian[0][3] != 0) 
    || (jacobian[0][4] != 1) || (jacobian[0][5] != 0) || (jacobian[1][0] != 10) || (jacobian[1][1] != 10)
    || (jacobian[1][2] !=0 ) || (jacobian[1][3] != 0) || (jacobian[1][4] !=0) || (jacobian[1][5] != 1)
    ) 
    {
    std::cerr << "Error with Jacobian: " << jacobian << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout << " [ PASSED ] " << std::endl;
    }

  return EXIT_SUCCESS;

}
