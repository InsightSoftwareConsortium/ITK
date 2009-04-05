/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIdentityTransformTest.cxx
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

#include <iostream>

#include "itkIdentityTransform.h"


int itkIdentityTransformTest(int ,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Identity Transform " << std::endl << std::endl;

  const double epsilon = 1e-10;
  const unsigned int N = 2;
  bool Ok = true;

  typedef itk::IdentityTransform<double>  IdentityTransformType;
  IdentityTransformType::Pointer transform = IdentityTransformType::New();
 
  std::cout << "Testing TransformPoint: ";
  IdentityTransformType::InputPointType::ValueType pInit[2] = {10,10};
  IdentityTransformType::InputPointType p = pInit;
  IdentityTransformType::OutputPointType r;

  r = transform->TransformPoint( p );
  for(unsigned int i=0; i<N; i++)
  {
     if( vcl_fabs( p[i]- r[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error Transforming Point" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }


  // Test TransformVector
  std::cout << "Testing TransformVector: ";
  IdentityTransformType::InputVectorType vin;
  vin[0] = 1;
  vin[1] = 2;
  IdentityTransformType::OutputVectorType vout;

  vout = transform->TransformVector( vin );
  for(unsigned int i=0; i<N; i++)
  {
     if( vcl_fabs( vout[i]-vin[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error with TransformVector itk::Vector" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }

  // Test TransformVector vnl_vector
  std::cout << "Testing TransformVector (vnl): ";
  IdentityTransformType::InputVnlVectorType  vnlin;
  vnlin[0] = 1;
  vnlin[1] = 2;
  IdentityTransformType::OutputVnlVectorType  vnlout;

  vnlout = transform->TransformVector( vnlin );
  for(unsigned int i=0; i<N; i++)
  {
     if( vcl_fabs( vnlout[i]-vnlin[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error with TransformVector vnlVector" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }

  // Test TransformCovariantVector
  std::cout << "Testing TransformCovariantVector: ";
  IdentityTransformType::InputCovariantVectorType   vcin;
  vcin[0] = 1;
  vcin[1] = 2;
  IdentityTransformType::OutputCovariantVectorType   vcout;

  vcout = transform->TransformCovariantVector( vcin );
  for(unsigned int i=0; i<N; i++)
  {
     if( vcl_fabs( vcout[i]-vcin[i] ) > epsilon )
     {
        Ok = false;
        break;    
     }
  }
  if( !Ok )
  { 
    std::cerr << "Error with TransformVector CovariantVector" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << " [ PASSED ] " << std::endl;
  }

  // Test the Set/Get Parameters
  std::cout << "Testing Set/GetParameters():";
  IdentityTransformType::ParametersType params(1);
  transform->SetParameters(params);
  std::cout << " [ PASSED ] " << std::endl;


  // Testing the Jacobian
  std::cout << "Testing Jacobian: ";
  IdentityTransformType::JacobianType jacobian =  transform->GetJacobian(p);

  if( 
    (jacobian[0][0] != 0.0) || (jacobian[1][0] != 0.0)) 
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
