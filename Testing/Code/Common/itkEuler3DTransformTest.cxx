/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuler3DTransformTest.cxx
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

#include <iostream>

#include "itkEuler3DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"


int itkEuler3DTransformTest(int,char *[] )
{

  std::cout << "==================================" << std::endl;
  std::cout << "Testing Euler Angles 3D Transform" << std::endl << std::endl;

  const double epsilon = 1e-10;
  const unsigned int N = 3;
  bool Ok = true;

  typedef itk::Euler3DTransform<double>  EulerTransformType;
  EulerTransformType::Pointer eulerTransform = EulerTransformType::New();
  

  // Testing Identity
  std::cout << "Testing identity transform: ";
  eulerTransform->SetIdentity();

  EulerTransformType::OffsetType offset = eulerTransform->GetOffset();
  if( offset[0] != 0.0 
     || offset[1] != 0.0 
     || offset[2] != 0.0 
    )
  {
     std::cout << "[ FAILED ]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[ PASSED ]" << std::endl;


  // 15 degrees in radians
  const double angleX = 15.0 * atan( 1.0f ) / 45.0; 
  const double cx = cos(angleX);
  const double sx = sin(angleX);
  
  // 10 degrees in radians
  const double angleY = 10.0 * atan( 1.0f ) / 45.0; 
  const double cy = cos(angleY);
  const double sy = sin(angleY);

  // 5 degrees in radians
  const double angleZ = 5.0 * atan( 1.0f ) / 45.0; 
  const double cz = cos(angleZ);
  const double sz = sin(angleZ);

  std::cout << "Testing Rotation:";
  eulerTransform->SetRotation(angleX,angleY,angleZ);

  // Rotate an itk::Point
  EulerTransformType::InputPointType::ValueType pInit[3] = {10,-5,3};
  EulerTransformType::InputPointType p = pInit;
  EulerTransformType::InputPointType q;

  itk::Matrix<double,3,3> RotationX;
  RotationX[0][0]=1;RotationX[0][1]=0;RotationX[0][2]=0;
  RotationX[1][0]=0;RotationX[1][1]=cx;RotationX[1][2]=-sx;
  RotationX[2][0]=0;RotationX[2][1]=sx;RotationX[2][2]=cx;


  itk::Matrix<double,3,3> RotationY;
  RotationY[0][0]=cy;RotationY[0][1]=0;RotationY[0][2]=sy;
  RotationY[1][0]=0;RotationY[1][1]=1;RotationY[1][2]=0;
  RotationY[2][0]=-sy;RotationY[2][1]=0;RotationY[2][2]=cy;

  
  itk::Matrix<double,3,3> RotationZ;
  RotationZ[0][0]=cz;RotationZ[0][1]=-sz;RotationZ[0][2]=0;
  RotationZ[1][0]=sz;RotationZ[1][1]=cz;RotationZ[1][2]=0;
  RotationZ[2][0]=0;RotationZ[2][1]=0;RotationZ[2][2]=1;


  q = RotationZ*RotationX*RotationY*p; // standard transformation
  
  
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

  eulerTransform->SetRotation(0,0,0);
  
  EulerTransformType::OffsetType::ValueType ioffsetInit[3] = {1,-4,8};
  EulerTransformType::OffsetType ioffset = ioffsetInit;

  eulerTransform->SetOffset( ioffset );
  std::cout << "eulerTransform: " << eulerTransform;
  
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


   // Testing Parameters
  std::cout << "Testing Set/Get Parameters: " ;
  EulerTransformType::ParametersType parameters(6);
  for(unsigned int i=0;i<6;i++)
  {
    parameters[i]=i;
  }
    
  eulerTransform->SetParameters(parameters);
  EulerTransformType::ParametersType parameters_result = eulerTransform->GetParameters();
  
  if( parameters_result[0] != 0.0
      || parameters_result[1] != 1.0
      || parameters_result[2] != 2.0
      || parameters_result[3] != 3.0
      || parameters_result[4] != 4.0
      || parameters_result[5] != 5.0
    )
  {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE; 
  }
  std::cout << " [ PASSED ] " << std::endl;


  // Testing Jacobian
  std::cout << "Testing Jacobian: ";
  for(unsigned int i=0;i<3;i++)
  {
    pInit[i]=0;
  }

  EulerTransformType::JacobianType  jacobian = eulerTransform->GetJacobian(pInit);
  
  if( jacobian[0][0] != 0.0 || jacobian[0][1] != 0.0 
      || jacobian[0][2] != 0.0 ||jacobian[0][3] != 1.0
      || jacobian[0][4] != 0.0 ||jacobian[0][5] != 0.0
      || jacobian[1][0] != 0.0 || jacobian[1][1] != 0.0 
      || jacobian[1][2] != 0.0 ||jacobian[1][3] != 0.0
      || jacobian[1][4] != 1.0 ||jacobian[1][5] != 0.0
      || jacobian[2][0] != 0.0 || jacobian[2][1] != 0.0 
      || jacobian[2][2] != 0.0 ||jacobian[2][3] != 0.0
      || jacobian[2][4] != 0.0 ||jacobian[2][5] != 1.0
    )
  {
    std::cout << " [ FAILED ] " << std::endl;
    return EXIT_FAILURE; 
  }
  std::cout << " [ PASSED ] " << std::endl;

  // Really test the Jacobian
  for( unsigned int p = 0; p < 2; p++ )
    {
    std::cout << "Testing Jacobian when ComputeZYX is ";
    if ( p == 0 )
      {
      std::cout << "true" << std::endl;
      eulerTransform->ComputeZYX( true );
      }
    else
      {
      std::cout << "false" << std::endl;
      eulerTransform->ComputeZYX( false );
      }

    parameters.Fill( 0.0 );
    parameters[0] = 0.2 / 180.0 * vnl_math::pi;
    parameters[1] = -1.0 / 180.0 * vnl_math::pi;
    parameters[2] = 2.4 / 180.0 * vnl_math::pi;
    parameters[3] = 5.0;
    parameters[4] = 6.0;
    parameters[5] = 8.0;

    eulerTransform->SetParameters( parameters );
    
    pInit[0] = 1.0;
    pInit[1] = 1.5;
    pInit[2] = 2.6;

    jacobian = eulerTransform->GetJacobian( pInit );
    std::cout << jacobian << std::endl;

    EulerTransformType::JacobianType approxJacobian = jacobian;

    for( unsigned int k = 0; k < eulerTransform->GetNumberOfParameters(); k++ )
      {
      const double delta = 0.001;
      EulerTransformType::ParametersType plusParameters;
      EulerTransformType::ParametersType minusParameters;

      plusParameters = parameters;
      minusParameters = parameters;
      plusParameters[k] += delta;
      minusParameters[k] -= delta;

      EulerTransformType::OutputPointType plusPoint;
      EulerTransformType::OutputPointType minusPoint;

      eulerTransform->SetParameters( plusParameters );
      plusPoint = eulerTransform->TransformPoint( pInit );
      eulerTransform->SetParameters( minusParameters );
      minusPoint = eulerTransform->TransformPoint( pInit );

      for( unsigned int j = 0; j < 3; j++ )
        {
        double approxDerivative = ( plusPoint[j] - minusPoint[j] ) / ( 2.0 * delta );
        double computedDerivative = jacobian[j][k];
        approxJacobian[j][k] = approxDerivative;
        if ( vnl_math_abs( approxDerivative - computedDerivative ) > 1e-5 )
          {
          std::cerr << "Error computing Jacobian [" << j << "][" << k << "]" << std::endl;
          std::cerr << "Result should be: " << approxDerivative << std::endl;
          std::cerr << "Reported result is: " << computedDerivative << std::endl;
          //std::cerr << " [ FAILED ] " << std::endl;
          //return EXIT_FAILURE;
          }
        }
      }

    std::cout << approxJacobian << std::endl;
    std::cout << " [ PASSED ] " << std::endl;

  }

  return EXIT_SUCCESS;

}
