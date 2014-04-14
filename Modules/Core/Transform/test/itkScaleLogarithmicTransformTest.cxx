/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkScaleLogarithmicTransform.h"

int itkScaleLogarithmicTransformTest(int ,char * [] )
{


  typedef itk::ScaleLogarithmicTransform<double>  TransformType;


  const double epsilon = 1e-10;
  const unsigned int N = 3;


  bool Ok = true;


  /* Create a 3D identity transformation and show its parameters */
  {
    TransformType::Pointer  identityTransform = TransformType::New();
    TransformType::ScaleType scale = identityTransform->GetScale();
    std::cout << "Scale from instantiating an identity transform:  ";
    for(unsigned int j=0; j<N; j++)
    {
      std::cout << scale[j] << " ";
    }
    std::cout << std::endl;
    for(unsigned int i=0; i<N; i++)
    {
      if( std::fabs( scale[i] - 1.0 ) > epsilon )
      {
        Ok = false;
        break;
      }
    }
    if( !Ok )
    {
      std::cerr << "Identity doesn't have a unit scale " << std::endl;
      return EXIT_FAILURE;
    }
  }

  /* Create a Scale transform */
  {
    TransformType::Pointer    scaleTransform = TransformType::New();

    TransformType::ScaleType::ValueType iscaleInit[3] = {1,4,9};
    TransformType::ScaleType  iscale = iscaleInit;

    scaleTransform->SetScale( iscale );

    TransformType::ScaleType scale = scaleTransform->GetScale();
    std::cout << "scale initialization  test:  ";
    for(unsigned int j=0; j<N; j++)
    {
      std::cout << scale[j] << " ";
    }
    std::cout << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      if( std::fabs( scale[i] - iscale[i] ) > epsilon )
      {
        Ok = false;
        break;
      }
    }
    if( !Ok )
    {
      std::cerr << "GetScale  differs from SetScale value " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // scale an itk::Point
      TransformType::InputPointType::ValueType pInit[3] = {10,10,10};
      TransformType::InputPointType p = pInit;
      TransformType::InputPointType q;
      for(unsigned int j=0; j<N; j++)
      {
        q[j] = p[j] * iscale[j];
      }
      TransformType::OutputPointType r;
      r = scaleTransform->TransformPoint( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( std::fabs( q[i] - r[i] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
      if( !Ok )
      {
        std::cerr << "Error scaling point : " << p << std::endl;
        std::cerr << "Result should be    : " << q << std::endl;
        std::cerr << "Reported Result is  : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok scaling an itk::Point " << std::endl;
      }
    }

    {
      // Scale an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = {10,10,10};
      TransformType::InputVectorType p = pInit;
      TransformType::OutputVectorType q;
      for(unsigned int j=0; j<N; j++)
      {
        q[j] = p[j] * iscale[j];
      }
      TransformType::OutputVectorType r;
      r = scaleTransform->TransformVector( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( std::fabs( q[i]- r[i] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
      if( !Ok )
      {
        std::cerr << "Error scaling vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok scaling an itk::Vector " << std::endl;
      }
    }

    {
      // Scale an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {10,10,10};
      TransformType::InputCovariantVectorType p = pInit;
      TransformType::OutputCovariantVectorType q;
      for(unsigned int j=0; j<N; j++)
      {
        q[j] = p[j] / iscale[j];
      }
      TransformType::OutputCovariantVectorType r;
      r = scaleTransform->TransformCovariantVector( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( std::fabs( q[i]- r[i] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
      if( !Ok )
      {
        std::cerr << "Error scaling covariant vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok scaling an itk::CovariantVector " << std::endl;
      }
    }

    {
      // Scale a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;
      p[2] = 15;
      TransformType::OutputVnlVectorType q;
      for(unsigned int j=0; j<N; j++)
      {
        q[j] = p[j] * iscale[j];
      }
      TransformType::OutputVnlVectorType r;
      r = scaleTransform->TransformVector( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( std::fabs( q[i] - r[i] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
      if( !Ok )
      {
        std::cerr << "Error scaling vnl_vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok scaling an vnl_Vector " << std::endl;
      }
    }


    // Exercise Set/Get Center methods
    {
      typedef TransformType::InputPointType  CenterType;
      CenterType center;
      center[0] = 5;
      center[1] = 6;
      center[2] = 7;

      scaleTransform->SetCenter( center );

      CenterType c2 = scaleTransform->GetCenter();
      if( c2.EuclideanDistanceTo( center ) > 1e-5 )
        {
        std::cerr << "Error in Set/Get center." << std::endl;
        std::cerr << "It was SetCenter() to    : " << center << std::endl;
        std::cerr << "but GetCenter() returned : " << c2     << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok SetCenter() / GetCenter() " << std::endl;
      }
    }


    // Exercise Set/Get parameters
    {
      typedef TransformType::ParametersType  ParametersType;
      ParametersType parameters;

      parameters = scaleTransform->GetParameters();
      parameters[0] = 0.0;  // log(1);
      parameters[1] = 0.0;  // log(1);
      parameters[2] = 0.0;  // log(1);
      scaleTransform->SetParameters( parameters );

      ParametersType p2 = scaleTransform->GetParameters();

      Ok = true;
      for( unsigned int i=0; i<N; i++)
        {
        if( std::fabs( p2[i] - parameters[i] ) > 1e-5 )
          {
          Ok = false;
          break;
          }
        }
      if( !Ok )
        {
        std::cerr << "Error in Set/Get parameters." << std::endl;
        std::cerr << "It was SetParameters() to    : " << parameters << std::endl;
        std::cerr << "but GetParameters() returned : " << p2     << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok SetParameters() / GetParameters() " << std::endl;
      }

    }
  }

  return EXIT_SUCCESS;

}
