/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleTransformTest.cxx
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

#include "itkScaleTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"




int itkScaleTransformTest(int ,char * [] )
{


  typedef itk::ScaleTransform<double>  TransformType;


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
      if( fabs( scale[i] - 1.0 ) > epsilon )
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
      if( fabs( scale[i] - iscale[i] ) > epsilon )
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
        if( fabs( q[i] - r[i] ) > epsilon )
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
        if( fabs( q[i]- r[i] ) > epsilon )
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
        if( fabs( q[i]- r[i] ) > epsilon )
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
        if( fabs( q[i] - r[i] ) > epsilon )
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




  }

 
 
  return EXIT_SUCCESS;

}
