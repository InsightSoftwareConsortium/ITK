/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredRigid2DTransformTest.cxx
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

#include "itkCenteredRigid2DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"

#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkTransformFactory.h"

namespace
{
bool CheckEqual( 
 itk::Point<double,2> p1,
 itk::Point<double,2> p2 )
{
  const double epsilon = 1e-5;
  for( unsigned int i = 0; i < 2; i++ )
    {
    if( fabs( p1[i] - p2[i] ) > epsilon )
      {
      std::cout << p1 << " != " << p2 << ":[ FAILED ]" << std::endl;
      return false;
      }
    }
  std::cout << p1 << " == " << p2 << ":[ PASSED ]" << std::endl;
  return true;
}
}


int itkCenteredRigid2DTransformTest(int argc,char *argv[] )
{
  if (argc < 2)
    {
    std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
    return EXIT_FAILURE;
    }
  

  std::cout << "==================================" << std::endl;
  std::cout << "Testing CenteredRigid 2D Transform" << std::endl << std::endl;

  const double epsilon = 1e-10;
  const unsigned int N = 2;
  bool Ok = true;

  typedef itk::CenteredRigid2DTransform<double>  CenteredRigidTransformType;
  CenteredRigidTransformType::Pointer transform = CenteredRigidTransformType::New();
  
  // 15 degrees in radians
  const double angle = 15.0 * atan( 1.0f ) / 45.0; 
  const double sinth = sin( angle );
  const double costh = cos( angle );


  std::cout << "Testing Rotation:";
  transform->SetAngle(angle);

  // Rotate an itk::Point
  CenteredRigidTransformType::InputPointType::ValueType pInit[2] = {10,10};
  CenteredRigidTransformType::InputPointType p = pInit;
  CenteredRigidTransformType::InputPointType q;

  q[0] =  p[0] * costh - p[1] * sinth;
  q[1] =  p[0] * sinth + p[1] * costh;

  CenteredRigidTransformType::OutputPointType r;
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
  
  CenteredRigidTransformType::OffsetType::ValueType ioffsetInit[2] = {1,4};
  CenteredRigidTransformType::OffsetType ioffset = ioffsetInit;

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

  {
    std::cout << "Testing Inverse:";

    // Populate the transform with some parameters
    CenteredRigidTransformType::Pointer transform = CenteredRigidTransformType::New();
    const double a = 0.175;
    transform->SetAngle( a);

    CenteredRigidTransformType::InputPointType c;
    c[0] = 13.456;
    c[1] = 45.890;
    transform->SetCenter( c );

    CenteredRigidTransformType::OutputVectorType t;
    t[0] = 9.873;
    t[1] = 40.312;
    transform->SetTranslation( t );

    // Transform point p1 to obtain p2
    CenteredRigidTransformType::InputPointType p1;
    p1[0] = 5.63;
    p1[1] = 9.02;

    CenteredRigidTransformType::OutputPointType p2 = 
      transform->TransformPoint( p1 );

    // Get inverse transform and transform point p2 to obtain point p3
    CenteredRigidTransformType::Pointer inverse;
    transform->CloneInverseTo( inverse );

    CenteredRigidTransformType::OutputPointType p3 = 
      inverse->TransformPoint( p2 );

    // Check that point p3 is the same as point p1
    Ok = true;
    for ( unsigned int i = 0; i < N; i++ )
      {
      if ( fabs( p1[i] - p3[i] ) > epsilon )
        {
        Ok = false;
        break;
        }
      }
    if( !Ok )
    { 
      std::cerr << "Error in inverse computation" << std::endl;
      std::cerr << "Result should be       : " << p1 << std::endl;
      std::cerr << "Reported Result is     : " << p3 << std::endl;
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << " [ PASSED ] " << std::endl;
    }
    
  }

   {
      // Test instantiation, inverse computation, back transform etc.
      typedef CenteredRigidTransformType TransformType;
      TransformType::Pointer t1 = TransformType::New();

      // Set parameters
      TransformType::ParametersType parameters( t1->GetNumberOfParameters() );

      parameters[0] = -21.0 / 180.0 * vnl_math::pi;
      parameters[1] = 12.0;
      parameters[2] = -8.9;
      parameters[3] = 67.8;
      parameters[4] = -0.2;

      t1->SetParameters( parameters );

      TransformType::InputPointType p1;
      p1[0] = 96.8;
      p1[1] = -3.2;

      TransformType::InputPointType p2;
      p2 = t1->TransformPoint( p1 );

      // Test inverse
      TransformType::Pointer t2;
      t1->CloneInverseTo( t2 );

      TransformType::InputPointType p3;
      p3 = t2->TransformPoint( p2 );

      std::cout << "Test CloneInverseTo(): ";
      if( !CheckEqual( p1, p3 ) )
        {
        return EXIT_FAILURE;
        }

      TransformType::Pointer t2dash = TransformType::New();
      t1->GetInverse( t2dash );
      TransformType::InputPointType p3dash;
      p3dash = t2dash->TransformPoint( p2 );

      std::cout << "Test GetInverse(): ";
      if( !CheckEqual( p1, p3dash ) )
        {
        return EXIT_FAILURE;
        }

      // Test clone
      TransformType::Pointer t3;
      t1->CloneTo( t3 );

      TransformType::InputPointType p4;
      p4 = t3->TransformPoint( p1 );
    
      std::cout << "Test Clone(): ";
      if( !CheckEqual( p2, p4 ) )
        {
        return EXIT_FAILURE;
        }

     // Test compose
     TransformType::Pointer t4 = TransformType::New();

     parameters[0] = 14.7 / 180.0 * vnl_math::pi;
     parameters[1] = 4.0;
     parameters[2] = 4.0;
     parameters[3] = 67.1;
     parameters[4] = 67.1;

     t4->SetParameters( parameters );

     TransformType::Pointer t5;
     t1->CloneTo( t5 );
     t5->Compose( t4, false );

     TransformType::InputPointType p5, p6, p7;
     p5 = t1->TransformPoint( p1 );
     p6 = t4->TransformPoint( p5 );
     p7 = t5->TransformPoint( p1 );

    std::cout << "Test Compose(.,false): ";
    if( !CheckEqual( p6, p7 ) )
      {
      return EXIT_FAILURE;
      } 

    t1->CloneTo( t5 );
    t5->Compose( t4, true );

    p5 = t4->TransformPoint( p1 );
    p6 = t1->TransformPoint( p5 );
    p7 = t5->TransformPoint( p1 );

    std::cout << "Test Compose(.,true): ";
    if( !CheckEqual( p6, p7 ) )
      {
      return EXIT_FAILURE;
      } 

    // Really test the jacobian
    std::cout << "Testing Jacobian: ";
    TransformType::JacobianType jacobian;
    jacobian = t4->GetJacobian( p1 );

    TransformType::JacobianType approxJacobian = jacobian;

    for( unsigned int k = 0; k < t1->GetNumberOfParameters(); k++ )
      {
      const double delta = 0.001;
      TransformType::ParametersType plusParameters;
      TransformType::ParametersType minusParameters;

      plusParameters = parameters;
      minusParameters = parameters;
      plusParameters[k] += delta;
      minusParameters[k] -= delta;

      TransformType::OutputPointType plusPoint;
      TransformType::OutputPointType minusPoint;

      t4->SetParameters( plusParameters );
      plusPoint = t4->TransformPoint( p1 );
      t4->SetParameters( minusParameters );
      minusPoint = t4->TransformPoint( p1 );

      for( unsigned int j = 0; j < 2; j++ )
        {
        double approxDerivative = ( plusPoint[j] - minusPoint[j] ) / ( 2.0 * delta );
        double computedDerivative = jacobian[j][k];
        approxJacobian[j][k] = approxDerivative;
        if ( vnl_math_abs( approxDerivative - computedDerivative ) > 1e-4 )
          {
          std::cerr << "Error computing Jacobian [" << j << "][" << k << "]" << std::endl;
          std::cerr << "Result should be: " << approxDerivative << std::endl;
          std::cerr << "Reported result is: " << computedDerivative << std::endl;
          std::cerr << " [ FAILED ] " << std::endl;
          return EXIT_FAILURE;
          } // if
        } // for j

      } // for k

    std::cout << " [ PASSED ] " << std::endl;
      
    }

 {
    // Test IO
    typedef CenteredRigidTransformType TransformType;
    itk::TransformFactory<TransformType>::RegisterTransform();
    
    TransformType::Pointer t1 = TransformType::New();

    TransformType::ParametersType po, pf;

    po = t1->GetParameters();
    for( unsigned int j = 0; j < po.GetSize(); j++ )
      {
      po[j] = static_cast<double>( j ) + 1.0;
      }
    po[0] *= vnl_math::pi / 180.0;
    t1->SetParameters( po );

    pf = t1->GetFixedParameters();
    for( unsigned int j = 0; j < pf.GetSize(); j++ )
      {
      pf[j] = static_cast<double>( j ) + 1.0;
      }
    t1->SetFixedParameters( pf );

    itk::TransformFileWriter::Pointer writer;
    itk::TransformFileReader::Pointer reader;

    writer = itk::TransformFileWriter::New();
    reader = itk::TransformFileReader::New();

    writer->SetFileName( argv[1] );
    reader->SetFileName( argv[1] );

    writer->AddTransform( t1 );

    try
    {
    writer->Update();
    reader->Update();
    }
    catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

    itk::TransformFileReader::TransformListType *list;
    list = reader->GetTransformList();

    // check the transformed points are the same
    std::cout << "Test Transform IO: ";
    TransformType::InputPointType ip;
    ip[0] = 8.0;
    ip[1] = 9.0;

    TransformType * ptr;
    ptr = dynamic_cast< TransformType * >( list->front().GetPointer() );
    if( !ptr )
      {
      std::cout << "Can't cast back to the right type!" << std::endl;
      return EXIT_FAILURE;
      }
    TransformType::Pointer t2 = ptr;

    TransformType::OutputPointType op1, op2;
    op1 = t1->TransformPoint( ip );
    op2 = t2->TransformPoint( ip );

    if( !CheckEqual( op1, op2 ) )
      {
      return EXIT_FAILURE;
      } 

  }

  return EXIT_SUCCESS;

}
