/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid2DTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkRigid2DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"
#include "itkTextOutput.h"

namespace
{
bool CheckEqual( 
 itk::Point<double,2> p1,
 itk::Point<double,2> p2 )
{
  const double epsilon = 1e-10;
  for( unsigned int i = 0; i < 2; i++ )
    {
    if( fabs( p1[i] - p2[i] ) > epsilon )
      {
      std::cout << p1 << " != " << p2 << ": FAILED" << std::endl;
      return false;
      }
    }
  std::cout << p1 << " == " << p2 << ": PASSED" << std::endl;
  return true;
}
}


int itkRigid2DTransformTest(int ,char * [] )
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  typedef itk::Rigid2DTransform<double>  TransformType;

  const double epsilon = 1e-10;
  const unsigned int N = 2;


  bool Ok = true;


  /* Create a 2D identity transformation and show its parameters */
  {
    TransformType::Pointer  identityTransform = TransformType::New();
    identityTransform->SetIdentity();
    TransformType::OffsetType offset = identityTransform->GetOffset();
    std::cout << "Vector from instantiating an identity transform:  ";
    std::cout << offset << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      if( fabs( offset[i]-0.0 ) > epsilon )
      {
        Ok = false;
        break;    
      }
    }
    if( !Ok )
    { 
      std::cerr << "Identity doesn't have a null offset" << std::endl;
      return EXIT_FAILURE;
    }
  }


 
  /* Create a Rigid 2D transform with translation */
  {
    TransformType::Pointer  translation = TransformType::New();
    TransformType::OffsetType::ValueType ioffsetInit[2] = {1,4};
    TransformType::OffsetType ioffset = ioffsetInit;

    translation->SetOffset( ioffset );

    TransformType::Pointer translationInverse = TransformType::New();
    if(!translation->GetInverse(translationInverse))
      {
      std::cout << "Cannot create transform" << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << "translation: " << translation;
    std::cout << "translationInverse: " << translationInverse;

    TransformType::OffsetType offset = translation->GetOffset();
    std::cout << "pure Translation test:  ";
    std::cout << offset << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      if( fabs( offset[i]- ioffset[i] ) > epsilon )
      {
        Ok = false;
        break;    
      }
    }
    if( !Ok )
    { 
      std::cerr << "Get Offset  differs from SetOffset value " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // Translate an itk::Point
      TransformType::InputPointType::ValueType pInit[2] = {10,10};
      TransformType::InputPointType p = pInit;
      TransformType::InputPointType q;
      q = p + ioffset;
      TransformType::OutputPointType r;
      r = translation->TransformPoint( p );
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
        std::cout << "Ok translating an itk::Point " << std::endl;
      }
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[2] = {10,10};
      TransformType::InputVectorType p = pInit;
      TransformType::OutputVectorType q;
      q = translation->TransformVector( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( fabs( q[i]- p[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error translating vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::Vector " << std::endl;
      }
    }

    {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[2] = {10,10};
      TransformType::InputCovariantVectorType p = pInit;
      TransformType::OutputCovariantVectorType q;
      q = translation->TransformCovariantVector( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( fabs( q[i]- p[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error translating covariant vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::CovariantVector " << std::endl;
      }
    }

    
    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;
      TransformType::OutputVnlVectorType q;
      q = translation->TransformVector( p );
      for(unsigned int i=0; i<N; i++)
      {
        if( fabs( q[i] - p[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error translating vnl_vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an vnl_Vector " << std::endl;
      }
    }




  }

 
  /* Create a Rigid 2D transform with a rotation given by a Matrix */
  {
    TransformType::Pointer  rotation = TransformType::New();
    TransformType::MatrixType mrotation;
   
    mrotation.SetIdentity();

    // 15 degrees in radians
    const double angle = 15.0 * atan( 1.0f ) / 45.0; 
    const double sinth = sin( angle );
    const double costh = cos( angle );

    // around the positive Z axis 
    mrotation[0][0] =  costh;
    mrotation[0][1] =  sinth;
    mrotation[1][0] = -sinth;
    mrotation[1][1] =  costh;

    rotation->SetRotationMatrix( mrotation );
    rotation->SetMatrix( mrotation );

    TransformType::OffsetType ioffset;
    ioffset.Fill( 0.0f );

    rotation->SetOffset( ioffset );

    TransformType::Pointer rotationInverse = TransformType::New();
    if(!rotation->GetInverse(rotationInverse))
      {
      std::cout << "Cannot create transform" << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << "rotation: " << rotation;
    std::cout << "rotationInverse: " << rotationInverse;

    // Verify the Offset content
    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  " << std::endl;
    std::cout << "Offset = " << offset << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      if( fabs( offset[i]- ioffset[i] ) > epsilon )
      {
        Ok = false;
        break;    
      }
    }
    if( !Ok )
    { 
      std::cerr << "Get Offset  differs from SetOffset value " << std::endl;
      return EXIT_FAILURE;
    }

    // Verify the Matrix content
    TransformType::MatrixType matrix = rotation->GetRotationMatrix();
    std::cout << "Rotation matrix:  " << std::endl;
    std::cout << matrix << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      for(unsigned int j=0; j<N; j++)
      {
        if( fabs( matrix[i][j]- mrotation[i][j] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
    }
    if( !Ok )
    { 
      std::cerr << "Get Rotation Matrix  differs " << std::endl;
      std::cerr << "from SetRotationMatrix value " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // Rotate an itk::Point
      TransformType::InputPointType::ValueType pInit[2] = {10,10};
      TransformType::InputPointType p = pInit;
      TransformType::InputPointType q;

      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;

      TransformType::OutputPointType r;
      r = rotation->TransformPoint( p );
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
        std::cout << "Ok translating an itk::Point " << std::endl;
      }
    }

    {
      // Rotate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[2] = {10,10};
      TransformType::InputVectorType p = pInit;

      TransformType::InputPointType q;
      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;

      TransformType::OutputVectorType r;
      r = rotation->TransformVector( p );
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
        std::cerr << "Error rotating vector  : " << p << std::endl;
        std::cerr << "Result should be       : " << q << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
      }
    }

    {
      // Rotate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[2] = {10,10};
      TransformType::InputCovariantVectorType p = pInit;
      TransformType::OutputCovariantVectorType q;

      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector( p );

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
        std::cerr << "Error Rotating covariant vector: " << p << std::endl;
        std::cerr << "Result should be               : " << q << std::endl;
        std::cerr << "Reported Result is             : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::CovariantVector " << std::endl;
      }
    }

    
    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;

      TransformType::OutputVnlVectorType q;

      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;


      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVector( p );
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
        std::cerr << "Error translating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be             : " << q << std::endl;
        std::cerr << "Reported Result is           : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an vnl_Vector " << std::endl;
      }
    }

    {
      // Test instantiation, inverse computation, back transform etc.
      TransformType::Pointer t1 = TransformType::New();

      // Set parameters
      double angle;
      TransformType::InputPointType center;
      TransformType::OutputVectorType translation;

      angle = -21.0 / 180.0 * vnl_math::pi;
      center[0] = 12.0;
      center[1] = -8.9;
      translation[0] = 67.8;
      translation[1] = -0.2;

      t1->SetAngle( angle );
      t1->SetCenter( center );
      t1->SetTranslation( translation );

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

     angle = 14.7 / 180.0 * vnl_math::pi;
     center.Fill( 4.0 );
     translation.Fill( 67.1) ;
     t4->SetAngle( angle );
     t4->SetCenter( center );
     t4->SetTranslation( translation );

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
      
    }
  }


   return EXIT_SUCCESS;

}
