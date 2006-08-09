/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransformTest.cxx
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

#include "itkRigid3DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_det.h"
#include "vnl/vnl_math.h"
#include "itkVector.h"

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
  //std::cout << p1 << " == " << p2 << ": PASSED" << std::endl;
  return true;
}
}


int itkRigid3DTransformTest(int ,char * [] )
{


  typedef itk::Rigid3DTransform<double>  TransformType;
  typedef TransformType::ParametersType  ParametersType;

  const double epsilon = 1e-10;
  const unsigned int N = 3;


  bool Ok = true;


  /* Create a 3D identity transformation and show its parameters */
  {
    TransformType::Pointer  identityTransform = TransformType::New();
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


 
  /* Create a Rigid 3D transform with translation */
  {
    TransformType::Pointer  translation = TransformType::New();
    TransformType::OffsetType::ValueType ioffsetInit[3] = {1,4,9};
    TransformType::OffsetType ioffset = ioffsetInit;

    translation->SetOffset( ioffset );

    TransformType::Pointer translationInverse = TransformType::New();
    if(!translation->GetInverse(translationInverse))
      {
      std::cout << "Cannot compute inverse" << std::endl;
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
      TransformType::InputPointType::ValueType pInit[3] = {10,10,10};
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
      TransformType::InputVectorType::ValueType pInit[3] = {10,10,10};
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
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {10,10,10};
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
      p[2] = 15;
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

 
  /* Create a Rigid 3D transform with a rotation given by a Matrix */
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

    TransformType::OffsetType ioffset;
    ioffset.Fill( 0.0f );

    rotation->SetOffset( ioffset );

    TransformType::Pointer rotationInverse = TransformType::New();
    if(!rotation->GetInverse(rotationInverse))
      {
      std::cout << "Cannot compute inverse" << std::endl;
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
      TransformType::InputPointType::ValueType pInit[3] = {10,10,10};
      TransformType::InputPointType p = pInit;
      TransformType::InputPointType q;

      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

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
      TransformType::InputVectorType::ValueType pInit[3] = {10,10,10};
      TransformType::InputVectorType p = pInit;

      TransformType::InputPointType q;
      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

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
      TransformType::InputCovariantVectorType::ValueType pInit[3] = {10,10,10};
      TransformType::InputCovariantVectorType p = pInit;
      TransformType::OutputCovariantVectorType q;

      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] =  p[2];

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
      p[2] = 15;

      TransformType::OutputVnlVectorType q;

      q[0] =  p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] =  p[2];


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
     // Testing SetParameters()
     std::cout << "Testing SetParameters() ... ";
     unsigned int j;

     TransformType::Pointer t = TransformType::New();
     ParametersType p( t->GetNumberOfParameters() );
      
     // attempt to set an non-orthogonal matrix
     for( j = 0; j < t->GetNumberOfParameters(); j++ )
      {
      p[j] = static_cast<double>( j + 1 );
      p[j] = vnl_math_sqr( p[j] );
      }

     Ok = false;
     try
      {
      t->SetParameters( p );
      }
     catch ( itk::ExceptionObject & itkNotUsed(err) )
      {
      Ok = true;
      }
     catch( ... )
      {
      std::cout << "Caught unknown exception" << std::endl;
      }

     if( !Ok )
      {
      std::cerr << "Error: expected to catch an exception when attempting";
      std::cerr << " to set an non-orthogonal matrix." << std::endl;
      return EXIT_FAILURE;
      }

    // attempt to set an orthogonal matrix
    typedef TransformType::MatrixType MatrixType;
    unsigned int par = 0;

    MatrixType matrix;
    matrix.GetVnlMatrix().set_identity();

    double a = 1.0 / 180.0 * vnl_math::pi;
    matrix[0][0] =        cos( a );
    matrix[0][1] =        sin( a );
    matrix[1][0] = -1.0 * sin( a ); 
    matrix[1][1] =        cos( a );

    par = 0;
    for( unsigned int row = 0; row < 3; row++ )
      {
      for( unsigned int col = 0; col < 3; col++ )
        {
        p[par] = matrix[row][col];
        ++par;
        }
      }

     Ok = true;
     try
      {
      t->SetParameters( p );
      }
     catch ( itk::ExceptionObject & err )
      {
      std::cout << err << std::endl;
      Ok = false;
      }
     catch( ... )
      {
      std::cout << "Caught unknown exception" << std::endl;
      Ok = false;
      }

     if( !Ok )
      {
      std::cerr << "Error: caught unexpected exception" << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "done." << std::endl;
    }


    {
    // Testing SetIdentity()
    std::cout << "Testing SetIdentity() ... ";
    
    TransformType::Pointer t = TransformType::New();
    ParametersType p( t->GetNumberOfParameters() );

    t->SetIdentity();
    p = t->GetParameters();

    // check if all elements is the expected value to within tolerance
    ParametersType pIdeal( t->GetNumberOfParameters() );
    pIdeal.Fill( 0.0 );
    pIdeal[0] = 1.0;
    pIdeal[4] = 1.0;
    pIdeal[8] = 1.0;

    Ok = true;
    for( unsigned int par = 0; par < t->GetNumberOfParameters(); par++ )
      {
      if( vnl_math_abs( p[par] - pIdeal[par] ) > epsilon )
        {
        std::cerr << "Expected parameters: " << pIdeal << std::endl;
        std::cerr << "Actual parameters: " << p << std::endl;
        Ok = false;
        break;
        }
      }

    if( !Ok )
      {
      std::cerr << "Test failed." << std::endl;
      return EXIT_FAILURE;
      }
  
    std::cout << "done. " << std::endl;

   }


   {
     // Testing SetMatrix()
     std::cout << "Testing SetMatrix() ... ";
     unsigned int par;

     typedef TransformType::MatrixType MatrixType;
     MatrixType matrix;

     TransformType::Pointer t = TransformType::New();
      
     // attempt to set an non-orthogonal matrix
     par = 0;
     for( unsigned int row = 0; row < 3; row++ )
        {
        for( unsigned int col = 0; col < 3; col++ )
          {
          matrix[row][col] = static_cast<double>( par + 1 );
          ++par;
          }
        }

     Ok = false;
     try
      {
      t->SetMatrix( matrix );
      }
     catch ( itk::ExceptionObject & itkNotUsed(err) )
      {
      Ok = true;
      }
     catch( ... )
      {
      std::cout << "Caught unknown exception" << std::endl;
      }

     if( !Ok )
      {
      std::cerr << "Error: expected to catch an exception when attempting";
      std::cerr << " to set an non-orthogonal matrix." << std::endl;
      return EXIT_FAILURE;
      }

      // attempt to set an orthogonal matrix
      matrix.GetVnlMatrix().set_identity();

      double a = 1.0 / 180.0 * vnl_math::pi;
      matrix[0][0] =        cos( a );
      matrix[0][1] =        sin( a );
      matrix[1][0] = -1.0 * sin( a ); 
      matrix[1][1] =        cos( a );

     Ok = true;
     try
      {
      t->SetMatrix( matrix );
      }
     catch ( itk::ExceptionObject & err )
      {
      std::cout << err << std::endl;
      Ok = false;
      }
     catch( ... )
      {
      std::cout << "Caught unknown exception" << std::endl;
      Ok = false;
      }

     if( !Ok )
      {
      std::cerr << "Error: caught unexpected exception" << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "done." << std::endl;
    }

  }

 
  return EXIT_SUCCESS;

}
