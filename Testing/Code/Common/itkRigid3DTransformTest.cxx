/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransformTest.cxx
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

#include "itkRigid3DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"




int itkRigid3DTransformTest(int argc,char * argv[] )
{


  typedef itk::Rigid3DTransform<double>  TransformType;

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

    TransformType::Pointer translationInverse = translation->Inverse();
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

    TransformType::Pointer rotationInverse = rotation->Inverse();
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




  }

 
  return EXIT_SUCCESS;

}
