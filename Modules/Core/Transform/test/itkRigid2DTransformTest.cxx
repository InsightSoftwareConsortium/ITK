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

#include "itkRigid2DTransform.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"

namespace
{
bool CheckEqual(
 itk::Point<double,2> p1,
 itk::Point<double,2> p2 )
{
  const double epsilon = 1e-10;
  for( unsigned int i = 0; i < 2; i++ )
    {
    if( std::fabs( p1[i] - p2[i] ) > epsilon )
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
      if( std::fabs( offset[i]-0.0 ) > epsilon )
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

    translationInverse = dynamic_cast<TransformType*>(translation->GetInverseTransform().GetPointer());
    if(!translationInverse)
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
      if( std::fabs( offset[i]- ioffset[i] ) > epsilon )
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
        if( std::fabs( q[i]- r[i] ) > epsilon )
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
        if( std::fabs( q[i]- p[i] ) > epsilon )
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
        if( std::fabs( q[i]- p[i] ) > epsilon )
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
        if( std::fabs( q[i] - p[i] ) > epsilon )
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
    const double angle = 15.0 * std::atan( 1.0f ) / 45.0;
    const double sinth = std::sin( angle );
    const double costh = std::cos( angle );

    // around the positive Z axis
    mrotation[0][0] =  costh;
    mrotation[0][1] =  sinth;
    mrotation[1][0] = -sinth;
    mrotation[1][1] =  costh;

    rotation->SetMatrix( mrotation );

    TRY_EXPECT_NO_EXCEPTION( rotation->SetMatrix( mrotation, 1e-8 ) );
    mrotation[0][0] += 1e-7;
    TRY_EXPECT_EXCEPTION( rotation->SetMatrix( mrotation, 1e-8 ) );
    mrotation[0][0] -= 1e-7;

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

    rotationInverse = dynamic_cast<TransformType*>(rotation->GetInverseTransform().GetPointer());
    if(!rotationInverse)
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
      if( std::fabs( offset[i]- ioffset[i] ) > epsilon )
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
    TransformType::MatrixType matrix = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << std::endl;
    std::cout << matrix << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      for(unsigned int j=0; j<N; j++)
      {
        if( std::fabs( matrix[i][j]- mrotation[i][j] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
    }
    if( !Ok )
    {
      std::cerr << "Get Rotation Matrix  differs " << std::endl;
      std::cerr << "from SetMatrix value " << std::endl;
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
        if( std::fabs( q[i]- r[i] ) > epsilon )
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
        if( std::fabs( q[i] - r[i] ) > epsilon )
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
        if( std::fabs( q[i] - r[i] ) > epsilon )
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
        if( std::fabs( q[i] - r[i] ) > epsilon )
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
      double angle0;
      TransformType::InputPointType center;
      TransformType::OutputVectorType translation;

      angle0 = -21.0 / 180.0 * itk::Math::pi;
      center[0] = 12.0;
      center[1] = -8.9;
      translation[0] = 67.8;
      translation[1] = -0.2;

      t1->SetAngle( angle0 );
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

      t2dash = dynamic_cast<TransformType*>(t1->GetInverseTransform().GetPointer());
      if (!t2dash)
        {
        std::cout << "Cannot compute inverse transformation" << std::endl;
        return EXIT_FAILURE;
        }
      p3dash = t2dash->TransformPoint( p2 );

      std::cout << "Test GetInverseTransform(): ";
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

     angle0 = 14.7 / 180.0 * itk::Math::pi;
     center.Fill( 4.0 );
     translation.Fill( 67.1);
     t4->SetAngle( angle0 );
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
