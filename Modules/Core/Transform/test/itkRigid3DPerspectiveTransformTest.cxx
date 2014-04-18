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

#include "itkRigid3DPerspectiveTransform.h"


int itkRigid3DPerspectiveTransformTest(int ,char * [] )
{


  typedef itk::Rigid3DPerspectiveTransform<double>  TransformType;

  const double epsilon = 1e-10;
  const unsigned int N = 3;

  const double focal   = 100.0;

  bool Ok = true;


  /* Create a 3D identity transformation and show its parameters */
  {
    TransformType::Pointer  identityTransform = TransformType::New();
    identityTransform->SetFocalDistance(  focal );

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

  /* Create a Rigid 3D transform with translation */
  {
    TransformType::Pointer  translation = TransformType::New();
    translation->SetFocalDistance( focal );

    TransformType::OffsetType ioffset;
    ioffset.Fill(0.0);

    translation->SetOffset( ioffset );

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
      // Projecting  an itk::Point
      TransformType::InputPointType p;
      p.Fill(10);
      TransformType::InputPointType q;
      q = p + ioffset;
      TransformType::OutputPointType s;
      const double factor = focal/q[2];
      s[0] = q[0] * factor;
      s[1] = q[1] * factor;
      TransformType::OutputPointType r;
      r = translation->TransformPoint( p );
      for(unsigned int i=0; i<N-1; i++)
      {
        if( std::fabs( s[i]- r[i] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
      if( !Ok )
      {
        std::cerr << "Error translating point: " << p << std::endl;
        std::cerr << "Result should be       : " << s << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::Point " << std::endl;
      }
    }
  }

  /* Create a Rigid 3D transform with a rotation */
  {
    TransformType::Pointer  rigid = TransformType::New();
    rigid->SetFocalDistance( focal );

    TransformType::OffsetType ioffset;
    ioffset.Fill(0.0);

    rigid->SetOffset( ioffset );

    TransformType::OffsetType offset = rigid->GetOffset();
    std::cout << "pure Translation test:  ";
    std::cout << offset << std::endl;

    typedef TransformType::VersorType  VersorType;
    VersorType rotation;
    VersorType::VectorType axis;
    VersorType::ValueType  angle = 30.0f * std::atan( 1.0f ) / 45.0f;
    axis[0] = 1.0f;
    axis[1] = 1.0f;
    axis[2] = 1.0f;

    rotation.Set( axis, angle );
    rigid->SetRotation( rotation );

    {
      // Project an itk::Point
      TransformType::InputPointType p;
      p.Fill(10.0);
      TransformType::InputPointType q;
      q = p + ioffset;
      TransformType::OutputPointType s;
      const double factor = focal/q[2];
      s[0] = q[0] * factor;
      s[1] = q[1] * factor;
      TransformType::OutputPointType r;
      r = rigid->TransformPoint( p );
      for(unsigned int i=0; i<N-1; i++)
      {
        if( std::fabs( s[i]- r[i] ) > epsilon )
        {
          Ok = false;
          break;
        }
      }
      if( !Ok )
      {
        std::cerr << "Error rotating point: " << p << std::endl;
        std::cerr << "Result should be       : " << s << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Point " << std::endl;
      }
    }
  }


  std::cout << "Test successful" << std::endl;
  return EXIT_SUCCESS;

}
