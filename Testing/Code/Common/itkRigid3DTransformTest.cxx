/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#include <iostream>

#include "itkRigid3DTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"




int main(int argc,char *argv[])
{


  typedef itk::Rigid3DTransform<double>  TransformType;

  typedef  itk::Vector<double,3>     VectorType;


  VectorType                   vector1;

  const double epsilon = 1e-10;
  const unsigned int N = 3;

  vector1 = 3,4,6;


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
    TransformType::OffsetType ioffset;
    ioffset = 1,4,9;

    translation->SetOffset( ioffset );

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
      TransformType::InputPointType p;
      p = 10,10,10;
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
      TransformType::InputVectorType p;
      p = 10,10,10;
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
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] =  7;
      p[2] = 15;
      TransformType::OutputVnlVectorType q;
      q = translation->TransformVnlVector( p );
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

 
  /* Create a Rigid 3D transform with rotation */
  {
    TransformType::Pointer  rotation = TransformType::New();

    itk::Vector<double,3> axis;
    axis = 1,1,1;

    const double angle = (atan(1.0)/45.0)*120.0; // turn 120 degrees

    // this rotation will permute the axis x->y, y->z, z->x
    rotation->SetRotation( axis, angle );

    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  ";
    std::cout << offset << std::endl;

    for(unsigned int i=0; i<N; i++)
    {
      if( fabs( offset[i] - 0.0 ) > epsilon )
      {
        Ok = false;
        break;    
      }
    }
    if( !Ok )
    { 
      std::cerr << "Get Offset  differs from null in rotation " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // Rotate an itk::Point
      TransformType::InputPointType p;
      p = 1,4,9;
      TransformType::OutputPointType q;
      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

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
        std::cerr << "Error rotating point : " << p << std::endl;
        std::cerr << "Result should be     : " << q << std::endl;
        std::cerr << "Reported Result is   : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Point " << std::endl;
      }
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType p;
      p = 1,4,9;
      TransformType::OutputVectorType q;
      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

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
        std::cerr << "Error rotating vector : " << p << std::endl;
        std::cerr << "Result should is      : " << q << std::endl;
        std::cerr << "Reported Result is    : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
      }
    }


    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 1;
      p[1] = 4;
      p[2] = 9;

      TransformType::OutputVnlVectorType q;

      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVnlVector( p );
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
        std::cerr << "Error rotating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be          : " << q << std::endl;
        std::cerr << "Reported Result is        : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an vnl_Vector " << std::endl;
      }
    }




  }

 
  return EXIT_SUCCESS;

}
