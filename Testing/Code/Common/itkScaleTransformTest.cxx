/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleTransformTest.cxx
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

#include "itkScaleTransform.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkVector.h"




int main(int argc,char *argv[])
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

    TransformType::ScaleType  iscale;
    iscale = 1,4,9;

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
      TransformType::InputPointType p;
      p = 10,10,10;
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
      TransformType::InputVectorType p;
      p = 10,10,10;
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
      TransformType::InputCovariantVectorType p;
      p = 10,10,10;
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
