/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransformOptimizer.txx
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
#ifndef _itkVersorTransformOptimizer_txx
#define _itkVersorTransformOptimizer_txx

#include "itkVersorTransformOptimizer.h"
#include "vnl/vnl_quaternion.h"
#include "itkEventObject.h"

namespace itk
{




/**
 * Advance one Step following the gradient direction
 * This method will be overrided in non-vector spaces
 */
template <class TCostFunction>
void
VersorTransformOptimizer<TCostFunction>
::StepAlongGradient( double factor, 
                     const DerivativeType & transformedGradient )
{ 


  const ParametersType & currentRotation = GetCurrentPosition();

  typename ParametersType::VectorType   axis;

  // The gradient indicate the contribution of each one 
  // of the axis to the direction of highest change in
  // the function
  axis[0] = transformedGradient[0];
  axis[1] = transformedGradient[1];
  axis[2] = transformedGradient[2];

  
  // gradientRotation is a rotation along the 
  // versor direction which maximize the 
  // variation of the cost function in question.
  // An additional Exponentiation produce a jump 
  // of a particular length along the versor gradient 
  // direction.

  ParametersType gradientRotation;
  gradientRotation.Set( axis, factor * axis.GetNorm() );

  //
  // Composing the currentRotation with the gradientRotation 
  // produces the new Rotation versor 
  //
  ParametersType newRotation = currentRotation * gradientRotation;

  this->SetCurrentPosition( newRotation );

}




} // end namespace itk

#endif
