/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConeSpatialFunction.txx
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
#ifndef __itkConeSpatialFunction_txx
#define __itkConeSpatialFunction_txx

#include "vnl/vnl_vector_fixed.h"
#include <math.h>
#include "itkConeSpatialFunction.h"

namespace itk
{

// Construct cone with angle of 45 degrees.
template <unsigned int VImageDimension>
ConeSpatialFunction<VImageDimension>::ConeSpatialFunction()
{
  m_Angle = 45.0;
}

template <unsigned int VImageDimension>
ConeSpatialFunction<VImageDimension>::~ConeSpatialFunction()
{

}

// Evaluate cone equation.
template <unsigned int VImageDimension>
ConeSpatialFunction<VImageDimension>::TFunctionValueType
ConeSpatialFunction<VImageDimension>
::Evaluate(TVectorType* position)
{
  double tanTheta = tan( m_Angle*3.1415926/180 );

  double result = position->get(1)*position->get(1) + position->get(2)*position->get(2)
    - position->get(0)*position->get(0)*tanTheta*tanTheta;

  if (result <= 0)
    return 1;
  else
    return 0;
}

} // end namespace itk

#endif
