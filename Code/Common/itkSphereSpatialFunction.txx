/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSphereSpatialFunction.txx
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
#ifndef __itkSphereSpatialFunction_txx
#define __itkSphereSpatialFunction_txx

#include "vnl_vector_fixed.h"
#include "itkSphereSpatialFunction.h"

namespace itk
{

template <unsigned int VImageDimension>
SphereSpatialFunction<VImageDimension>::SphereSpatialFunction()
{
  m_Radius = 1;
  m_Center.fill(0.0);
}

template <unsigned int VImageDimension>
SphereSpatialFunction<VImageDimension>::~SphereSpatialFunction()
{

}

template <unsigned int VImageDimension>
double SphereSpatialFunction<VImageDimension>
::EvaluateFunctionValue(TVectorType* position)
{
  double acc = 0;

  for(int i = 0; i < VImageDimension; i++)
  {
    acc += (position->get(i) - m_Center.get(i))*(position->get(i) - m_Center.get(i));
  }

  acc -= m_Radius*m_Radius;

  return acc;
}

template <unsigned int VImageDimension>
void SphereSpatialFunction<VImageDimension>
::EvaluateFunctionGradient(TVectorType* position, TVectorType* gradient)
{
  // To do: add gradient code
}

} // end namespace itk

#endif
