/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSpatialFunction.txx
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
#ifndef __itkGaussianSpatialFunction_txx
#define __itkGaussianSpatialFunction_txx

#include <math.h>
#include "itkGaussianSpatialFunction.h"

namespace itk
{

template <typename TOutput, unsigned int VImageDimension, typename TInput>
GaussianSpatialFunction<TOutput, VImageDimension, TInput>::GaussianSpatialFunction()
{
  m_Mean = TArrayType::Filled(10.0);
  m_Sigma = TArrayType::Filled(5.0);
  m_Scale = 1.0;
  m_Normalized = false;
}

template <typename TOutput, unsigned int VImageDimension, typename TInput>
GaussianSpatialFunction<TOutput, VImageDimension, TInput>::~GaussianSpatialFunction()
{

}

template <typename TOutput, unsigned int VImageDimension, typename TInput>
GaussianSpatialFunction<TOutput, VImageDimension, TInput>::OutputType 
GaussianSpatialFunction<TOutput, VImageDimension, TInput>
::Evaluate(const TInput& position) const
{
  // We have to compute the gaussian in several stages, because of the
  // n-dimensional generalization

  // Normalizing the Gaussian is important for statistical applications
  // but is generally not desirable for creating images because of the
  // very small numbers involved (would need to use doubles)
  double prefixDenom;

  if (m_Normalized)
    {
    prefixDenom = m_Sigma[0];

    for(int i = 1; i < VImageDimension; i++)
      {
      prefixDenom *= m_Sigma[i];
      }

    prefixDenom *= 2 * 3.1415927;
    }
  else
    {
    prefixDenom = 1.0;
    }

  double suffixExp = 0;

  for(int i = 0; i < VImageDimension; i++)
    {
    suffixExp += (position[i] - m_Mean[i])*(position[i] - m_Mean[i]) / (2 * m_Sigma[i] * m_Sigma[i]);
    }

  double value = m_Scale * (1 / prefixDenom) * exp(-1 * suffixExp);

  return (TOutput) value;
}


} // end namespace itk

#endif
