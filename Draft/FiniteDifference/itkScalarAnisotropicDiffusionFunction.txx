/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkScalarAnisotropicDiffusionFunction_txx_
#define __itkScalarAnisotropicDiffusionFunction_txx_

#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk {

template <class TImage>
void
ScalarAnisotropicDiffusionFunction<TImage>
::CalculateAverageGradientMagnitudeSquared(TImage *ip)
{
  PixelType accumulator;
  PixelType val;
  PixelType counter;
  typedef RegionNonBoundaryNeighborhoodIterator<TImage> RNI_type;
  NeighborhoodInnerProduct<TImage> IP;
  
  RNI_type iterator_list[ImageDimension];
  DerivativeOperator<PixelType, ImageDimension> operator_list[ImageDimension];
  
  // Set up the derivative operators and their iterators.
  // Instead of maintaining a single N-d neighborhood of pointers,
  // we maintain a list of 1-d neighborhoods along each axial direction.
  // This is more efficient for higher dimensions.
  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      operator_list[i].SetOrder(1);
      operator_list[i].SetDirection(i);
      operator_list[i].CreateDirectional();
      iterator_list[i]=RNI_type(operator_list[i].GetRadius(), ip,
                                ip->GetRequestedRegion()); 
      iterator_list[i] = iterator_list[i].Begin();
    }

  // Now do the actual processing
  accumulator = NumericTraits<PixelType>::Zero;
  counter     = NumericTraits<PixelType>::Zero;
  const RNI_type iterator_end = iterator_list[0].End();
  for (iterator_list[0] = iterator_list[0].Begin();
       !iterator_list[0].IsAtEnd(); )
    {
      counter += NumericTraits<PixelType>::One;
      for (unsigned int i = 0; i < ImageDimension; ++i)
        {
          val = IP(iterator_list[i], operator_list[i]);     
          accumulator += val * val;
          ++iterator_list[i];
        }
    }

  m_AverageGradientMagnitudeSquared =( (PixelType) (accumulator / counter) );
  std::cout << m_AverageGradientMagnitudeSquared << std::endl;
}

}// end namespace itk


#endif
