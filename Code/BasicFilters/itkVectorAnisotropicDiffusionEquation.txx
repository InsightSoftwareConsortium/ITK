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
#ifndef __itkVectorAnisotropicDiffusionEquation_txx_
#define __itkVectorAnisotropicDiffusionEquation_txx_

#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkVectorNeighborhoodInnerProduct.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkDerivativeOperator.h"

namespace itk {

template <class TImage>
void
VectorAnisotropicDiffusionEquation<TImage>
::CalculateAverageGradientMagnitudeSquared(TImage *ip)
{
  typedef ConstNeighborhoodIterator<TImage>      RNI_type;
  typedef ConstSmartNeighborhoodIterator<TImage> SNI_type;
  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TImage> BFC_type;

  unsigned int i, j;
  ZeroFluxNeumannBoundaryCondition<TImage>  bc;
  ScalarValueType                           accumulator;
  PixelType                                 val;
  ScalarValueType                           counter;
  BFC_type                                  bfc;
  typename BFC_type::FaceListType           faceList;
  typename RNI_type::RadiusType             radius;
  typename BFC_type::FaceListType::iterator fit;

  SmartVectorNeighborhoodInnerProduct<TImage> SIP;
  VectorNeighborhoodInnerProduct<TImage>      IP;
  RNI_type                                    iterator_list[ImageDimension];
  SNI_type                               face_iterator_list[ImageDimension];
  DerivativeOperator<ScalarValueType,
                        ImageDimension> operator_list[ImageDimension];
  
  // Set up the derivative operators, one for each dimension
  for (i = 0; i < ImageDimension; ++i)
    {
      operator_list[i].SetOrder(1);
      operator_list[i].SetDirection(i);
      operator_list[i].CreateDirectional();
      radius[i] = operator_list[i].GetRadius()[i];
    }

  // Get the various region "faces" that are on the data set boundary.
  faceList = bfc(ip, ip->GetRequestedRegion(), radius);
  fit      = faceList.begin();

  // Now do the actual processing
  accumulator = NumericTraits<ScalarValueType>::Zero;
  counter     = NumericTraits<ScalarValueType>::Zero;

  // First process the non-boundary region

  // Instead of maintaining a single N-d neighborhood of pointers,
  // we maintain a list of 1-d neighborhoods along each axial direction.
  // This is more efficient for higher dimensions.
  for (i = 0; i < ImageDimension; ++i)
    {
      iterator_list[i]=RNI_type(operator_list[i].GetRadius(), ip, *fit); 
      iterator_list[i].GoToBegin();
    }  
  while ( !iterator_list[0].IsAtEnd() )
    {
      counter += NumericTraits<ScalarValueType>::One;
      for (i = 0; i < ImageDimension; ++i)
        {
          val = IP(iterator_list[i], operator_list[i]);
          for (j = 0; j < VectorDimension; ++j)
            {  accumulator += val[j] * val[j];  }
          ++iterator_list[i];
        }
    }
  
  // Go on to the next region(s).  These are on the boundary faces.
  ++fit; 
  while ( fit != faceList.end() )
    {
        for (i = 0; i < ImageDimension; ++i)
          {
            face_iterator_list[i]=SNI_type(operator_list[i].GetRadius(), ip, *fit);
            face_iterator_list[i].GoToBegin();
          }
        
       while ( ! face_iterator_list[0].IsAtEnd() )
         {
           counter += NumericTraits<ScalarValueType>::One;
           for (i = 0; i < ImageDimension; ++i)
             {
               val = SIP(face_iterator_list[i], operator_list[i]);
               for (j= 0; j < VectorDimension; ++j)
                 {  accumulator += val[j] * val[j];  }
               ++face_iterator_list[i];
             }
         }
       ++fit;
    }
  
  m_AverageGradientMagnitudeSquared =( (ScalarValueType) (accumulator / counter) );

  //DEBUG
  std::cout << m_AverageGradientMagnitudeSquared << std::endl;
  // end DEBUG
}

}// end namespace itk


#endif
