/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateImageFunction.txx
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
#ifndef _itkLinearInterpolateImageFunction_txx
#define _itkLinearInterpolateImageFunction_txx

#include <iostream>

#include "itkPixelTraits.h"
#include "itkIndex.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template<class TInputImage>
void
LinearInterpolateImageFunction<TInputImage>
::SetInputImage( const InputImageType *ptr )
{
  this->Superclass::SetInputImage( ptr );

  m_ImageSize =
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();

  m_Neighbors = 1UL << ImageDimension;

}


/**
 *
 */
template<class TInputImage>
void
LinearInterpolateImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "linearly interpolate image." << std::endl;
}


/**
 *
 */
template<class TInputImage>
double
LinearInterpolateImageFunction<TInputImage>
::Evaluate(
const IndexType& index ) const
{
  double value = 0.0;    // Value of interpolated pixel

  /* Check if indices are within image; immediately return 0 if not */
  for( int j = 0; j < ImageDimension; j++ )
    {
    if( (unsigned long)index[j] >= m_ImageSize[j] )
      {
        return 0;
      }
    }

  /* Otherwise return the appropriate pixel value */
  value = (double)this->GetInputImage()->GetPixel( index );
  return ( value );
}


/**
 *
 */
template<class TInputImage>
double
LinearInterpolateImageFunction<TInputImage>
::Evaluate(
const PointType & point ) const
{

  /* Prepare coordinates; check whether inside image or not. */


  for (int j = 0; j < ImageDimension; j++)
  {
    m_Base[j]  = (long)floor(point[j]);
    m_Alpha[j] = point[j] - m_Base[j];
  }

  return EvaluateFromBaseAndAlpha();

}



/**
 *
 */
template<class TInputImage>
double
LinearInterpolateImageFunction<TInputImage>
::EvaluateFromBaseAndAlpha() const
{
  int j;        // Index over coordinates

  /* Prepare coordinates; check whether inside image or not.
     If completely outside image, return 0 immediately. */

  bool partial = false;                 // Partially inside image?

  for (j = 0; j < ImageDimension; j++) {
    if ( m_Base[j] < -1 || (unsigned long)m_Base[j] >= m_ImageSize[j] ) {
      // Completely outside
      return 0; }
    else if ( m_Base[j] < 0 || (unsigned long)m_Base[j] >= m_ImageSize[j]-1) {
      // Overlaps the boundary
      partial = true; }
  }

  IndexType neighIndex;
  double value = 0.0;            // Interpolated pixel value
  typename InputImageType::ConstPointer image = this->GetInputImage();

  /* Case 1: Interpolation neighborhood overlaps the image boundary */
  if (partial) {
    for (unsigned int counter = 0; counter < m_Neighbors; counter++) {
      double alf = 1.0;          // Inter parameter for each neighbor
      int upper = counter;       // Each bit indicates upper/lower neighbor
      for (j = 0; j < ImageDimension; j++) {

        neighIndex[j] = m_Base[j];

        if (upper & 1) {
          if ( m_Base[j] >= 0 &&
               (unsigned long)m_Base[j] > m_ImageSize[j] - 2 ) {
            alf = 0.0;
            break; }
          else {
            neighIndex[j] += 1;
            alf *= m_Alpha[j]; }
          }
        else {
          if ( m_Base[j] < 0 ) {
            alf = 0.0;
            break;  }
          else
            alf *= 1.0 - m_Alpha[j];
        }
        upper >>= 1;
      }

      value += alf * (double)image->GetPixel( neighIndex );
    }
  }

  /* Case 2: Interpolation neighborhood is completely inside the image */
  else {
    for (unsigned int counter = 0; counter < m_Neighbors; counter++) {
      double alf = 1.0;              // Interp parameter for each neighbor
      unsigned int upper = counter;  // Each bit indicates upper/lower neighbor
      for (j = 0; j < ImageDimension; j++) {

        if (upper & 1) {
          neighIndex[j] = m_Base[j] + 1;
          alf *= m_Alpha[j];
        }
        else {
          neighIndex[j] = m_Base[j];
          alf *= 1.0 - m_Alpha[j];
        }
        upper >>= 1;
      }

      value += alf * (double) image->GetPixel( neighIndex );
    }
  }

  return ( value );

}

} // namespace itk

#endif
