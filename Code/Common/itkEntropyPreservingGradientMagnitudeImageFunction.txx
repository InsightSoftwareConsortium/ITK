/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEntropyPreservingGradientMagnitudeImageFunction.txx
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
#ifndef _itkEntropyPreservingGradientMagnitudeImageFunction_txx
#define _itkEntropyPreservingGradientMagnitudeImageFunction_txx
#include "vnl/vnl_math.h"

namespace itk
{

/**
 *
 */
template <class TInputImage>
void
EntropyPreservingGradientMagnitudeImageFunction<TInputImage>
::SetInputImage( InputImageType * ptr )
{
  if( !ptr ) return;

  this->Superclass::SetInputImage( ptr );

  m_ImageSize = 
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();
  
  m_ImageSizeOK = true;
  for( int j = 0; j < ImageDimension; j++ )
    {
    if( m_ImageSize[j] < 3 )
      {
      m_ImageSizeOK = false;
      }
    }

  m_Speed = 1.0;

}


/**
 *
 */
template<class TInputImage>
void
EntropyPreservingGradientMagnitudeImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "calculate entropy satisfying derivative:" << std::endl;
  os << indent << "speed: " << m_Speed << std::endl;
}


/**
 *
 */
template <class TInputImage>
double
EntropyPreservingGradientMagnitudeImageFunction<TInputImage>
::Evaluate(
const IndexType& index ) const
{

  if( !m_Image )
    {
    throw ExceptionObject();
    }

  if( !m_ImageSizeOK )
    {
    return 0.0;
    }

  m_Magnitude = 0.0;
  m_NeighIndex = index ;

  InputImageConstPointer image = this->GetInputImage();

  m_CenterValue = (double) image->GetPixel( index );
  
  for( int j = 0; j < ImageDimension; j++ )
    {
    // calculate backward difference
    if( index[j] > 0 )
      {
      m_NeighIndex[j] = index[j] - 1;
      m_DiffValue = m_CenterValue - (double) image->GetPixel( m_NeighIndex );

      if( ( m_Speed > 0 && m_DiffValue > 0 ) ||
          ( m_Speed < 0 && m_DiffValue < 0 ) )
        {
        m_Magnitude += m_DiffValue * m_DiffValue;
        }

      }

    // calculate forward difference
    if( index[j] < m_ImageSize[j] - 1 )
      {
      m_NeighIndex[j] = index[j] + 1;
      m_DiffValue = (double) image->GetPixel( m_NeighIndex ) - m_CenterValue;

      if( ( m_Speed > 0 && m_DiffValue < 0 ) ||
          ( m_Speed < 0 && m_DiffValue > 0 ) )
        {
        m_Magnitude += m_DiffValue * m_DiffValue;
        }

     }

    // reset neigh index
    m_NeighIndex[j] = index[j];

    }

  m_Magnitude = vnl_math_sqrt( m_Magnitude );

  return ( m_Magnitude );

}


} // namespace itk

#endif
