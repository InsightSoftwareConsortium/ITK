/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCentralDerivativeImageFunction.txx
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
#ifndef _itkCentralDerivativeImageFunction_txx
#define _itkCentralDerivativeImageFunction_txx

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage>
CentralDerivativeImageFunction<TInputImage>
::CentralDerivativeImageFunction()
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ImageSpacing[j] = 1.0;
    m_ImageSize[j] = 0;
    m_ImageStart[j] = 0;
    }
}


/**
 *
 */
template <class TInputImage>
void
CentralDerivativeImageFunction<TInputImage>
::SetInputImage( const InputImageType * ptr )
{
  this->Superclass::SetInputImage( ptr );

  m_ImageSize = 
    m_Image->GetBufferedRegion().GetSize();

  m_ImageStart =
    m_Image->GetBufferedRegion().GetIndex();  
}


/**
 *
 */
template <class TInputImage>
void
CentralDerivativeImageFunction<TInputImage>
::SetImageSpacing( const double * spacing )
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ImageSpacing[j] = spacing[j];
    }
}


/**
 *
 */
template<class TInputImage>
void
CentralDerivativeImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "calculate central difference derivative:" << std::endl;
}


/**
 *
 */
template <class TInputImage>
double
CentralDerivativeImageFunction<TInputImage>
::Evaluate(
const IndexType& index,
unsigned int dim ) const
{
  
  double derivative = 0.0;

  if( !m_Image || dim > ImageDimension - 1 )
    {
    return ( derivative );
    }
  
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( index[j] < (signed long) m_ImageStart[j] ||
        index[j] > (signed long) (m_ImageStart[j] + m_ImageSize[j]) - 1 )
      {
      return ( derivative );
      }
    }

  IndexType neighIndex = index;


  // bounds checking
  if( index[dim] < (signed long) m_ImageStart[dim] + 1 ||
      index[dim] >= (signed long) m_ImageStart[dim] + m_ImageSize[dim] - 2 )
    {
      return ( derivative );
    }
  
  // compute derivative
  neighIndex[dim] += 1;
  derivative = m_Image->GetPixel( neighIndex );

  neighIndex[dim] -= 2;
  derivative -= m_Image->GetPixel( neighIndex );

  derivative *= 0.5 / m_ImageSpacing[dim];


  return ( derivative );

}


} // namespace itk

#endif
