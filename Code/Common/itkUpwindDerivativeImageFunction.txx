/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUpwindDerivativeImageFunction.txx
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
#ifndef _itkUpwindDerivativeImageFunction_txx
#define _itkUpwindDerivativeImageFunction_txx

namespace itk
{

/**
 *
 */
template <class TInputImage>
void
UpwindDerivativeImageFunction<TInputImage>
::SetInputImage( InputImageType * ptr )
{
  this->Superclass::SetInputImage( ptr );

  m_ImageSize = 
    this->GetInputImage()->GetLargestPossibleRegion().GetSize();
  
  m_Speed = 1.0;

}


/**
 *
 */
template<class TInputImage>
void
UpwindDerivativeImageFunction<TInputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "calculate upwind derivative:" << std::endl;
  os << indent << "speed: " << m_Speed << std::endl;
}


/**
 *
 */
template <class TInputImage>
double
UpwindDerivativeImageFunction<TInputImage>
::Evaluate(
const IndexType& index,
unsigned int dim ) const
{
  
  if( !m_Image || dim > ImageDimension - 1 )
    {
    return 0.0;
    }

  m_Derivative = 0.0;
  m_NeighIndex = index ;

  m_CenterValue = (double) m_Image->GetPixel( index );
  
  // calculate backward difference
  if( m_Speed > 0 && index[dim] > 0 )
    {
    m_NeighIndex[dim] = index[dim] - 1;
    m_Derivative = m_CenterValue - (double) m_Image->GetPixel( m_NeighIndex );
    }

  // calculate forward difference
  if( m_Speed <= 0 && index[dim] < m_ImageSize[dim] - 1 )
    {
    m_NeighIndex[dim] = index[dim] + 1;
    m_Derivative = (double) m_Image->GetPixel( m_NeighIndex ) - m_CenterValue;
    }

  return ( m_Derivative );

}


} // namespace itk

#endif
