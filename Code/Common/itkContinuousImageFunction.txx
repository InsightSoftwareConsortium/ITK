/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContinuousImageFunction.txx
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
#ifndef _itkContinuousImageFunction_txx
#define _itkContinuousImageFunction_txx

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutput>
ContinuousImageFunction<TInputImage,TOutput>
::ContinuousImageFunction()
{
  m_Image = NULL;
  m_BufferStart.Fill( 0.0 );
  m_BufferEnd.Fill( -1.0 );

  m_ImageOrigin.Fill( 0.0 );
  m_ImageSpacing.Fill( 1.0 );
  
}


/**
 * Connect the input image
 */
template <class TInputImage, class TOutput>
void
ContinuousImageFunction<TInputImage,TOutput>
::SetInputImage( InputImageType * ptr )
{
  if ( m_Image == ptr ) return;

  this->Modified();

  m_Image = ptr;
  this->ComputeBufferLimits();

}


/**
 *
 */
template <class TInputImage, class TOutput>
void
ContinuousImageFunction<TInputImage,TOutput>
::SetImageSpacing( VectorType& spacing )
{

  bool changed = false;
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( spacing[j] != m_ImageSpacing[j] ) changed = true;
    }

  if( !changed ) return;

  this->Modified();

  m_ImageSpacing = spacing;
  this->ComputeBufferLimits();

}


/**
 *
 */
template <class TInputImage, class TOutput>
void
ContinuousImageFunction<TInputImage,TOutput>
::SetImageOrigin( PointType& origin )
{
  bool changed = false;
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    if( origin[j] != m_ImageOrigin[j] ) changed = true;
    }

  if( !changed ) return;

  this->Modified();

  m_ImageOrigin = origin;
  this->ComputeBufferLimits();

}


/**
 * Compute the buffer limits
 */
template <class TInputImage, class TOutput>
void
ContinuousImageFunction<TInputImage,TOutput>
::ComputeBufferLimits()
{
  typename TInputImage::SizeType size =
    m_Image->GetBufferedRegion().GetSize();

  typename TInputImage::IndexType start =
    m_Image->GetBufferedRegion().GetIndex();

  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_BufferStart[j] = m_ImageSpacing[j] * double(start[j]) + 
      m_ImageOrigin[j];
    m_BufferEnd[j] = m_ImageSpacing[j] * double(size[j] - 1 ) + 
      m_BufferStart[j];
    }

}


/**
 *
 */
template<class TInputImage, class TOutput>
void
ContinuousImageFunction<TInputImage,TOutput>
::PrintSelf(std::ostream& os, Indent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "continuous image function" << std::endl;
}


} // namespace itk

#endif
