/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.txx
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
#ifndef _itkRandomImageSource_txx
#define _itkRandomImageSource_txx

#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_sample.h"

 
namespace itk
{

/**
 *
 */
template <class TOutputImage>
RandomImageSource<TOutputImage>
::RandomImageSource()
{
  m_Size = new unsigned long [TOutputImage::GetImageDimension()];
  m_Spacing = new float [TOutputImage::GetImageDimension()];
  m_Origin = new float [TOutputImage::GetImageDimension()];  

  //Initial image is 64 wide in each direction.
  for (unsigned int i=0; i<TOutputImage::GetImageDimension(); i++)
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }

  m_Min = NumericTraits<OutputImagePixelType>::min();
  m_Max = NumericTraits<OutputImagePixelType>::max();

}

template <class TOutputImage>
RandomImageSource<TOutputImage>
::~RandomImageSource()
{
  delete [] m_Size;
  delete [] m_Spacing;
  delete [] m_Origin;
}

/**
 *
 */
template <class TOutputImage>
void 
RandomImageSource<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


/**
 * Microsoft compiler defines these and screws up the traits
 */

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::GenerateOutputInformation()
{
  TOutputImage *output;
  typename TOutputImage::IndexType index = {{0}};
  typename TOutputImage::SizeType size = {{0}};
  size.SetSize( m_Size );
  
  output = this->GetOutput(0);

  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize( size );
  largestPossibleRegion.SetIndex( index );
  output->SetLargestPossibleRegion( largestPossibleRegion );

  output->SetSpacing(m_Spacing);
  output->SetOrigin(m_Origin);
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId )
{
  itkDebugMacro(<<"Generating a random image of scalars");

  typedef typename TOutputImage::PixelType scalarType;
  typename TOutputImage::Pointer image=this->GetOutput(0);
  image->SetBufferedRegion( image->GetRequestedRegion() );

  ImageRegionIterator<TOutputImage> it(image, outputRegionForThread);

  for ( ; !it.IsAtEnd(); ++it)
    {
    it.Set( (scalarType) vnl_sample_uniform( (double)m_Min,(double)m_Max) );
    }
}

} // end namespace itk

#endif
