/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMapper.txx
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
#ifndef _itkImageMapper_txx
#define _itkImageMapper_txx

#include "itkImageMapper.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImage, class TTransformation> 
ImageMapper<TImage,TTransformation>
::ImageMapper()
{
}



/**
 * Set the Domain
 */
template <class TImage, class TTransformation> 
void
ImageMapper<TImage,TTransformation>
::SetDomain(DomainType *  domain)
{
  Superclass::SetDomain( domain );

  m_Spacing  = domain->GetSpacing();
  RegionType region = domain->GetRequestedRegion();

  m_Start  = region.GetIndex();
  m_Size   = region.GetSize();

  m_Interpolator = InterpolatorType::New();
  m_Interpolator->SetInputImage( domain );

}





/**
 * Test whether the point is inside the image domain
 */
template <class TImage, class TTransformation> 
bool
ImageMapper<TImage,TTransformation>
::IsInside( const PointType & point ) 
{ 

  typename Superclass::TransformationType::Pointer transformation;

  transformation = GetTransformation();

  PointType mappedPoint = transformation->Transform( point );


  for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
  {
    m_CurrentPoint[j] =  mappedPoint[j] / m_Spacing[j] ;
  }
   
  bool value = true;
  for( unsigned int i = 0; i < TImage::ImageDimension; i++ )
  {
    
    if( m_CurrentPoint[i] < m_Start[i] )
    { 
      value = false;
      break;
    }
    
    if( m_CurrentPoint[i] >= m_Start[i] + m_Size[i] ) 
    {
      value = false;
      break;
    }
  }

  return value;

}




/**
 * Evaluate the image at some point
 */
template <class TImage, class TTransformation> 
double
ImageMapper<TImage,TTransformation>
::Evaluate( void ) const
{ 

  const double value = m_Interpolator->Evaluate( m_CurrentPoint );

  return value;

}


} // end namespace itk

#endif
