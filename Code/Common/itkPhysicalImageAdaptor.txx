/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhysicalImageAdaptor.txx
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
#ifndef _itkPhysicalImageAdaptor_txx
#define _itkPhysicalImageAdaptor_txx
#include "itkPhysicalImageAdaptor.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImage, class TAccessor >
PhysicalImageAdaptor<TImage , TAccessor>
::PhysicalImageAdaptor()
{
}


/**
 * Destructor
 */
template <class TImage, class TAccessor >
PhysicalImageAdaptor<TImage , TAccessor>
::~PhysicalImageAdaptor()
{
}




//----------------------------------------------------------------------------
/**
 *
 */
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const double * 
PhysicalImageAdaptor<TImage , TAccessor>
::GetSpacing( void ) const
{
  return m_Image->GetSpacing();
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::SetSpacing(const double spacing[TImage::ImageDimension] )
{
  // delegation to internal image
  m_Image->SetSpacing( spacing );
}

//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::SetSpacing(const float spacing[TImage::ImageDimension] )
{
  // delegation to internal image
  m_Image->SetSpacing( spacing );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::SetOrigin(const PointType & origin )
{
  // delegation to internal image
  m_Image->SetOrigin( origin );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::SetOrigin(const double origin[TImage::ImageDimension] )
{
  // delegation to internal image
  m_Image->SetOrigin( origin );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::SetOrigin(const float origin[TImage::ImageDimension] )
{
  // delegation to internal image
  m_Image->SetOrigin( origin );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const double * 
PhysicalImageAdaptor<TImage , TAccessor>
::GetOrigin( void ) const
{
  return m_Image->GetOrigin();
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
PhysicalImageAdaptor<TImage , TAccessor>
::SetImage( TImage * image )
{
  Superclass::SetImage(image);

  m_Image = image;
  Superclass::SetLargestPossibleRegion( m_Image->GetLargestPossibleRegion() );
  Superclass::SetBufferedRegion( m_Image->GetBufferedRegion() );
  Superclass::SetRequestedRegion( m_Image->GetRequestedRegion() );
}

} // end namespace itk



#endif
