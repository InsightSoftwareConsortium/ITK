/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageAdaptor.txx
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
#ifndef _itkImageAdaptor_txx
#define _itkImageAdaptor_txx
#include "itkImageAdaptor.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImage, class TAccessor >
ImageAdaptor<TImage , TAccessor>
::ImageAdaptor()
{
  // Allocate an internal image.  A process object might try to allocate an
  // temporary image that is the same type as its input or output.  If that
  // image type is an adaptor, we need to make sure that an internal image is
  // available because the process object will not know to call SetImage on
  // the adaptor.
  m_Image = TImage::New();
}


/**
 * Destructor
 */
template <class TImage, class TAccessor >
ImageAdaptor<TImage , TAccessor>
::~ImageAdaptor()
{
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::Allocate()
{
  m_Image->Allocate();
}



/**
 *
 */
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const unsigned long * 
ImageAdaptor<TImage , TAccessor>
::GetOffsetTable( void ) const
{
  return m_Image->GetOffsetTable();
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
typename ImageAdaptor<TImage, TAccessor>::IndexType
ImageAdaptor<TImage , TAccessor>
::ComputeIndex( unsigned long offset ) const
{
  return m_Image->ComputeIndex( offset );
}





//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::Update()
{
  Superclass::Update();
  
  m_Image->Update();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::UpdateOutputInformation()
{
  // call the superclass' method first, then delegate
  Superclass::UpdateOutputInformation();

  // delegation to internal image
  m_Image->UpdateOutputInformation();
}






//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetRequestedRegionToLargestPossibleRegion()
{
  // call the superclass' method first, then delegate
  Superclass::SetRequestedRegionToLargestPossibleRegion();

  // delegation to internal image
  m_Image->SetRequestedRegionToLargestPossibleRegion();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::CopyInformation(DataObject *data)
{
  // call the superclass' method first, then delegate
  Superclass::CopyInformation( data );

  // delegation to internal image
  m_Image->CopyInformation( data );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetImage( TImage * image )
{
  m_Image = image;
  Superclass::SetLargestPossibleRegion( m_Image->GetLargestPossibleRegion() );
  Superclass::SetBufferedRegion( m_Image->GetBufferedRegion() );
  Superclass::SetRequestedRegion( m_Image->GetRequestedRegion() );
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
ImageAdaptor<TImage , TAccessor>::InternalPixelType  * 
ImageAdaptor<TImage , TAccessor>
::GetBufferPointer()
{
  return m_Image->GetBufferPointer();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::Modified() const
{
  Superclass::Modified();
  
  m_Image->Modified();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
unsigned long
ImageAdaptor<TImage , TAccessor>
::GetMTime() const
{
  unsigned long mtime1, mtime2;

  mtime1 = Superclass::GetMTime();
  mtime2 = m_Image->GetMTime();
  
  return (mtime1 >= mtime2 ? mtime1 : mtime2);
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::SetBufferedRegion(const RegionType &region)
{
  // call the superclass' method first, then delegate
  Superclass::SetBufferedRegion( region );

  // delegation to internal image
  m_Image->SetBufferedRegion( region );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const ImageAdaptor<TImage, TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetBufferedRegion( void ) const
{
  // delegation to internal image
  return m_Image->GetBufferedRegion();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::SetLargestPossibleRegion(const RegionType &region)
{
  // call the superclass' method first, then delegate
  Superclass::SetLargestPossibleRegion( region );

  // delegation to internal image
  m_Image->SetLargestPossibleRegion( region );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const ImageAdaptor<TImage, TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetLargestPossibleRegion( void ) const
{
  // delegation to internal image
  return m_Image->GetLargestPossibleRegion();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::SetRequestedRegion(const RegionType &region)
{
  // call the superclass' method first, then delegate
  Superclass::SetRequestedRegion( region );

  // delegation to internal image
  m_Image->SetRequestedRegion( region );
}


//----------------------------------------------------------------------------
template<class TImage, class TAccessor>
void 
ImageAdaptor<TImage, TAccessor>
::SetRequestedRegion(DataObject *data)
{
  // call the superclass' method first, then delegate
  Superclass::SetRequestedRegion( data );

  // delegation to internal image
  m_Image->SetRequestedRegion( data );
}

//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const ImageAdaptor<TImage, TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetRequestedRegion( void ) const
{
  // delegation to internal image
  return m_Image->GetRequestedRegion();
}




} // end namespace itk



#endif
