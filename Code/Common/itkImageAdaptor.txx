/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
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
}


/**
 * Destructor
 */
template <class TImage, class TAccessor >
ImageAdaptor<TImage , TAccessor>
::~ImageAdaptor()
{
  if( m_Image )
  {
    m_Image->UnRegister();
  }
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
const double * 
ImageAdaptor<TImage , TAccessor>
::GetSpacing( void ) const
{
  return m_Image->GetSpacing();

}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetSpacing(const double spacing[TImage::ImageDimension] )
{
  // call the superclass' method first, then delegate
  Superclass::SetSpacing(spacing);

  // delegation to internal image
  m_Image->SetSpacing( spacing );
}

//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetSpacing(const float spacing[TImage::ImageDimension] )
{
  // call the superclass' method first, then delegate
  Superclass::SetSpacing(spacing);

  // delegation to internal image
  m_Image->SetSpacing( spacing );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetOrigin(const double origin[TImage::ImageDimension] )
{
  // call the superclass' method first, then delegate
  Superclass::SetOrigin(origin);

  // delegation to internal image
  m_Image->SetOrigin( origin );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetOrigin(const float origin[TImage::ImageDimension] )
{
  // call the superclass' method first, then delegate
  Superclass::SetOrigin(origin);

  // delegation to internal image
  m_Image->SetOrigin( origin );
}


//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const double * 
ImageAdaptor<TImage , TAccessor>
::GetOrigin( void ) const
{
  return m_Image->GetOrigin();
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
  m_Image->Register();
  Superclass::SetLargestPossibleRegion( m_Image->GetLargestPossibleRegion() );
  Superclass::SetBufferedRegion( m_Image->GetBufferedRegion() );
  Superclass::SetRequestedRegion( m_Image->GetRequestedRegion() );
  Superclass::SetOrigin( m_Image->GetOrigin() );
  Superclass::SetSpacing( m_Image->GetSpacing() );
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
::Modified() 
{
  Object::Modified();
  
  m_Image->Modified();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
unsigned long
ImageAdaptor<TImage , TAccessor>
::GetMTime() 
{
  unsigned long mtime1, mtime2;

  mtime1 = this->GetMTime();
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
template <class TImage, class TAccessor >
const ImageAdaptor<TImage, TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetRequestedRegion( void ) const
{
  // delegation to internal image
  return m_Image->GetRequestedRegion();
}




} // end namespace itk


