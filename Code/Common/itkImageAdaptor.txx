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
  //Superclass::PrintSelf(os,indent);
  
  os << indent << "ImageAdaptor " << std::endl;
  os << std::endl;
}





//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetSpacing(const double spacing[TImage::ImageDimension] )
{
  m_Image->SetSpacing( spacing );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const double *
ImageAdaptor<TImage , TAccessor>
::GetSpacing() const
{
  return m_Image->GetSpacing();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetOrigin(const double origin[TImage::ImageDimension] )
{
  m_Image->SetOrigin( origin );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const double *
ImageAdaptor<TImage , TAccessor>
::GetOrigin() const
{
  return m_Image->GetOrigin();
}





//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::Update()
{
  m_Image->Update();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::UpdateOutputInformation()
{
  m_Image->UpdateOutputInformation();
}






//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetRequestedRegionToLargestPossibleRegion()
{
  m_Image->SetRequestedRegionToLargestPossibleRegion();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::CopyInformation(DataObject *data)
{
  m_Image->CopyInformation( data );
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
bool 
ImageAdaptor<TImage , TAccessor>
::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  return m_Image->RequestedRegionIsOutsideOfTheBufferedRegion();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
bool 
ImageAdaptor<TImage , TAccessor>
::VerifyRequestedRegion()
{
  return m_Image->VerifyRequestedRegion();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetImage( TImage * image )
{
  m_Image = image;
  m_Image->Register();
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
const unsigned long *
ImageAdaptor<TImage , TAccessor>
::GetOffsetTable() const
{
  return m_Image->GetOffsetTable();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
unsigned long 
ImageAdaptor<TImage , TAccessor>
::ComputeOffset(const IndexType &ind) const
{
  return m_Image->ComputeOffset( ind );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
ImageAdaptor<TImage , TAccessor>::IndexType
ImageAdaptor<TImage , TAccessor>
::ComputeIndex(unsigned long offset) const
{
  return m_Image->ComputeIndex( offset );
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::Modified() 
{
  m_Image->Modified();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
unsigned long
ImageAdaptor<TImage , TAccessor>
::GetMTime() 
{
  return m_Image->GetMTime();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::SetBufferedRegion(const RegionType &region)
{
  m_Image->SetBufferedRegion( region );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::SetLargestPossibleRegion(const RegionType &region)
{
  m_Image->SetLargestPossibleRegion( region );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void
ImageAdaptor<TImage , TAccessor>
::SetRequestedRegion(const RegionType &region)
{
  m_Image->SetRequestedRegion( region );
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const ImageAdaptor<TImage , TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetBufferedRegion()
{
  return m_Image->GetBufferedRegion();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const ImageAdaptor<TImage , TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetLargestPossibleRegion()
{
  return m_Image->GetLargestPossibleRegion();
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const ImageAdaptor<TImage , TAccessor>::RegionType &
ImageAdaptor<TImage , TAccessor>
::GetRequestedRegion()
{
  return m_Image->GetRequestedRegion();
}





} // end namespace itk


