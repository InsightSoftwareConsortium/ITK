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
  
  os << indent << "ImageAdaptor " << std::endl;
  os << std::endl;
}





//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetSpacing(const float spacing[TImage::ImageDimension] )
{
  m_Image->SetSpacing( spacing );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const float *
ImageAdaptor<TImage , TAccessor>
::GetSpacing() const
{
  return m_Image->GetSpacing();
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
void 
ImageAdaptor<TImage , TAccessor>
::SetOrigin(const float origin[TImage::ImageDimension] )
{
  m_Image->SetOrigin( origin );
}



//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
const float *
ImageAdaptor<TImage , TAccessor>
::GetOrigin() const
{
  return m_Image->GetOrigin();
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
}




//----------------------------------------------------------------------------
template <class TImage, class TAccessor >
typename TAccessor::InternalType  * 
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



} // end namespace itk


