/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPhysicalImageAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
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
::PrintSelf(std::ostream& os, Indent indent)
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


