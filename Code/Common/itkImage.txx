/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImage_txx
#define _itkImage_txx
#include "itkImage.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
Image<TPixel, VImageDimension, TImageTraits>
::Image()
: m_DataAccessor ()
{
  m_Buffer = PixelContainer::New();
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
Image<TPixel, VImageDimension, TImageTraits>
::~Image()
{
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::Allocate()
{
  unsigned long num;

  this->ComputeOffsetTable();
  num = this->GetOffsetTable()[VImageDimension];
  
  m_Buffer->Reserve(num);
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Buffer << std::endl;
}


} // end namespace itk

#endif
