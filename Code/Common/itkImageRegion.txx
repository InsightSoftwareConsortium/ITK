/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegion.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageRegion_txx
#define _itkImageRegion_txx
#include "itkImageRegion.h"

namespace itk
{

/**
 * Instantiate object.
 */
template<unsigned int VImageDimension>
ImageRegion<VImageDimension>
::ImageRegion()
{
  for(unsigned int i=0; i<VImageDimension; i++)
  {
    m_Index[i] = 0; 
    m_Size[i]  = 0;
  }
}

/**
 * Destructor for the ImageRegion class.
 */
template<unsigned int VImageDimension>
ImageRegion<VImageDimension>
::~ImageRegion()
{
}

template<unsigned int VImageDimension>
unsigned long 
ImageRegion<VImageDimension>
::GetNumberOfPixels() const
{
  int numPixels=1;

  for (unsigned int i=0; i<VImageDimension; i++)
    {
    numPixels *= m_Size[i];
    }
  
  return numPixels;
}


} // end namespace itk


#endif
