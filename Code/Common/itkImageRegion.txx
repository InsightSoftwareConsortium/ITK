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
  Index nullIndex = {0};
  m_Index = nullIndex;
  memset( m_Size, 0, VImageDimension*sizeof(unsigned long) );
}

/**
 * Destructor for the ImageRegion class.
 */
template<unsigned int VImageDimension>
ImageRegion<VImageDimension>
::~ImageRegion()
{
}

/**
 * Print internal instance variables.
 */
template<unsigned int VImageDimension>
void 
ImageRegion<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: " << m_Index;
  os << indent << "Size: [";
  for (unsigned int i=0; i < VImageDimension - 1; ++i)
    {
    os << m_Size[i] << ", ";
    }
  if (VImageDimension >= 1)
    {
    os << m_Size[VImageDimension-1];
    }
  os << "]" << std::endl;
}

} // end namespace itk

