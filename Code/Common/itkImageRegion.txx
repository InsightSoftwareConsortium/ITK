/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegion.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  m_Index.Fill(0);
  m_Size.Fill(0);
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
  unsigned long numPixels=1;

  for (unsigned int i=0; i<VImageDimension; i++)
    {
    numPixels *= m_Size[i];
    }
  
  return numPixels;
}

template<unsigned int VImageDimension>
void
ImageRegion<VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  
  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: " << this->GetIndex() << std::endl;
  os << indent << "Size: " << this->GetSize() << std::endl;
}


template<unsigned int VImageDimension>
void
ImageRegion<VImageDimension>
::PadByRadius(unsigned long radius)
{
  unsigned long radiusVector[VImageDimension];

  for (unsigned int i=0; i < VImageDimension; ++i)
    {
    radiusVector[i] = radius;
    }

  this->PadByRadius(radiusVector);
}

template<unsigned int VImageDimension>
void
ImageRegion<VImageDimension>
::PadByRadius(const SizeType &radius)
{
  this->PadByRadius( radius.GetSize() );
}

template<unsigned int VImageDimension>
void
ImageRegion<VImageDimension>
::PadByRadius(const unsigned long radius[VImageDimension])
{
  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    m_Size[i] += 2 * radius[i];
    m_Index[i] -= static_cast<long>(radius[i]);
    }  
}

template<unsigned int VImageDimension>
bool
ImageRegion<VImageDimension>
::Crop(const Self& region)
{
  long crop;
  unsigned int i;
  bool cropPossible = true;

  // Can we crop?
  for (i = 0; i < VImageDimension && cropPossible; i++)
    {
    // Is left edge of current region to the right of the right edge
    // of the region to crop with? (if so, we cannot crop)
    if (m_Index[i] >= region.GetIndex()[i]
        + static_cast<long>(region.GetSize()[i]))
      {
      cropPossible = false;
      }
    // If right edge of the current region to the left of the left
    // edge of the region to crop with? (if so, we cannot crop)
    if (m_Index[i] + static_cast<long>(m_Size[i]) <= region.GetIndex()[i])
      {
      cropPossible = false;
      }
    }

  // if we cannot crop, return without changing anythin
  if (!cropPossible)
    {
    return cropPossible;
    }

  // we can crop, so crop
  for (i=0; i < VImageDimension; i++)
    {
    // first check the start index
    if (m_Index[i] < region.GetIndex()[i])
      {
      // how much do we need to adjust
      crop = region.GetIndex()[i] - m_Index[i];

      // adjust the start index and the size of the current region
      m_Index[i] += crop;
      m_Size[i] -= static_cast<unsigned long>(crop);
      }
    // now check the final size
    if (m_Index[i] + static_cast<long>(m_Size[i])
        > region.GetIndex()[i] + static_cast<long>(region.GetSize()[i]))
      {
      // how much do we need to adjust
      crop = m_Index[i] + static_cast<long>(m_Size[i])
        - region.GetIndex()[i] - static_cast<long>(region.GetSize()[i]);

      // adjust the size
      m_Size[i] -= static_cast<unsigned long>(crop);
      }
    }

  return cropPossible;
}


template<unsigned int VImageDimension>
typename ImageRegion<VImageDimension>::SliceRegion
ImageRegion<VImageDimension>
::Slice(const unsigned long dim) const
{
  Index<SliceDimension> sliceIndex;
  Size<SliceDimension> sliceSize;

  unsigned int ii = 0;
  for (unsigned int i=0; i < VImageDimension; i++)
    {
    if (i != dim)
      {
      sliceIndex[ii] = m_Index[i];
      sliceSize[ii] = m_Size[i];
      ++ii;
      }
    }

  return ImageRegion<SliceDimension>(sliceIndex, sliceSize);
}


} // end namespace itk
#endif
