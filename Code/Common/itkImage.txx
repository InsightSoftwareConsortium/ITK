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
#include "itkImage.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::Image()
{
  m_Data = 0;
  this->SetDimension( VImageDimension );
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::~Image()
{
  if (m_Data != 0)
    {
    delete m_Data;
    m_Data = 0;
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::Allocate()
{
  unsigned long num=1;
  const unsigned long *size = this->GetSize();
  
  for (unsigned int i=0; i < this->GetDimension(); i++)
    {
    num *= size[i];
    }
  
  if (m_Data == 0)
    { 
    m_Data = new std::vector<TPixel>(num);
    }
  else
    {
    m_Data->reserve(num);
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetPixel(const Index &ind, const TPixel& value)
{
  // add bounds checking for the region/image
  unsigned long offset=0;
  unsigned long prod=1;

  const unsigned long *index = ind.GetIndex();
  
  const unsigned long *size = this->GetSize();
  unsigned int dimension = this->GetDimension();
  
  // data is arranged as [][][][slice][row][col]
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[i]; 
    prod *= size[i];
    }
  
  (*m_Data)[offset] = value;
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
const TPixel& Image<TPixel, VImageDimension>
::GetPixel(const Index &ind)
{
  /**
   * add bounds checking for the region/image
   */
  unsigned long offset=0;
  unsigned long prod=1;
  
  const unsigned long *index = ind.GetIndex();

  const unsigned long *size = this->GetSize();
  unsigned int dimension = this->GetDimension();

  /**
   * data is arranged as [][][][slice][row][col]
   */
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[i];
    prod *= size[i];
    }
  
  return ( (*m_Data)[offset] );
}


/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
ImageIterator<TPixel, VImageDimension> 
Image<TPixel, VImageDimension>
::Begin()
{
  Iterator ind;
  long indexOrigin[VImageDimension];

  memset(indexOrigin, 0, VImageDimension*sizeof(long));
  
  /**
   * Set the BasePointer, Pointer, RegionBasePointer, RegionPointer,
   * ImageSize, RegionSize of the image into the interator
   */
  ind.SetPointer( m_Data->begin() );
  ind.SetImageSize( this->GetSize() );
  ind.SetRegionSize( this->GetSize() );
  ind.SetImageIndexOrigin( indexOrigin );
  ind.SetRegionIndexOrigin( indexOrigin );

  /**
   * set the default i,j,k of the index for the iterator
   */
  Iterator::Index index;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}


/**
 * The End() of the image is one pixel past the last pixel.  The index of this
 * pixel is [m_ImageSize[0]-1, m_ImageSize[1]-1, ...,
 *           m_ImageSize[VImageDimension-2]-1, m_ImageSize[VImageDimension-1]]
 */
template <class TPixel, unsigned int VImageDimension>
ImageIterator<TPixel, VImageDimension> 
Image<TPixel, VImageDimension>
::End()
{
  Iterator ind;
  long indexOrigin[VImageDimension];
  const unsigned long *size = this->GetSize();

  memset(indexOrigin, 0, VImageDimension*sizeof(long));
  
  /**
   * Set the BasePointer, Pointer, RegionBasePointer, RegionPointer,
   * ImageSize, RegionSize of the image into the interator
   */
  ind.SetPointer( m_Data->end() );
  ind.SetImageSize( this->GetSize() );
  ind.SetRegionSize( this->GetSize() );
  ind.SetImageIndexOrigin( indexOrigin );
  ind.SetRegionIndexOrigin( indexOrigin );

  /**
   * set the default i,j,k of the index for the iterator
   */
  Iterator::Index index;
  for (unsigned int i=0; i < VImageDimension; i++)
    {
    indexOrigin[i] = indexOrigin[i] + size[i] - 1;
    }
  indexOrigin[VImageDimension-1]++;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}

/**
 *
 */
template <class TPixel, unsigned int VImageDimension>
ImageScalarIterator<TPixel, VImageDimension> 
Image<TPixel, VImageDimension>
::ScalarBegin()
{
  ScalarIterator ind;
  long indexOrigin[VImageDimension];
  
  /**
   * Set the BasePointer, Pointer, RegionBasePointer, RegionPointer,
   * ImageSize, RegionSize of the image into the interator
   */
  ind.SetPointer( m_Data->begin() );
  ind.SetImageSize( this->GetSize() );
  ind.SetRegionSize( this->GetSize() );
  ind.SetImageIndexOrigin( indexOrigin );
  ind.SetRegionIndexOrigin( indexOrigin );

  /**
   * set the default i,j,k of the index for the iterator
   */
  ScalarIterator::Index index;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}


/**
 * The End() of the image is one pixel past the last pixel.  The index of this
 * pixel is [m_ImageSize[0]-1, m_ImageSize[1]-1, ...,
 *           m_ImageSize[VImageDimension-2]-1, m_ImageSize[VImageDimension-1]]
 */
template <class TPixel, unsigned int VImageDimension>
ImageScalarIterator<TPixel, VImageDimension> 
Image<TPixel, VImageDimension>
::ScalarEnd()
{
  ScalarIterator ind;
  long indexOrigin[VImageDimension];
  const unsigned long *size = this->GetSize();

  memset(indexOrigin, 0, VImageDimension*sizeof(long));
  
  /**
   * Set the BasePointer, Pointer, RegionBasePointer, RegionPointer,
   * ImageSize, RegionSize of the image into the interator
   */
  ind.SetPointer( m_Data->end() );
  ind.SetImageSize( this->GetSize() );
  ind.SetRegionSize( this->GetSize() );
  ind.SetImageIndexOrigin( indexOrigin );
  ind.SetRegionIndexOrigin( indexOrigin );

  /**
   * set the default i,j,k of the index for the iterator
   */
  ScalarIterator::Index index;
  for (int i=0; i < VImageDimension; i++)
    {
    indexOrigin[i] = indexOrigin[i] + size[i] - 1;
    }
  indexOrigin[VImageDimension-1]++;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
void Image<TPixel, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageBase::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Data << std::endl;
}

} // namespace itk
