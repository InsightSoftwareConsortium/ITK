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
template<class TPixel, unsigned int TImageDimension>
Image<TPixel, TImageDimension>
::Image()
{
  m_Data = 0;
  this->SetDimension( TImageDimension );
}


/**
 *
 */
template<class TPixel, unsigned int TImageDimension>
Image<TPixel, TImageDimension>
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
template<class TPixel, unsigned int TImageDimension>
void 
Image<TPixel, TImageDimension>
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
template<class TPixel, unsigned int TImageDimension>
void 
Image<TPixel, TImageDimension>
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
template<class TPixel, unsigned int TImageDimension>
const TPixel& Image<TPixel, TImageDimension>
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
template <class TPixel, unsigned int TImageDimension>
ImageIterator<TPixel, TImageDimension> 
Image<TPixel, TImageDimension>
::Begin()
{
  Iterator ind;
  long indexOrigin[TImageDimension];

  memset(indexOrigin, 0, TImageDimension*sizeof(long));
  
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
 *           m_ImageSize[TImageDimension-2]-1, m_ImageSize[TImageDimension-1]]
 */
template <class TPixel, unsigned int TImageDimension>
ImageIterator<TPixel, TImageDimension> 
Image<TPixel, TImageDimension>
::End()
{
  Iterator ind;
  long indexOrigin[TImageDimension];
  const unsigned long *size = this->GetSize();

  memset(indexOrigin, 0, TImageDimension*sizeof(long));
  
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
  for (unsigned int i=0; i < TImageDimension; i++)
    {
    indexOrigin[i] = indexOrigin[i] + size[i] - 1;
    }
  indexOrigin[TImageDimension-1]++;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}

/**
 *
 */
template <class TPixel, unsigned int TImageDimension>
ImageScalarIterator<TPixel, TImageDimension> 
Image<TPixel, TImageDimension>
::ScalarBegin()
{
  ScalarIterator ind;
  long indexOrigin[TImageDimension];
  
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
 *           m_ImageSize[TImageDimension-2]-1, m_ImageSize[TImageDimension-1]]
 */
template <class TPixel, unsigned int TImageDimension>
ImageScalarIterator<TPixel, TImageDimension> 
Image<TPixel, TImageDimension>
::ScalarEnd()
{
  ScalarIterator ind;
  long indexOrigin[TImageDimension];
  const unsigned long *size = this->GetSize();

  memset(indexOrigin, 0, TImageDimension*sizeof(long));
  
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
  for (int i=0; i < TImageDimension; i++)
    {
    indexOrigin[i] = indexOrigin[i] + size[i] - 1;
    }
  indexOrigin[TImageDimension-1]++;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}


/**
 *
 */
template<class TPixel, unsigned int TImageDimension>
void Image<TPixel, TImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageBase::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Data << std::endl;
}

} // namespace itk
