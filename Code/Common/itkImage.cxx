/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/

// #include "itkImage.h"
#include "itkObjectFactory.h"

template<class T, unsigned int TImageDimension>
itkImage<T, TImageDimension>::Pointer itkImage<T, TImageDimension>
::New()
{
  itkImage<T, TImageDimension>* ret = 
    itkObjectFactory<itkImage<T, TImageDimension> >::Create();
  if ( ret )
    {
    return ret;
    }
  return
    itkImage<T, TImageDimension>::Pointer(new itkImage<T, TImageDimension>);
}

template<class T, unsigned int TImageDimension>
itkImage<T, TImageDimension>
::itkImage()
  : m_Data(0)
{
   this->SetDimension( TImageDimension );
}

template<class T, unsigned int TImageDimension>
itkImage<T, TImageDimension>
::~itkImage()
{
  if (m_Data != 0)
    {
    delete m_Data;
    m_Data = 0;
    }
}

template<class T, unsigned int TImageDimension>
void itkImage<T, TImageDimension>
::Allocate()
{
  unsigned long num=1;
  const unsigned long *size = this->GetSize();
  
  for (int i=0; i < this->GetDimension(); i++)
    {
    num *= size[i];
    }
  
  if (m_Data == 0)
    { 
    m_Data = new std::vector<T>(num);
    }
  else
    {
    m_Data->reserve(num);
    }
}

template<class T, unsigned int TImageDimension>
void itkImage<T, TImageDimension>
::SetPixel(const Index &ind, const T& value)
{
  // add bounds checking for the region/image
  unsigned long offset=0;
  unsigned long prod=1;

  const long *index = ind.GetIndex();
  
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

template<class T, unsigned int TImageDimension>
const T& itkImage<T, TImageDimension>
::GetPixel(const Index &ind)
{
  // add bounds checking for the region/image
  unsigned long offset=0;
  unsigned long prod=1;
  
  const long *index = ind.GetIndex();

  const unsigned long *size = this->GetSize();
  unsigned int dimension = this->GetDimension();

  // data is arranged as [][][][slice][row][col]
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[i];
    prod *= size[i];
    }
  
  return ( (*m_Data)[offset] );
}

template <class T, unsigned int TImageDimension>
itkImageIterator<T, TImageDimension> itkImage<T, TImageDimension>
::Begin()
{
  Iterator ind;
  long indexOrigin[TImageDimension];

  memset(indexOrigin, 0, TImageDimension*sizeof(long));
  
  // Set the BasePointer, Pointer, RegionBasePointer, RegionPointer,
  // ImageSize, RegionSize of the image into the interator
  ind.SetPointer( m_Data->begin() );
  ind.SetImageSize( this->GetSize() );
  ind.SetRegionSize( this->GetSize() );
  ind.SetImageIndexOrigin( indexOrigin );
  ind.SetRegionIndexOrigin( indexOrigin );

  // set the default i,j,k of the index for the iterator
  Iterator::Index index;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}


//
// The End() of the image is one pixel past the last pixel.  The index of this
// pixel is [m_ImageSize[0]-1, m_ImageSize[1]-1, ...,
//           m_ImageSize[TImageDimension-2]-1, m_ImageSize[TImageDimension-1]]
//
template <class T, unsigned int TImageDimension>
itkImageIterator<T, TImageDimension> itkImage<T, TImageDimension>
::End()
{
  Iterator ind;
  long indexOrigin[TImageDimension];

  memset(indexOrigin, 0, TImageDimension*sizeof(long));
  
  // Set the BasePointer, Pointer, RegionBasePointer, RegionPointer,
  // ImageSize, RegionSize of the image into the interator
  ind.SetPointer( m_Data->end() );
  ind.SetImageSize( this->GetSize() );
  ind.SetRegionSize( this->GetSize() );
  ind.SetImageIndexOrigin( indexOrigin );
  ind.SetRegionIndexOrigin( indexOrigin );

  // set the default i,j,k of the index for the iterator
  Iterator::Index index;
  for (int i=0; i < TImageDimension; i++)
    {
    indexOrigin[i] = indexOrigin[i] + m_Size[i] - 1;
    }
  indexOrigin[TImageDimension-1]++;
  index.SetIndex( indexOrigin );
  ind.SetIndex( index );
  
  return ind;
}
