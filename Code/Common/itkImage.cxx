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

#include "itkImage.h"

template<class T, unsigned int TImageDimension>
itkImage<T, TImageDimension>::Pointer itkImage<T, TImageDimension>::New()
{
  return
    itkImage<T, TImageDimension>::Pointer(new itkImage<T, TImageDimension>);
}

template<class T, unsigned int TImageDimension>
itkImage<T, TImageDimension>::itkImage()
  : m_Data(0)
{
   this->SetDimension( TImageDimension );
}

template<class T, unsigned int TImageDimension>
itkImage<T, TImageDimension>::~itkImage()
{
  if (m_Data != 0)
    {
    delete m_Data;
    m_Data = 0;
    }
}

template<class T, unsigned int TImageDimension>
void itkImage<T, TImageDimension>::Allocate()
{
  unsigned long num=1;
  const int *size = this->GetSize();
  
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
void itkImage<T, TImageDimension>::SetPixel(const itkImage::Index &ind, const T& value)
{
  unsigned long offset=0;
  unsigned long prod=1;

  const unsigned long *index = ind.GetIndex();
  
  const int *size = this->GetSize();
  int dimension = this->GetDimension();
  
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[dimension-1-i];
    prod *= size[dimension-1-i];
    }
  
  (*m_Data)[offset] = value;
}

template<class T, unsigned int TImageDimension>
const T& itkImage<T, TImageDimension>::GetPixel(const itkImage::Index &ind)
{
  unsigned long offset=0;
  unsigned long prod=1;
  
  const unsigned long *index = ind.GetIndex();

  const int *size = this->GetSize();
  int dimension = this->GetDimension();
  
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[dimension-1-i];
    prod *= size[dimension-1-i];
    }
  
  return ( (*m_Data)[offset] );
}
