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

template<class T>
itkImage<T>::Pointer itkImage<T>::New()
{
  return itkImage<T>::Pointer(new itkImage<T>);
}

template<class T>
itkImage<T>::itkImage()
  : m_Data(0)
{
}

template<class T>
itkImage<T>::~itkImage()
{
  if (m_Data != 0)
    {
    delete m_Data;
    m_Data = 0;
    }
}

template<class T>
void itkImage<T>::Allocate()
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

template<class T>
void itkImage<T>::SetPixel(int *index, const T& value)
{
  unsigned long offset=0;
  unsigned long prod=1;
  
  const int *size = this->GetSize();
  int dimension = this->GetDimension();
  
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[dimension-1-i];
    prod *= size[dimension-1-i];
    }
  
  (*m_Data)[offset] = value;
  
  std::cerr << "Vector stats: " << std::endl
	    << "\tcapacity = " << m_Data->capacity() << std::endl
	    << "\tsize = " << m_Data->size() << std::endl;
}

template<class T>
const T& itkImage<T>::GetPixel(int *index)
{
  unsigned long offset=0;
  unsigned long prod=1;
  
  const int *size = this->GetSize();
  int dimension = this->GetDimension();
  
  for (int i=dimension-1; i >= 0; i--)
    {
    offset += prod*index[dimension-1-i];
    prod *= size[dimension-1-i];
    }
  
  return ( (*m_Data)[offset] );
  
}
