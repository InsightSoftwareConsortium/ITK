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

ITK_NAMESPACE_BEGIN

/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::Image()
{
  m_Buffer = 0;

  memset( m_ImageSize, 0, VImageDimension*sizeof(unsigned long) );
  memset( m_BufferSize, 0, VImageDimension*sizeof(unsigned long) );
  memset( m_RegionSize, 0, VImageDimension*sizeof(unsigned long) );

  memset( m_OffsetTable, 0, VImageDimension*sizeof(unsigned long) );

  int i;
  for (i=0; i < VImageDimension; i++)
    {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::~Image()
{
  if (m_Buffer != 0)
    {
    delete m_Buffer;
    m_Buffer = 0;
    }
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::Allocate()
{
  unsigned long num=1;
  unsigned long prod=1;
  
  for (int i=0; i < VImageDimension; i++)
    {
    m_OffsetTable[i] = num;
    num *= m_BufferSize[i];
    }
  
  if (m_Buffer == 0)
    { 
    m_Buffer = new std::valarray<TPixel>(num);
    }
  else
    {
    m_Buffer->resize(num);
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageBase::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Buffer << std::endl;
}

ITK_NAMESPACE_END
