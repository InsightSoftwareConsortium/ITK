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
  m_Buffer = 0;

  memset( m_ImageSize, 0, VImageDimension*sizeof(unsigned long) );
  memset( m_BufferSize, 0, VImageDimension*sizeof(unsigned long) );
  memset( m_RegionSize, 0, VImageDimension*sizeof(unsigned long) );

  memset( m_OffsetTable, 0, VImageDimension*sizeof(unsigned long) );

  unsigned int i;
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
  //unsigned long prod=1;
  
  for (unsigned int i=0; i < VImageDimension; i++)
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

template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::Initialize()
{
  unsigned long size[VImageDimension];
  Index ind;

  std::cout << "Image::Initialize()" << std::endl;
  
  ind[0] = m_WholeExtent[0];
  ind[1] = m_WholeExtent[2];
  ind[2] = m_WholeExtent[4];
  size[0] = m_WholeExtent[1] - m_WholeExtent[0] + 1;
  size[1] = m_WholeExtent[3] - m_WholeExtent[2] + 1;
  size[2] = m_WholeExtent[5] - m_WholeExtent[4] + 1;
  this->SetImageStartIndex(ind);
  this->SetImageSize(size);

  ind[0] = m_Extent[0];
  ind[1] = m_Extent[2];
  ind[2] = m_Extent[4];
  size[0] = m_Extent[1] - m_Extent[0] + 1;
  size[1] = m_Extent[3] - m_Extent[2] + 1;
  size[2] = m_Extent[5] - m_Extent[4] + 1;
  this->SetBufferStartIndex(ind);
  this->SetBufferSize(size);

  ind[0] = m_UpdateExtent[0];
  ind[1] = m_UpdateExtent[2];
  ind[2] = m_UpdateExtent[4];
  size[0] = m_UpdateExtent[1] - m_UpdateExtent[0] + 1;
  size[1] = m_UpdateExtent[3] - m_UpdateExtent[2] + 1;
  size[2] = m_UpdateExtent[5] - m_UpdateExtent[4] + 1;
  this->SetRegionStartIndex(ind);
  this->SetRegionSize(size);
  
  this->Allocate();
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

} // end namespace itk
