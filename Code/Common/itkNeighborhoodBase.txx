/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include <iostream>

namespace itk {

template<class TPixel, unsigned int VDimension>
unsigned long
NeighborhoodBase<TPixel, VDimension>
::GetStride(const unsigned long axis) const
{
  unsigned long stride;

  size_t accum = 1;
  for (unsigned int i=0; i<VDimension; ++i)
    {
      if (i == axis) stride = accum;
      accum *= m_Size[i];
    }

  return stride;
}

template<class TPixel, unsigned int VDimension>
void
NeighborhoodBase<TPixel, VDimension>
::SetRadius(const unsigned long s)
{
  SizeType k;
  for (unsigned int i = 0; i< VDimension; i++)
    {
      k[i] = s;
    }
  this->SetRadius(k);
}

template<class TPixel, unsigned int VDimension>
void
NeighborhoodBase<TPixel, VDimension>
::SetRadius(const SizeType &r)
{
  memcpy(m_Radius.m_Size, r.m_Size, sizeof(const unsigned long)*VDimension);
  this->SetSize();

  unsigned int cumul=1;
  for (unsigned int i = 0; i<VDimension; i++)
    {
      cumul*=m_Size[i];
    }

   this->Allocate(cumul);
}

template<class TPixel, unsigned int VDimension>
void NeighborhoodBase<TPixel, VDimension>
::PrintSelf()            // Note -- Debugging function for development
{                    //         that should probably be removed at
                     //         some point.  jc 10-05-00  
  std::cout << "NeighborhoodBase" << std::endl;
  std::cout << "        this = " << this << std::endl;
  std::cout << "  VDimension = " << VDimension << std::endl;
  std::cout << "    m_Radius = { ";
  for (unsigned int i = 0; i<VDimension; i++) std::cout << m_Radius[i] << " ";
  std::cout << "}" << std::endl;
  std::cout << "    m_Size   = { ";
  for (unsigned int i = 0; i<VDimension; i++) std::cout << m_Size[i] << " ";
  std::cout << "}" << std::endl;
}

}  // namespace itk
