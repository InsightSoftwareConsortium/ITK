/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhood.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhood_txx
#define _itkNeighborhood_txx
namespace itk {

template<class TPixel, unsigned int VDimension, class TContainer>
unsigned long
Neighborhood<TPixel, VDimension, TContainer>
::GetStride(const unsigned long axis) const
{
  unsigned long stride = 0;

  size_t accum = 1;
  for (unsigned int i=0; i<VDimension; ++i)
    {
      if (i == axis) stride = accum;
      accum *= m_Size[i];
    }

  return stride;
}

template<class TPixel, unsigned int VDimension, class TContainer>
void
Neighborhood<TPixel, VDimension, TContainer>
::SetRadius(const unsigned long s)
{
  SizeType k;
  for (unsigned int i = 0; i< VDimension; i++)
    {
      k[i] = s;
    }
  this->SetRadius(k);
}

template<class TPixel, unsigned int VDimension, class TContainer>
void
Neighborhood<TPixel, VDimension, TContainer>
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

template<class TPixel, unsigned int VDimension, class TContainer>
void Neighborhood<TPixel, VDimension, TContainer>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;

  os << indent << "m_Size: [ ";
  for (i=0; i<VDimension; ++i) os << m_Size[i] << " ";
  os << "]" << std::endl;
  
  os << indent << "m_Radius: [ ";
  for (i=0; i<VDimension; ++i) os << m_Radius[i] << " ";
  os << "]" << std::endl;  
}

}  // namespace itk

#endif
