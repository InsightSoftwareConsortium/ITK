/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef _itkNeighborhoodIterator_txx
#define _itkNeighborhoodIterator_txx
namespace itk {

template<class TImage, class TAllocator, class TDerefAllocator>
void
NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;
  os << indent;
  os << "NeighborhoodIterator {this= " << this;
  os << ", m_Region = { Start = {";
  for (i=0; i < Dimension; ++i) os << m_Region.GetIndex()[i] << " ";
  os << "}, Size = { ";
  for (i=0; i < Dimension; ++i) os << m_Region.GetSize()[i] << " ";
  os << "} }";
  os << ", m_StartIndex = { ";
  for (i=0; i < Dimension; ++i) os << m_StartIndex[i] << " ";
  os << ", m_Loop = { ";
  for (i=0; i < Dimension; ++i) os << m_Loop[i] << " ";
  os << "}, m_Bound = { ";
  for (i=0; i < Dimension; ++i) os << m_Bound[i] << " ";
  os << "}, m_WrapOffset = { ";
  for (i=0; i < Dimension; ++i) os << m_WrapOffset[i] << " ";
  os << "}, m_OutputWrapOffsetModifier = { ";
  for (i=0; i < Dimension; ++i) os << m_OutputWrapOffsetModifier[i] << " ";
  os << "}, m_OutputBuffer = " << m_OutputBuffer;
  //  os << ", m_Image = " << m_Image;
  os << ", m_Buffer = " << m_Buffer;
  os << ", m_EndPointer = " << m_EndPointer;
  os << "}"  << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());
}

template<class TImage, class TAllocator, class TDerefAllocator>
NeighborhoodIterator<TImage, TAllocator, TDerefAllocator> &
NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::operator=(const Self& orig)
{
  Superclass::operator=(orig);
  memcpy(m_WrapOffset, orig.m_WrapOffset, sizeof(unsigned long) *
         Dimension);
  memcpy(m_Bound, orig.m_Bound, sizeof(unsigned long) * Dimension);
  memcpy(m_Loop, orig.m_Loop, sizeof(unsigned long) * Dimension);
  m_OutputBuffer = orig.m_OutputBuffer;
  m_Image = orig.m_Image;
  m_Buffer = orig.m_Buffer;
  m_StartIndex = orig.m_StartIndex;
  m_EndPointer = orig.m_EndPointer;
  return *this;
}

template<class TImage, class TAllocator,  class TDerefAllocator>
NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::NeighborhoodIterator(const Self& orig)
  : Neighborhood<InternalPixelType *, Dimension, TAllocator>(orig)
{
  memcpy(m_WrapOffset, orig.m_WrapOffset, sizeof(unsigned long) *
         Dimension);
  memcpy(m_Bound, orig.m_Bound, sizeof(unsigned long) * Dimension);
  memcpy(m_Loop, orig.m_Loop, sizeof(unsigned long) * Dimension);
  m_OutputBuffer = orig.m_OutputBuffer;
  m_Image = orig.m_Image;
  m_Buffer = orig.m_Buffer;
  m_StartIndex = orig.m_StartIndex;
  m_EndPointer = orig.m_EndPointer;
}
  
template<class TImage, class TAllocator, class TDerefAllocator>
void NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::Initialize(const SizeType &radius, ImageType *ptr, const RegionType &region)
{
  m_Region = region;
  m_OutputBuffer = 0;
  memset(m_OutputWrapOffsetModifier, 0, sizeof(long) * Dimension);
  this->SetRadius(radius);
  m_Image = ptr;
  m_Buffer = m_Image->GetBufferPointer();
  this->SetStartIndex(region.GetIndex());
  this->SetLocation(region.GetIndex());
  this->SetBound(region.GetSize());
  this->SetEnd();
}

template<class TImage, class TAllocator, class TDerefAllocator>
void NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::SetPixelPointers(const IndexType &offset)
{
  const Iterator _end = this->end();
  unsigned int i;
  Iterator Nit;
  InternalPixelType * Iit;
  unsigned long loop[Dimension];
  const SizeType size = this->GetSize();
  const unsigned long *OffsetTable = m_Image->GetOffsetTable();
  memset(loop, 0, sizeof(long) * Dimension);
  const SizeType radius = this->GetRadius();
  
  // Find first "upper-left-corner"  pixel address of neighborhood
  Iit = m_Buffer + m_Image->ComputeOffset(offset);
  
  for (i = 0; i<Dimension; ++i)
    {
      Iit -= radius[i] * OffsetTable[i];
    }
  
  // Compute the rest of the pixel addresses
  for (Nit = this->begin(); Nit != _end; ++Nit)
    {
      *Nit = Iit;
      ++Iit;
      for (i = 0; i <Dimension; ++i)
        {
          loop[i]++;
          if ( loop[i] == size[i] )
            {
              if (i==Dimension-1) break;
              Iit +=  OffsetTable[i+1] - OffsetTable[i] * size[i];
              loop[i]= 0;
            }
          else break;
        }      
    }
}
  
template<class TImage, class TAllocator, class TDerefAllocator>
const NeighborhoodIterator<TImage, TAllocator, TDerefAllocator> &
NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::operator++()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();
  
  // Increment pointers.
  for (it = this->begin(); it < _end; ++it)
    {
      (*it)++;
    }
  if (m_OutputBuffer)
    {
      ++m_OutputBuffer;
    }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      m_Loop[i]++;
      if ( m_Loop[i] == m_Bound[i] )
        {
          m_Loop[i]= m_StartIndex[i];
          for (it = this->begin(); it < _end; ++it)
            {
              (*it) += m_WrapOffset[i];
            }
          if (m_OutputBuffer)
            {
              m_OutputBuffer += m_WrapOffset[i]
                + m_OutputWrapOffsetModifier[i];
            }
        }        
      else break;
    }
  return *this;
}

template<class TImage, class TAllocator, class TDerefAllocator>
const NeighborhoodIterator<TImage, TAllocator, TDerefAllocator> &
NeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::operator--()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();
  
  // Decrement pointers.
  for (it = this->begin(); it < _end; ++it)
    {
      (*it)--;
    }
  if (m_OutputBuffer)
    {
      --m_OutputBuffer;
    }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      m_Loop[i]--;
      if ( m_Loop[i] < m_StartIndex[i] )
        {
          m_Loop[i]= m_Bound[i] - 1;
          for (it = this->begin(); it < _end; ++it)
            {
              (*it) -= m_WrapOffset[i];
            }
          if (m_OutputBuffer)
            {
              m_OutputBuffer -= m_WrapOffset[i]
                + m_OutputWrapOffsetModifier[i];
            }
        }        
      else break;
    }
  return *this;
}

} // namespace itk

#endif
