/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConstNeighborhoodIterator_txx
#define _itkConstNeighborhoodIterator_txx
#include "itkConstNeighborhoodIterator.h"
namespace itk {

template<class TImage, class TBoundaryCondition>
bool
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::InBounds() const
{ 
  bool ans = true;
  for (unsigned int i=0; i<Dimension; i++)
    {
      if (m_Loop[i] < m_InnerBoundsLow[i] || m_Loop[i] >= m_InnerBoundsHigh[i])
        {
        m_InBounds[i] = ans = false;
        break;
        }
    }
  return ans;
}


  
template<class TImage, class TBoundaryCondition>
typename ConstNeighborhoodIterator<TImage, TBoundaryCondition>::PixelType
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::GetPixel(const unsigned n) const
{
  // If the region the iterator is walking (padded by the neighborhood size)
  // never bumps up against the bounds of the buffered region, then don't
  // bother checking any boundary conditions
  if (!m_NeedToUseBoundaryCondition)
    {
    return (*(this->operator[](n)));
    }

  register unsigned int i;
  OffsetValueType OverlapLow, OverlapHigh;
  OffsetType temp, offset;
  bool flag;

  // Is this whole neighborhood in bounds?
  if (this->InBounds())
    {
    return (*(this->operator[](n)));
    }
  else
    {
    temp = this->ComputeInternalIndex(n);
      
    flag = true;

    // Is this pixel in bounds?
    for (i=0; i<Dimension; ++i)
      {
      if (m_InBounds[i]) offset[i] = 0; // this dimension in bounds
      else  // part of this dimension spills out of bounds
        {
        // Calculate overlap for this dimension
        OverlapLow = m_InnerBoundsLow[i] - m_Loop[i];
        OverlapHigh =
          static_cast<OffsetValueType>(this->GetSize(i) - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i] ));

        // 
        if (temp[i] < OverlapLow)
          {
          flag = false;
          offset[i] = OverlapLow - temp[i];
          }
        else if ( OverlapHigh < temp[i] )
          {
          flag = false;
          offset[i] =  OverlapHigh - temp[i];
          }
        else offset[i] = 0;
        }
      }

    if (flag) 
      {
      return ( *(this->operator[](n)) ) ;
      }
    else 
      {
      return( m_BoundaryCondition->operator()(temp, offset, this) );
      }
    } 
}
  
  
template<class TImage, class TBoundaryCondition>
typename ConstNeighborhoodIterator<TImage, TBoundaryCondition>::OffsetType
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::ComputeInternalIndex(unsigned int n) const
{
  OffsetType ans;
  long D = (long)Dimension;
  unsigned long r;
  r = (unsigned long)n;
  for (long i = D-1; i >= 0 ; --i)
    {
      ans[i] = static_cast<OffsetValueType>(r / this->GetStride(i));
      r = r % this->GetStride(i);
    }
  return ans;
}


template <class TImage, class TBoundaryCondition>
typename ConstNeighborhoodIterator<TImage, TBoundaryCondition>::RegionType
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::GetBoundingBoxAsImageRegion() const
{
  RegionType ans;
  ans.SetIndex(this->GetIndex(0));
  ans.SetSize(this->GetSize());
  
  return ans;
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::ConstNeighborhoodIterator()
{
  IndexType zeroIndex; zeroIndex.Fill(0);
  SizeType  zeroSize; zeroSize.Fill(0);

  m_Bound.Fill(0);
  m_Begin = 0;
  m_BeginIndex.Fill(0);
  // m_ConstImage
  m_End   = 0;
  m_EndIndex.Fill(0);
  m_Loop.Fill(0);
  m_Region.SetIndex(zeroIndex);
  m_Region.SetSize(zeroSize);
  
  m_WrapOffset.Fill(0);
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::ConstNeighborhoodIterator(const Self& orig)
  : Neighborhood<InternalPixelType *, Dimension>(orig)
{
  m_Bound      = orig.m_Bound;
  m_Begin      = orig.m_Begin;
  m_BeginIndex = orig.m_BeginIndex;  
  m_ConstImage = orig.m_ConstImage;
  m_End        = orig.m_End;
  m_EndIndex   = orig.m_EndIndex;
  m_Loop       = orig.m_Loop;
  m_Region     = orig.m_Region;
  m_WrapOffset = orig.m_WrapOffset;

  m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;
  m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;
  for (unsigned int i = 0; i < Dimension; ++i)
    {
    m_InBounds[i] = orig.m_InBounds[i];
    }

  m_InnerBoundsLow  = orig.m_InnerBoundsLow;
  m_InnerBoundsHigh = orig.m_InnerBoundsHigh;

 // Check to see if the default boundary
  // conditions have been overridden.
  if ( orig.m_BoundaryCondition ==
       static_cast<ImageBoundaryConditionConstPointerType>(&orig.m_InternalBoundaryCondition ))
    {
    this->ResetBoundaryCondition();
    }
  else 
    { m_BoundaryCondition = orig.m_BoundaryCondition; }
  
}

template<class TImage, class TBoundaryCondition>
void
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::SetEndIndex()
{
  m_EndIndex = m_Region.GetIndex();
  m_EndIndex[Dimension-1] = m_Region.GetIndex()[Dimension-1] +
    static_cast<long>(m_Region.GetSize()[Dimension-1]);
}

template<class TImage, class TBoundaryCondition> 
typename ConstNeighborhoodIterator<TImage, TBoundaryCondition>::NeighborhoodType
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::GetNeighborhood() const
{
  register unsigned int i;
  OffsetType OverlapLow, OverlapHigh, temp, offset;
  bool flag;

  const ConstIterator _end = this->End();
  NeighborhoodType ans;
  typename NeighborhoodType::Iterator ans_it;
  ConstIterator this_it;

  ans.SetRadius( this->GetRadius() );

  if (m_NeedToUseBoundaryCondition == false)
    {
      for (ans_it = ans.Begin(), this_it = this->Begin();
           this_it < _end; ans_it++, this_it++)
        { *ans_it = **this_it; }
    }
  else if (InBounds())
    {
      for (ans_it = ans.Begin(), this_it = this->Begin();
           this_it < _end; ans_it++, this_it++)
        { *ans_it = **this_it; }
    }
  else
    {
      // Calculate overlap & initialize index
      for (i=0; i<Dimension; i++)
        {
          OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
          OverlapHigh[i]=
            static_cast<OffsetValueType>(this->GetSize(i)) - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i] );
          temp[i] = 0;
        }

      // Iterate through neighborhood
      for (ans_it = ans.Begin(), this_it = this->Begin();
           this_it < _end; ans_it++, this_it++)
        {
          flag = true;
          
          // Is this pixel in bounds?
          for (i=0; i<Dimension; ++i)
            {
              if (m_InBounds[i]) offset[i] = 0; // this dimension in bounds
              else  // part of this dimension spills out of bounds
                {
                  if (temp[i] < OverlapLow[i])
                    {
                      flag = false;
                      offset[i] = OverlapLow[i] - temp[i];
                    }
                  else if ( OverlapHigh[i] < temp[i] )
                    {
                      flag = false;
                      offset[i] =  OverlapHigh[i] - temp[i];
                    }
                  else offset[i] = 0;
                }
            }
          
          if (flag) *ans_it = **this_it;
          else *ans_it = m_BoundaryCondition->operator()(temp, offset, this);
          
          for (i=0; i<Dimension; ++i)  // Update index
            {
              temp[i]++;
              if ( temp[i] == static_cast<OffsetValueType>(this->GetSize(i)) ) temp[i]= 0; 
              else break;
            }
        } 
    }
  return ans;
}

template<class TImage, class TBoundaryCondition>
void
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::GoToBegin()
{
  this->SetLocation( m_BeginIndex );
}

template<class TImage, class TBoundaryCondition>
void
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::GoToEnd()
{
  this->SetLocation( m_EndIndex );
}

template<class TImage, class TBoundaryCondition>
void ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::Initialize(const SizeType &radius, const ImageType *ptr,
             const RegionType &region)
{ 
  m_ConstImage = ptr;
  m_Region = region;
  const IndexType regionIndex = region.GetIndex();

  this->SetRadius(radius);
  this->SetBeginIndex(region.GetIndex());
  this->SetLocation(region.GetIndex());
  this->SetBound(region.GetSize());
  this->SetEndIndex();
  
  m_Begin = ptr->GetBufferPointer() + ptr->ComputeOffset(regionIndex);

  m_End = ptr->GetBufferPointer() + ptr->ComputeOffset( m_EndIndex );

 // now determine whether boundary conditions are going to be needed
  const IndexType bStart = ptr->GetBufferedRegion().GetIndex();
  const SizeType  bSize  = ptr->GetBufferedRegion().GetSize();
  const IndexType rStart = region.GetIndex();
  const SizeType  rSize  = region.GetSize();

  long  overlapLow, overlapHigh;

  m_NeedToUseBoundaryCondition = false;
  for (unsigned long i = 0; i < Dimension; ++i)
    {
    overlapLow = static_cast<long>((rStart[i] - radius[i]) - bStart[i]);
    overlapHigh= static_cast<long>((bStart[i] + bSize[i]) - (rStart[i] + rSize[i] + radius[i]));

    if (overlapLow < 0) // out of bounds condition, define a region of 
      {
      m_NeedToUseBoundaryCondition = true;
      break;
      }

    if (overlapHigh < 0)
      {
      m_NeedToUseBoundaryCondition = true;
      break;
      }
    }
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::operator=(const Self& orig)
{
  Superclass::operator=(orig);

  m_Bound        = orig.m_Bound;
  m_Begin        = orig.m_Begin;
  m_ConstImage   = orig.m_ConstImage;
  m_End          = orig.m_End;
  m_EndIndex     = orig.m_EndIndex;
  m_Loop         = orig.m_Loop;
  m_Region       = orig.m_Region;
  m_BeginIndex = orig.m_BeginIndex;
  m_WrapOffset = orig.m_WrapOffset;

 m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;
  m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;
  
  m_InnerBoundsLow  = orig.m_InnerBoundsLow;
  m_InnerBoundsHigh = orig.m_InnerBoundsHigh;
  
  for (unsigned int i = 0; i < Dimension; ++i)
    {
      m_InBounds[i] = orig.m_InBounds[i];
    }

  // Check to see if the default boundary conditions
  // have been overridden.
  if (orig.m_BoundaryCondition ==
      static_cast<ImageBoundaryConditionConstPointerType>( &orig.m_InternalBoundaryCondition ) )
    {
      this->ResetBoundaryCondition();
    }
  else m_BoundaryCondition = orig.m_BoundaryCondition;

  return *this;
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::operator++()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = Superclass::End();

  // Increment pointers.
  for (it = Superclass::Begin(); it < _end; ++it)
    {
      (*it)++;
    }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      m_Loop[i]++;
      if ( m_Loop[i] == m_Bound[i] )
        {
          m_Loop[i] = m_BeginIndex[i];
          for (it = Superclass::Begin(); it < _end; ++it)
            {
              (*it) += m_WrapOffset[i];
            }
        }        
      else break;
    }
  return *this;
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::operator--()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = Superclass::End();
  
  // Decrement pointers.
  for (it = Superclass::Begin(); it < _end; ++it)
    {
      (*it)--;
    }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      if (m_Loop[i] == m_BeginIndex[i])
        {
          m_Loop[i]= m_Bound[i] - 1;
          for (it = Superclass::Begin(); it < _end; ++it)
            {
              (*it) -= m_WrapOffset[i];
            }
        }        
      else
        {
          m_Loop[i]--;
          break;
        }
    }
  return *this;
}

template<class TImage, class TBoundaryCondition>
void
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;
  os << indent;
  os << "ConstNeighborhoodIterator {this= " << this;
  os << ", m_Region = { Start = {";
  for (i=0; i < Dimension; ++i) os << m_Region.GetIndex()[i] << " ";
  os << "}, Size = { ";
  for (i=0; i < Dimension; ++i) os << m_Region.GetSize()[i] << " ";
  os << "} }";
  os << ", m_BeginIndex = { ";
  for (i=0; i < Dimension; ++i) os << m_BeginIndex[i] << " ";
  os << "} , m_EndIndex = { ";
  for (i=0; i < Dimension; ++i) os << m_EndIndex[i] << " ";
  os << "} , m_Loop = { ";
  for (i=0; i < Dimension; ++i) os << m_Loop[i] << " ";
  os << "}, m_Bound = { ";
  for (i=0; i < Dimension; ++i) os << m_Bound[i] << " ";
  os << "}, m_WrapOffset = { ";
  for (i=0; i < Dimension; ++i) os << m_WrapOffset[i] << " ";

  os << ", m_Begin = " << m_Begin;
  os << ", m_End = " << m_End;
  os << "}"  << std::endl;

  os << indent << ",  m_InnerBoundsLow = { ";
  for (i = 0; i<Dimension; i++) os << m_InnerBoundsLow[i] << " ";
  os << "}, m_InnerBoundsHigh = { ";
  for (i = 0; i<Dimension; i++) os << m_InnerBoundsHigh[i] << " ";
  os << "} }" << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());  
}

template<class TImage, class TBoundaryCondition>
void ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::SetBound(const SizeType& size)
{
  SizeType radius  = this->GetRadius();
  const OffsetValueType *offset   = m_ConstImage->GetOffsetTable();
  const IndexType imageRRStart  = m_ConstImage->GetRequestedRegion().GetIndex();
  SizeType imageRRSize = m_ConstImage->GetRequestedRegion().GetSize();
  SizeType imageBufferSize = m_ConstImage->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets. Inner bounds are the loop
  // indicies where the iterator will begin to overlap the edge of the image
  // requested region.
  for (unsigned int i=0; i<Dimension; ++i)
    {
      m_Bound[i]          = m_BeginIndex[i] + static_cast<IndexValueType>(size[i]);
      m_InnerBoundsHigh[i]= static_cast<IndexValueType>(imageRRStart[i] + ( imageRRSize[i]) - static_cast<SizeValueType>(radius[i]) );
      m_InnerBoundsLow[i] = static_cast<IndexValueType>(imageRRStart[i] + radius[i]);
      m_WrapOffset[i]     = (static_cast<OffsetValueType>(imageBufferSize[i]) - ( m_Bound[i]
                              - m_BeginIndex[i] )) * offset[i];
    }
  m_WrapOffset[Dimension-1] = 0; // last offset is zero because there are no
                                 // higher dimensions  
}


template<class TImage, class TBoundaryCondition>
void ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::SetPixelPointers(const IndexType &pos)
{
  const Iterator _end = Superclass::End();
  InternalPixelType * Iit;
  ImageType *ptr = const_cast<ImageType *>(m_ConstImage.GetPointer());
  const SizeType size = this->GetSize();
  const OffsetValueType *OffsetTable = m_ConstImage->GetOffsetTable();
  const SizeType radius = this->GetRadius();

  unsigned int i;
  Iterator Nit;
  SizeType loop;
  for (i=0; i<Dimension; ++i) loop[i]=0;

  // Find first "upper-left-corner"  pixel address of neighborhood
  Iit = ptr->GetBufferPointer() + ptr->ComputeOffset(pos);

  for (i = 0; i<Dimension; ++i)
    {
      Iit -= radius[i] * OffsetTable[i];
    }

  // Compute the rest of the pixel addresses
  for (Nit = Superclass::Begin(); Nit != _end; ++Nit)
    {
      *Nit = Iit;
      ++Iit;
      for (i = 0; i <Dimension; ++i)
        {
          loop[i]++;
          if ( loop[i] == size[i] )
            {
              if (i==Dimension-1) break;
              Iit +=  OffsetTable[i+1] - OffsetTable[i] * static_cast<long>(size[i]);
              loop[i]= 0;
            }
          else break;
        }
    }
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstNeighborhoodIterator<TImage, TBoundaryCondition>
::operator+=(const OffsetType & idx)
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->End();
  OffsetValueType accumulator = 0;
  const OffsetValueType* stride = this->GetImagePointer()->GetOffsetTable();

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];
  
  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets. 
  for (i = 1; i< Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for (it = this->Begin(); it < _end; ++it)
    {
      (*it) += accumulator;
    }

  // Update loop counter values
  m_Loop += idx;

  return *this;
}

template<class TImage, class TBoundaryCondition>
ConstNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstNeighborhoodIterator<TImage, TBoundaryCondition> 
::operator-=(const OffsetType & idx)
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->End();
  OffsetValueType accumulator = 0;
  const OffsetValueType* stride = this->GetImagePointer()->GetOffsetTable();

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];
  
  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets. 
  for (i = 1; i< Dimension; ++i)
    {
      accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for (it = this->Begin(); it < _end; ++it)
    {
      (*it) -= accumulator;
    }

  // Update loop counter values
  m_Loop -= idx;

  return *this;
}



} // namespace itk

#endif
