/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstSmartNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConstSmartNeighborhoodIterator_txx
#define _itkConstSmartNeighborhoodIterator_txx
#include "itkConstSmartNeighborhoodIterator.h"

namespace itk {
/*
template<class TImage, class TBoundaryCondition>
typename ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>::PixelType
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::GetPixel(const unsigned long n) const
{
  // If the region the iterator is walking (padded by the neighborhood size)
  // never bumps up against the bounds of the buffered region, then don't
  // bother checking any boundary conditions
  if (!m_NeedToUseBoundaryCondition)
    {
    return (*(this->operator[](n)));
    }

  register unsigned int i;
  OffsetType OverlapLow, OverlapHigh, temp, offset;
  bool flag;

  // Is this whole neighborhood in bounds?
  if (this->InBounds()) return (*(this->operator[](n)));
  else
    {
     temp = this->ComputeInternalIndex(n);
      
      // Calculate overlap
      for (i=0; i<Dimension; i++)
        {
          OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
          OverlapHigh[i]=
            static_cast<OffsetValueType>(this->GetSize(i) - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i] ));
        }

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
  */  
  //template<class TImage, class TBoundaryCondition>
  //ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
  //::ConstSmartNeighborhoodIterator(const Self& orig)
  //  : ConstNeighborhoodIterator<TImage>(orig)
  //{
  /*
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
       (ImageBoundaryConditionPointerType)&orig.m_InternalBoundaryCondition )
    {
    this->ResetBoundaryCondition();
    }
  else 
  { m_BoundaryCondition = orig.m_BoundaryCondition; }
  */
  //}

template<class TImage,class TBoundaryCondition>
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition> &  
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::operator=(const Self& orig)
{
  Superclass::operator=(orig);
  /*
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
      (ImageBoundaryConditionPointerType) &orig.m_InternalBoundaryCondition ) 
    {
      this->ResetBoundaryCondition();
    }
    else m_BoundaryCondition = orig.m_BoundaryCondition;
  */
  return *this;
}
  
//template<class TImage, class TBoundaryCondition>
//void ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
//::Initialize(const SizeType &radius, const ImageType *ptr,
//             const RegionType &region)
//{
// call the superclass' version
//  Superclass::Initialize(radius, ptr, region);
/*
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
    //  std::cout << "BoundaryConditions are " << (m_NeedToUseBoundaryCondition
    //  ? "needed" : "not needed") << std::endl;
    */
//}

/*
template<class TImage, class TBoundaryCondition>
bool
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
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
typename ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::NeighborhoodType
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
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
void ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;
  os << indent << "ConstSmartNeighborhoodIterator { this = " << this
     << ", m_InnerBoundsLow = { ";
  for (i = 0; i<Dimension; i++) os << m_InnerBoundsLow[i] << " ";
  os << "}, m_InnerBoundsHigh = { ";
  for (i = 0; i<Dimension; i++) os << m_InnerBoundsHigh[i] << " ";
  os << "} }" << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());
}

template<class TImage, class TBoundaryCondition>
void ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
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
*/
} // end namespace itk

#endif

