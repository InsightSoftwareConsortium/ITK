/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartRegionNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk {

template<class TImage, class TAllocator, class TDerefAllocator>
bool
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::InBounds()
{
  bool ans = true;
  for (unsigned int i=0; i<Dimension; i++)
    {
      if (m_Loop[i] < m_InnerBoundsLow[i] || m_Loop[i] >= m_InnerBoundsHigh[i])
        {
          m_InBounds[i] = ans = false;
        }
    }
  return ans;
}

template<class TImage, class TAllocator, class TDerefAllocator>
typename SmartRegionNeighborhoodIterator<TImage, TAllocator,
  TDerefAllocator>::NeighborhoodType
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::GetNeighborhood()
{
  const Iterator _end = this->end();
  NeighborhoodType ans;
  typename NeighborhoodType::Iterator ans_it;
  Iterator this_it;

  ans.SetRadius( this->GetRadius() );
  
  if (InBounds())
    {
      for (ans_it = ans.begin(), this_it = this->begin();
           this_it < _end; ans_it++, this_it++)
        {
          *ans_it = **this_it;
        }
    }
  else
    {
      register unsigned int i;
      unsigned int OverlapLow[Dimension];
      unsigned int OverlapHigh[Dimension];
      unsigned int temp[Dimension];
      bool flag;
      
      // Calculate overlap & initialize index
      for (i=0; i<Dimension; i++)
        {
          OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
          OverlapHigh[i]= this->GetSize(i)
            - (m_Loop[i]-m_InnerBoundsHigh[i])-1;
          temp[i] = 0;
        }
      
      // Iterate through neighborhood
      for (ans_it = ans.begin(), this_it = this->begin();
           this_it < _end; ans_it++, this_it++)
        {
          flag = true;
          for (i=0; i<Dimension; ++i)
            {
              if (!m_InBounds[i] && ((temp[i] < OverlapLow[i])
                                     || (temp[i] >= OverlapHigh[i])) )
                {
                  flag=false;
                  break;
                }
            }
          
          if (flag)
            {
              *ans_it = **this_it;
            }
          
          else  // Boundary condition here
            {
              // *ans_it = m_BoundaryCondition(this);
              *ans_it = NumericTraits<ScalarValueType>::Zero;
            }
          
          for (i=0; i<Dimension; ++i)  // Update index
            {
              temp[i]++;
              if ( temp[i] == this->GetSize(i) ) temp[i]= 0;
              else break;
            }
        }
      
    }
   
  return ans;
  
}

template<class TImage, class TAllocator, class TDerefAllocator>
void
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::SetNeighborhood(NeighborhoodType &N)
{
  const Iterator _end = this->end();
  Iterator this_it;
  typename  NeighborhoodType::Iterator N_it;
  
  if (InBounds())
    {
      for (N_it = N.begin(), this_it = begin(); this_it < _end;
           this_it++, N_it++)
        {
          **this_it = *N_it;
        }  
    }
  else
    {
      register unsigned int i;
      unsigned int OverlapLow[Dimension];
      unsigned int OverlapHigh[Dimension];
      unsigned int temp[Dimension];
      bool flag;
      
      // Calculate overlap & initialize index
      for (i=0; i<Dimension; i++)
        {
          OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
          OverlapHigh[i]= this->GetSize(i)
            - (m_Loop[i]-m_InnerBoundsHigh[i])-1;
          temp[i] = 0;
        }
      
      // Iterate through neighborhood
      for (N_it = N.begin(), this_it = this->begin();
           this_it < _end; N_it++, this_it++)
        {
          flag = true;
          for (i=0; i<Dimension; ++i)
            {
              if (!m_InBounds[i] && ((temp[i] < OverlapLow[i])
                                     || (temp[i] >= OverlapHigh[i])) )
                {
                  flag=false;
                  break;
                }
            }
          
          if (flag)
            {
              **this_it = *N_it;
            }
          
          for (i=0; i<Dimension; ++i)  // Update index
            {
              temp[i]++;
              if ( temp[i] == this->GetSize(i) ) temp[i]= 0;
              else break;
            }
        }
      
    }

}

template<class TImage, class TAllocator, class TDerefAllocator>
void SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;
  os << indent << "SmartRegionNeighborhoodIterator { this = " << this
     << ", m_InnerBoundsLow = { ";
  for (i = 0; i<Dimension; i++) os << m_InnerBoundsLow[i] << " ";
  os << "}, m_InnerBoundsHigh = { ";
  for (i = 0; i<Dimension; i++) os << m_InnerBoundsHigh[i] << " ";
  os << "} }" << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());
}

template<class TImage, class TAllocator, class TDerefAllocator>
void SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator>
::SetBound(const SizeType& size)
{
  SizeType radius  = this->GetRadius();
  const unsigned long *offset     = m_Image->GetOffsetTable();
  const IndexType imageRRStart  = m_Image->GetRequestedRegion().GetIndex();
  SizeType imageRRSize = m_Image->GetRequestedRegion().GetSize();
  SizeType imageBufferSize = m_Image->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets. Inner bounds are the loop
  // indicies where the iterator will begin to overlap the edge of the image
  // requested region.
  for (unsigned int i=0; i<Dimension; ++i)
    {
      m_Bound[i]          = m_StartIndex[i]+size[i];
      m_InnerBoundsHigh[i]= imageRRStart[i] + ( imageRRSize[i] - radius[i] );
      m_InnerBoundsLow[i] = imageRRStart[i] + radius[i];
      m_WrapOffset[i]     = (imageBufferSize[i] - (m_Bound[i]
                              - m_StartIndex[i])) * offset[i];
    }
}

template<class TImage, class TAllocator, class TDerefAllocator>
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator> 
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator> 
::Begin() const 
{
  //Copy the current iterator
  Self it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TImage, class TAllocator, class TDerefAllocator>
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator> 
SmartRegionNeighborhoodIterator<TImage, TAllocator, TDerefAllocator> 
::End() const
{
  IndexType endIndex;
  
  // Copy the current iterator
  Self it( *this );

  // Calculate the end index
  for (unsigned int i = 0; i< Dimension; ++i)
    {
      endIndex.m_Index[i] = m_Bound[i] -1;
    }
  
  // Set the position to the m_BeginOffset
  it.SetLocation( endIndex );

  ++it;
  return it;
}


} // end namespace itk
