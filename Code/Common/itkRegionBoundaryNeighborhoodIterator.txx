/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionBoundaryNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk {

template<class TPixel, unsigned int VDimension>
void
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
::SetBound(const SizeType& size)
{
  SmartRegionNeighborhoodIterator<TPixel, VDimension>::SetBound(size);
  m_InnerStride = (m_Bound[0] - m_StartIndex[0]) - 2*this->GetRadius(0);
}

template<class TPixel, unsigned int VDimension>
const NeighborhoodIterator<TPixel, VDimension> &
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
::operator++()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();

  if (! this->InBounds())
    {
      NeighborhoodIterator<TPixel, VDimension>::operator++();
    }

  while(this->InBounds())
    {
      // Increment pointers.
      for (it = this->begin(); it < _end; ++it)
        {
          (*it)+= m_InnerStride;;
        }
      if (m_OutputBuffer)
        {
          m_OutputBuffer += m_InnerStride;
        }
      
      // Check loop bounds, wrap & add pointer offsets if needed.
      for (i=0; i < VDimension; ++i)
        {
          if (i==0)
            {
              m_Loop[0] += m_InnerStride;
            }
          else
            {
              m_Loop[i]++;
            }
          
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
    }
  return *this; 
}

template<class TPixel, unsigned int VDimension>
const NeighborhoodIterator<TPixel, VDimension> &
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
::operator--()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = this->end();

  if (! this->InBounds())
    {
      NeighborhoodIterator<TPixel, VDimension>::operator--();
    }

  while(this->InBounds())
    {
      // Decrement pointers.
      for (it = this->begin(); it < _end; ++it)
        {
          (*it)-= m_InnerStride;;
        }
      if (m_OutputBuffer)
        {
          m_OutputBuffer -= m_InnerStride;
        }
      
      // Check loop bounds, wrap & add pointer offsets if needed.
      for (i=0; i < VDimension; ++i)
        {
          if (i==0)
            {
              m_Loop[0] -= m_InnerStride;
            }
          else
            {
              m_Loop[i]--;
            }
          
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
    }
  return *this; 
}

template<class TPixel, unsigned int VDimension>
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
::Begin() const
{
  //Copy the current iterator
  Self it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TPixel, unsigned int VDimension>
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
::End() const
{
  IndexType endIndex;
  
  // Copy the current iterator
  Self it( *this );

  // Calculate the end index
  for (unsigned int i = 0; i< VDimension; ++i)
    {
      endIndex.m_Index[i] = m_Bound[i] -1;
    }
  
  it.SetLocation( endIndex );
  ++it;

  return it;
}

template<class TPixel, unsigned int VDimension>
void RegionBoundaryNeighborhoodIterator<TPixel, VDimension>
::PrintSelf()   // -- Note: this method is for developmental/
{           //          debugging purposes and should be
            //          removed at some point.
  std::cout << "RegionBoundaryNeighborhoodIterator" << std::endl;
  std::cout << "\tm_InnerStride = " << m_InnerStride << std::endl;
  SmartRegionNeighborhoodIterator<TPixel, VDimension>::PrintSelf();
}

} // namespace itk
