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
  int i;
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
                  m_OutputBuffer += m_WrapOffset[i];
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
::Begin()
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
::End()
{
  IndexType endIndex;
  
  // Copy the current iterator
  Self it( *this );

  // Calculate the end index
  endIndex.m_Index[0] = m_Bound[0];
  for (int i = 1; i< VDimension; ++i)
    {
      endIndex.m_Index[i] = m_Bound[i] -1;
    }
  
  // Set the position to the m_BeginOffset
  it.SetLocation( endIndex );

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
