/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk {
  
template<class TPixel, unsigned int VDimension>
Neighborhood<TPixel, VDimension>
RegionNeighborhoodIterator<TPixel, VDimension>
::GetNeighborhood()
{
  Neighborhood ans;
  Neighborhood::Iterator ans_it;
  Iterator this_it;
  const Iterator _end = this->end();

  ans.SetRadius( this->GetRadius() );

  for (ans_it = ans.begin(), this_it = this->begin();
       this_it < _end; ans_it++, this_it++)
    {
      *ans_it = **this_it;
    }

  return ans;
}

template<class TPixel, unsigned int VDimension>
void
RegionNeighborhoodIterator<TPixel, VDimension>
::SetNeighborhood(Neighborhood &N)
{
  Iterator this_it;
  const Iterator _end = this->end();
  Neighborhood::Iterator N_it;
  
  for (this_it = this->begin(); this_it < _end; this_it++, N_it++)
    {
      **this_it = *N_it;
    }

}
  
template<class TPixel, unsigned int VDimension>
void RegionNeighborhoodIterator<TPixel, VDimension>
::Print()   // --- Note: This method is for development/debugging
{           //           and should be removed at some point.
  int i;    //           jc 10-05-00
  //NeighborhoodBase<TPixel, VDimension>::Print();
  std::cout << "RegionNeighborhoodIterator" << std::endl;
  std::cout << "        this = " << this << std::endl;
  std::cout << "this->size() = " << this->size() << std::endl;
  std::cout << "    m_Bound = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_Bound[i] << " ";
  std::cout << "}" << std::endl;
  std::cout << "     m_Loop = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_Loop[i] << " ";
  std::cout << "}" << std::endl;
}
  
template<class TPixel, unsigned int VDimension>
void RegionNeighborhoodIterator<TPixel, VDimension>
::SetBound(const Size& size)
{
  const unsigned long *offset     = m_Image->GetOffsetTable();
  const unsigned long *bufferSize
    = m_Image->GetBufferedRegion().GetSize().m_Size;

  // Set the bounds and the wrapping offsets
  for (int i=0; i<VDimension; ++i)
    {
      m_Bound[i]      = m_StartIndex[i]+size[i];
      m_WrapOffset[i] = (bufferSize[i] - (m_Bound[i] - m_StartIndex[i]))
                        * offset[i];
    }  
}

template<class TPixel, unsigned int VDimension>
TPixel
RegionNeighborhoodIterator<TPixel, VDimension>
::InnerProduct(std::valarray<TPixel> &v)
{
  TPixel sum = 0;
  TPixel *it;
  Iterator this_it;
  const TPixel *_end = &(v[v.size()]);
  
  for (it = &(v[0]), this_it = this->begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * **this_it; 
    }
  return sum;
}


template<class TPixel, unsigned int VDimension>
RegionNeighborhoodIterator<TPixel, VDimension>
RegionNeighborhoodIterator<TPixel, VDimension>
::Begin()
{
  //Copy the current iterator
  Self it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TPixel, unsigned int VDimension>
RegionNeighborhoodIterator<TPixel, VDimension>
RegionNeighborhoodIterator<TPixel, VDimension>
::End()
{
  Index endIndex;
  
  // Copy the current iterator
  Self it( *this );

  // Calculate the end index
  endIndex.m_Index[0] = m_Bound[0];
  for (int i = 1; i< VDimension; ++i)
    {
      endIndex.m_Index[i] = m_Bound[i] -1;
    }
  it.SetLocation( endIndex );

  return it;
}

} // namespace itk
