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

template<class TPixel, unsigned int VDimension>
bool
SmartRegionNeighborhoodIterator<TPixel, VDimension>
::InBounds()
{
  bool ans = true;
  for (int i=0; i<VDimension; i++)
    {
      if (m_Loop[i] < m_InnerBoundsLow[i] || m_Loop[i] >= m_InnerBoundsHigh[i])
        {
          m_InBounds[i] = ans = false;
        }
    }
  return ans;
}

template<class TPixel, unsigned int VDimension>
Neighborhood<TPixel, VDimension>
SmartRegionNeighborhoodIterator<TPixel, VDimension>
::GetNeighborhood()
{
  const Iterator _end = this->end();
  Neighborhood ans;
  Neighborhood::Iterator ans_it;
  Iterator this_it;

  ans.SetRadius( this->GetRadius() );
  
  if ( this->GetBoundsChecking() ) // Bounds checking.
    {
      
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
          register int i;
          int OverlapLow[VDimension];
          int OverlapHigh[VDimension];
          int temp[VDimension];
          bool flag;

          // Calculate overlap & initialize index
          for (i=0; i<VDimension; i++)
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
              for (i=0; i<VDimension; ++i)
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
                  *ans_it = 0;
                }

              for (i=0; i<VDimension; ++i)  // Update index
                {
                  temp[i]++;
                  if ( temp[i] == this->GetSize(i) ) temp[i]= 0;
                  else break;
                }
            }
          
        }
    }

  else  // No bounds checking.
    {
      for (ans_it = ans.begin(), this_it = this->begin();
           this_it < _end; ans_it++, this_it++)
        {
          *ans_it = **this_it;
        }
    }
  
  return ans;
  
}

template<class TPixel, unsigned int VDimension>
void
SmartRegionNeighborhoodIterator<TPixel, VDimension>
::SetNeighborhood(Neighborhood &N)
{
  const Iterator _end = this->end();
  Iterator this_it;
  Neighborhood::Iterator N_it;
  
  if ( this->GetBoundsChecking() ) // Bounds checking
    {

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
          register int i;
          int OverlapLow[VDimension];
          int OverlapHigh[VDimension];
          int temp[VDimension];
          bool flag;

          // Calculate overlap & initialize index
          for (i=0; i<VDimension; i++)
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
              for (i=0; i<VDimension; ++i)
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
              
              for (i=0; i<VDimension; ++i)  // Update index
                {
                  temp[i]++;
                  if ( temp[i] == this->GetSize(i) ) temp[i]= 0;
                  else break;
                }
            }
          
        }
    }  
  else  // No bounds checking
    {
      for (this_it = this->begin(); this_it < _end; this_it++, N_it++)
        {
          **this_it = *N_it;
        }
    }
}

template<class TPixel, unsigned int VDimension>
TPixel
SmartRegionNeighborhoodIterator<TPixel, VDimension>
::InnerProduct(std::valarray<TPixel> &v)
{
  
  TPixel sum = 0;
  TPixel *it;
  const TPixel *_end = &(v[v.size()]);
  Iterator this_it;

  if ( this->GetBoundsChecking() ) // Bounds Checking
    {
      if (InBounds())
        {
          for (it = &(v[0]), this_it = this->begin();
               it < _end; ++it, ++this_it)
            {
              sum += *it * **this_it; 
            }
        }
      else
        {
          // Need to rewrite this if it proves useful since it will actually
          // slow down the RegionBoundaryNeighborhoodIterator somewhat...
          // jc 8-9-00
          return this->GetNeighborhood().InnerProduct(v);
        }
    }
  else
    {
      for (it = &(v[0]), this_it = this->begin();
           it < _end; ++it, ++this_it)
        {
          sum += *it * **this_it; 
        }
    }

  return sum;
}

template<class TPixel, unsigned int VDimension>
void SmartRegionNeighborhoodIterator<TPixel, VDimension>
::Print()
{
  int i;
  //NeighborhoodBase<TPixel, VDimension>::Print();
  std::cout << "SmartRegionNeighborhoodIterator" << std::endl;
  std::cout << "        this = " << this << std::endl;
  std::cout << "this->size() = " << this->size() << std::endl;
  std::cout << "    m_Bound = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_Bound[i] << " ";
  std::cout << "}" << std::endl;
  std::cout << "    m_InnerBoundsLow = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_InnerBoundsLow[i] << " ";
  std::cout << "}" << std::endl;
  std::cout << "    m_InnerBoundsHigh = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_InnerBoundsHigh[i] << " ";
  std::cout << "}" << std::endl;
  std::cout << "     m_Loop = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_Loop[i] << " ";
  std::cout << "}" << std::endl;
  std::cout << "   m_WrapOffset = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_WrapOffset[i] << " ";
  std::cout << "}" << std::endl;
   std::cout << "   m_StartIndex = { ";
  for (i = 0; i<VDimension; i++) std::cout << m_StartIndex[i] << " ";
  std::cout << "}" << std::endl;
  
}

template<class TPixel, unsigned int VDimension>
void SmartRegionNeighborhoodIterator<TPixel, VDimension>
::SetBound(const unsigned long bound[VDimension])
{
  const unsigned long *radius     = this->GetRadius();
  const unsigned long *offset     = m_Image->GetOffsetTable();
  const unsigned long *regionSize = m_Image->GetRegionSize();
  const unsigned long *bufferSize = m_Image->GetBufferSize();

  // Set the bounds and the wrapping offsets.
  for (int i=0; i<VDimension; ++i)
    {
      m_Bound[i]           = m_StartIndex[i]+bound[i];
      m_InnerBoundsHigh[i] = m_StartIndex[i]+ (regionSize[i] - radius[i]);
      m_InnerBoundsLow[i]  = m_StartIndex[i]+ radius[i];
      m_WrapOffset[i]      = (bufferSize[i] - (m_Bound[i] - m_StartIndex[i]))
                              * offset[i];
    }
}

template<class TPixel, unsigned int VDimension>
SmartRegionNeighborhoodIterator<TPixel, VDimension>
SmartRegionNeighborhoodIterator<TPixel, VDimension>
::Begin()
{
  //Copy the current iterator
  Self it( *this );

  // Set the position to the m_BeginOffset
  it.SetLocation( this->m_StartIndex );

  return it;
}

template<class TPixel, unsigned int VDimension>
SmartRegionNeighborhoodIterator<TPixel, VDimension>
SmartRegionNeighborhoodIterator<TPixel, VDimension>
::End()
{
  Index endIndex;
  
  // Copy the current iterator
  Self it( *this );

  // Calculate the end index
  endIndex.m_Index[0] = m_StartIndex[0] + m_Bound[0];
  for (int i = 1; i< VDimension; ++i)
    {
      endIndex.m_Index[i] = m_StartIndex[i] + m_Bound[i] -1;
    }
  
  // Set the position to the m_BeginOffset
  it.SetLocation( endIndex );

  return it;
}


} // end namespace itk
