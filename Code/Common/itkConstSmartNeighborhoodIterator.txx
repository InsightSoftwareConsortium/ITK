/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstSmartNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkConstSmartNeighborhoodIterator_txx
#define _itkConstSmartNeighborhoodIterator_txx
namespace itk {

template<class TImage, class TBoundaryCondition>
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>::PixelType
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::GetPixel(const unsigned long n) const
{
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
            this->GetSize(i) - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i] );
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

      if (flag) return ( *(this->operator[](n)) ) ;
      else return( m_BoundaryCondition->operator()(temp, offset, this) );
    } 
}
  
template<class TImage, class TBoundaryCondition>
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::ConstSmartNeighborhoodIterator(const Self& orig)
  : ConstNeighborhoodIterator<TImage>(orig)
{ std::cout << "CALLING constNeighboIt copy constructor " << std::endl;
  m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;

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
  else m_BoundaryCondition = orig.m_BoundaryCondition;  
}

template<class TImage,class TBoundaryCondition>
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition> &  
ConstSmartNeighborhoodIterator<TImage, TBoundaryCondition>
::operator=(const Self& orig)
{
  Superclass::operator=(orig);

  this->m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;
  
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
  return *this;
}
  
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
  
  if (InBounds())
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
            this->GetSize(i) - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i] );
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
              if ( (unsigned int)(temp[i]) == this->GetSize(i) ) temp[i]= 0; 
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
  const unsigned long *offset   = m_ConstImage->GetOffsetTable();
  const IndexType imageRRStart  = m_ConstImage->GetRequestedRegion().GetIndex();
  SizeType imageRRSize = m_ConstImage->GetRequestedRegion().GetSize();
  SizeType imageBufferSize = m_ConstImage->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets. Inner bounds are the loop
  // indicies where the iterator will begin to overlap the edge of the image
  // requested region.
  for (unsigned int i=0; i<Dimension; ++i)
    {
      m_Bound[i]          = m_BeginIndex[i] + size[i];
      m_InnerBoundsHigh[i]= imageRRStart[i] + ( imageRRSize[i] - radius[i] );
      m_InnerBoundsLow[i] = imageRRStart[i] + radius[i];
      m_WrapOffset[i]     = (imageBufferSize[i] - ( m_Bound[i]
                              - m_BeginIndex[i] )) * offset[i];
    }
  m_WrapOffset[Dimension-1] = 0; // last offset is zero because there are no
                                 // higher dimensions
}

} // end namespace itk

#endif
