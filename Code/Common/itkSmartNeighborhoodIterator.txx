/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartNeighborhoodIterator.txx
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
#ifndef _itkSmartNeighborhoodIterator_txx
#define _itkSmartNeighborhoodIterator_txx
namespace itk {

template<class TImage, class TBoundaryCondition>
void
SmartNeighborhoodIterator<TImage, TBoundaryCondition>
::SetPixel(const unsigned long n, const PixelType& v)
{
  register unsigned int i;
  OffsetType OverlapLow, OverlapHigh, temp, offset;
  bool flag;

  // Is this whole neighborhood in bounds?
  if (this->InBounds()) *(this->operator[](n)) = v;
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

      if (flag) *(this->operator[](n)) = v ;
      else
        { // Attempt to write out of bounds
          throw RangeError();
        };
    } 
}
  
template<class TImage, class TBoundaryCondition>
void
SmartNeighborhoodIterator<TImage, TBoundaryCondition>
::SetNeighborhood(NeighborhoodType &N)
{
  register unsigned int i;
  OffsetType OverlapLow, OverlapHigh, temp;
  bool flag;
      
  const Iterator _end = this->End();
  Iterator this_it;
  typename  NeighborhoodType::Iterator N_it;
  
  if (InBounds())
    {
      for (N_it = N.Begin(), this_it = Begin(); this_it < _end;
           this_it++, N_it++)
        {
          **this_it = *N_it;
        }  
    }
  else
    {
      // Calculate overlap & initialize index
      for (i=0; i<Dimension; i++)
        {
          OverlapLow[i] =m_InnerBoundsLow[i] - m_Loop[i];
          OverlapHigh[i]=
            this->GetSize(i) - (m_Loop[i]-m_InnerBoundsHigh[i])-1;
          temp[i] = 0;
        }
      
      // Iterate through neighborhood
      for (N_it = N.Begin(), this_it = this->Begin();
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
              if ( (unsigned int)(temp[i]) == this->GetSize(i) ) temp[i]= 0;
              else break;
            }
        }
      
    }
}

template<class TImage, class TBoundaryCondition>
void SmartNeighborhoodIterator<TImage, TBoundaryCondition>
::PrintSelf(std::ostream &os, Indent indent) const
{
  os << indent << "SmartNeighborhoodIterator { this = " << this
     << "} " << std::endl;

  Superclass::PrintSelf(os, indent.GetNextIndent());
}

} // end namespace itk

#endif
