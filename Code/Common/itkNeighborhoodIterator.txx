/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodIterator_txx
#define _itkNeighborhoodIterator_txx
namespace itk {

template<class TImage, class TBoundaryCondition>
void
NeighborhoodIterator<TImage, TBoundaryCondition>
::SetPixel(const unsigned n, const PixelType& v)
{ 
  register unsigned int i;
  OffsetType OverlapLow, OverlapHigh, temp, offset;
  bool flag;

  if (this->m_NeedToUseBoundaryCondition == false)
    { *(this->operator[](n)) = v; }
  
  // Is this whole neighborhood in bounds?
  else if (this->InBounds()) *(this->operator[](n)) = v;
  else
    {
    temp = this->ComputeInternalIndex(n);
      
    // Calculate overlap
    for (i=0; i<Dimension; i++)
      {
      OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
      OverlapHigh[i]=
        static_cast<OffsetValueType>(this->GetSize(i)
                                     - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i]) );
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
      *(this->operator[](n)) = v;
      }
    else
      { // Attempt to write out of bounds
      throw RangeError(__FILE__, __LINE__);
      };
    }
}

template<class TImage, class TBoundaryCondition>
void
NeighborhoodIterator<TImage, TBoundaryCondition>
::SetPixel(const unsigned n, const PixelType& v, bool &status)
{
  register unsigned int i;
  OffsetType temp;

  typename OffsetType::OffsetValueType OverlapLow, OverlapHigh;

  if (this->m_NeedToUseBoundaryCondition == false)
    {
    status = true;
    *(this->operator[](n)) = v;
    }
  
  // Is this whole neighborhood in bounds?
  else if (this->InBounds())
    {
    *(this->operator[](n)) = v;
    status = true;
    return;
    }
  else
    {
    temp = this->ComputeInternalIndex(n);
      
    // Calculate overlap
    for (i=0; i<Dimension; i++)
      {
      if (! m_InBounds[i]) // Part of dimension spills out of bounds
        {
        OverlapLow = m_InnerBoundsLow[i] - m_Loop[i];
        OverlapHigh=
          static_cast<OffsetValueType>(this->GetSize(i)
                                       - ( (m_Loop[i]+2) - m_InnerBoundsHigh[i]) );
        if (temp[i] < OverlapLow || OverlapHigh < temp[i])
          {
          status = false;
          return;
          }
        }
      }
      
    *(this->operator[](n)) = v ;
    status = true;
    }
}

  
template<class TImage, class TBoundaryCondition>
void
NeighborhoodIterator<TImage, TBoundaryCondition>
::PrintSelf(std::ostream &os, Indent indent) const
{
  os << indent;
  os << "NeighborhoodIterator {this= " << this << "}" << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());
}

template<class TImage, class TBoundaryCondition>
void
NeighborhoodIterator<TImage, TBoundaryCondition>
::SetNeighborhood(const NeighborhoodType &N)
{
  register unsigned int i;
  OffsetType OverlapLow, OverlapHigh, temp;
  bool flag;
      
  const Iterator _end = this->End();
  Iterator this_it;
  typename  NeighborhoodType::ConstIterator N_it;

  if (this->m_NeedToUseBoundaryCondition == false)
    {
    for (N_it = N.Begin(), this_it = Begin(); this_it < _end;
         this_it++, N_it++)
      {
      **this_it = *N_it;
      }
    }
  else if (InBounds())
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
        static_cast<OffsetValueType>(this->GetSize(i) - (m_Loop[i]-m_InnerBoundsHigh[i])-1);
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

} // namespace itk

#endif
