/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstShapedNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConstShapedNeighborhoodIterator_txx
#define _itkConstShapedNeighborhoodIterator_txx
#include "itkConstShapedNeighborhoodIterator.h"
namespace itk {

  
template<class TImage, class TBoundaryCondition>
void
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
::PrintSelf(std::ostream &os, Indent indent) const
{
  os << indent <<  "ConstShapedNeighborhoodIterator {this = " << this;
  os << " m_ActiveIndexList = [";
  for (IndexListType::const_iterator it = m_ActiveIndexList.begin();
       it != m_ActiveIndexList.end(); ++it)
    { os << *it << " "; }
  os << "] ";
  os << " m_CenterIsActive = " << m_CenterIsActive;
  os << "}" << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());
}

template<class TImage, class TBoundaryCondition>
void
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
::ActivateIndex(const unsigned int n)
{
  const OffsetValueType *OffsetTable = m_ConstImage->GetOffsetTable();

  // Insert so that the list remains ordered.
  IndexListType::iterator it   = m_ActiveIndexList.begin();

  if (m_ActiveIndexList.empty()) { m_ActiveIndexList.push_front(n); }
  else
    {
      while (n > *it)
        {
          it++;
          if (it == m_ActiveIndexList.end()) { break; }
        }
      if (it == m_ActiveIndexList.end())
        { m_ActiveIndexList.insert(it, n); }
      else if (n != *it)
        { m_ActiveIndexList.insert(it, n); }
    }
  
  // Adjust the begin and end iterators.
  m_ConstBeginIterator.GoToBegin();
  m_ConstEndIterator.GoToEnd();

  // Did we just activate the index at the center of the neighborhood?
  if ( n == this->GetCenterNeighborhoodIndex() )
    { m_CenterIsActive = true; }

  // Set the pointer in the neighborhood location just activated.
  this->operator[](n) = this->GetCenterPointer();
  for (unsigned i = 0; i < Dimension; ++i)
    { this->operator[](n) += OffsetTable[i] * this->GetOffset(n)[i];  }
}

template<class TImage, class TBoundaryCondition>
void
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
::DeactivateIndex(const unsigned int n)
{
  IndexListType::iterator it = m_ActiveIndexList.begin();

  if (m_ActiveIndexList.empty()) return;
  else
    {
      while (n != *it) 
        {
          it++;
          if (it == m_ActiveIndexList.end()) return;
        }
      m_ActiveIndexList.erase(it);
    }

  // Adjust the begin and end iterators.
  m_ConstBeginIterator.GoToBegin();
  m_ConstEndIterator.GoToEnd();

  // Did we just deactivate the index at the center of the neighborhood?
  if ( n == this->GetCenterNeighborhoodIndex() )
    { m_CenterIsActive = false; }
}


template<class TImage, class TBoundaryCondition>
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
::operator++()
{
  unsigned int i;
  IndexListType::const_iterator it;

  // Center pointer must be updated whether or not it is active.
  if (! m_CenterIsActive) { this->operator[](this->GetCenterNeighborhoodIndex())++; }
  
  // Increment pointers for only the active pixels.
  for (it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); it++)
    { (this->operator[](*it))++; }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      m_Loop[i]++;
      if ( m_Loop[i] == m_Bound[i] )
        {
          m_Loop[i] = m_BeginIndex[i];
          if (! m_CenterIsActive)
            { this->operator[](this->GetCenterNeighborhoodIndex())+= m_WrapOffset[i]; }
          for (it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); it++)
            { (this->operator[](*it))+= m_WrapOffset[i]; }
        }        
      else break;
    }
  return *this;
}

template<class TImage, class TBoundaryCondition>
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
::operator--()
{
  unsigned int i;
 IndexListType::const_iterator it;

 // Center pointer must be updated whether or not it is active.
  if (! m_CenterIsActive) { this->operator[](this->GetCenterNeighborhoodIndex())--; }

  // Decrement pointers for only the active pixels.
  for (it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); it++)
    { (this->operator[](*it))--; }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      if (m_Loop[i] == m_BeginIndex[i])
        {
          m_Loop[i]= m_Bound[i] - 1;
          if (! m_CenterIsActive)
            { this->operator[](this->GetCenterNeighborhoodIndex())-= m_WrapOffset[i]; }
          for (it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); it++)
            { (this->operator[](*it))-= m_WrapOffset[i]; }
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
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition>
::operator+=(const OffsetType & idx)
{
  unsigned int i;
  IndexListType::const_iterator it;
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

  // Center pointer is always updated even if not active.
  if (! m_CenterIsActive)
    { this->operator[](this->GetCenterNeighborhoodIndex())+= accumulator; }  

  // Increment pointers only for those active pixels
  for (it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); it++)
    { (this->operator[](*it))+= accumulator ; }

  // Update loop counter values
  m_Loop += idx;

  return *this;
}

template<class TImage, class TBoundaryCondition>
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition> &
ConstShapedNeighborhoodIterator<TImage, TBoundaryCondition> 
::operator-=(const OffsetType & idx)
{
  unsigned int i;
  IndexListType::const_iterator it;
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

  // Center pointer is always updated even if not active.
  if (! m_CenterIsActive)
    { this->operator[](this->GetCenterNeighborhoodIndex()) -= accumulator; }  

  // Increment pointers only for those active pixels
  for (it = m_ActiveIndexList.begin(); it != m_ActiveIndexList.end(); it++)
    { (this->operator[](*it)) -= accumulator ; }

  // Update loop counter values
  m_Loop -= idx;

  return *this;
}

} // namespace itk

#endif
 
