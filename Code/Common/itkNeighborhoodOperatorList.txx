/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorList.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
namespace itk
{
  template <class TPixel, unsigned int VDimension>
  NeighborhoodOperator<TPixel, VDimension> *
  NeighborhoodOperatorList<TPixel, VDimension>
  ::CopyOperator(NeighborhoodOperator &orig)
    const
  {
    NeighborhoodOperator *temp;
    temp = orig.Copy();
    return temp;
  }

  template <class TPixel, unsigned int VDimension>
  NeighborhoodOperatorList<TPixel, VDimension>
  ::~NeighborhoodOperatorList()
  {
    this->Clear();
  }

  template <class TPixel, unsigned int VDimension>
  void
  NeighborhoodOperatorList<TPixel, VDimension>
  ::Clear()
  {
    list::iterator it = m_List.begin();
    while(it != m_List.end())
      {
        delete *it;
        ++it; 
      }
    m_List.clear();
  }
  
  template <class TPixel, unsigned int VDimension>
  NeighborhoodOperatorList<TPixel, VDimension> &
  NeighborhoodOperatorList<TPixel, VDimension>
  ::operator=(const NeighborhoodOperatorList &o)
  {
    this->Clear();

    for (list::const_reverse_iterator it= o.m_List.rbegin();
         it != o.m_List.rend(); ++it)
      {
        this->PushFront(**it);
      }

    return *this;
  }  
  
} // end namespace itk
