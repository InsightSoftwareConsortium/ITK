/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  ModUle:    itkSparseFieldLayer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSparseFieldLayer_txx
#define __itkSparseFieldLayer_txx
#include "itkSparseFieldLayer.h"

#include <math.h>
namespace itk {

template<class TNodeType>
SparseFieldLayer<TNodeType>
::SparseFieldLayer()
{
  m_HeadNode = new NodeType;
  m_HeadNode->Next = m_HeadNode;
  m_HeadNode->Previous = m_HeadNode;
}

template<class TNodeType>
SparseFieldLayer<TNodeType>
::~SparseFieldLayer()
{
  delete m_HeadNode;
}
  
template<class TNodeType>
void
SparseFieldLayer<TNodeType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "m_HeadNode:  " << m_HeadNode << std::endl;
  os << indent << "Empty? : " << this->Empty() << std::endl;
}

template<class TNodeType>
unsigned int
SparseFieldLayer<TNodeType>
::Size() const
{
  unsigned int counter = 0;

  for (NodeType *it = m_HeadNode; it->Next != m_HeadNode; it = it->Next)
    { counter++; }
  return counter;
}

template<class TNodeType>
typename SparseFieldLayer<TNodeType>::RegionListType
SparseFieldLayer<TNodeType>
::SplitRegions (int num) const
{
  std::vector<RegionType> regionlist;
  unsigned int size, regionsize;
  size=Size();
  regionsize=static_cast<unsigned int>(ceil(static_cast<float>(size)/static_cast<float>(num)));
  ConstIterator position=Begin();
  ConstIterator last=End();
  
  for (int i=0;i<num;i++) 
    {
      unsigned int j=0;
      RegionType region;
      region.first=position;
      while ((j<regionsize)&&(position!=last)) 
        {
          j++;
          ++position;
        }
      region.last=position;
      regionlist.push_back(region);
   }
  
  return regionlist;
}

}// end namespace itk

#endif
