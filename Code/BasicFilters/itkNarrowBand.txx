/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNarrowBand.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNarrowBand_txx
#define _itkNarrowBand_txx
#include "itkNarrowBand.h"
#include "math.h"

namespace itk
{

//template <class NodeType>
//void 
//NarrowBand<NodeType>
//::PrintSelf(std::ostream& os, Indent indent) const
//{
//  Superclass::PrintSelf(os, indent);
//}

template <class NodeType >
std::vector< NarrowBand<NodeType>::RegionType>
NarrowBand<NodeType>
::SplitBand( unsigned int n)
{
  unsigned int i;
  std::vector<RegionType> regionList;
  if (n > m_NodeContainer.size())
    {
    n = m_NodeContainer.size();
    }
  unsigned int regionsize = static_cast<unsigned int> (floor(static_cast<float>(m_NodeContainer.size())/static_cast<float>( n )));
  if (regionsize == 0)
    {
    regionsize = 1;
    }
  RegionType region;
  Iterator pos = this->Begin();
  
  for (i = 0; i < n; i++)
    {
    region.Begin = pos;
    pos += regionsize;
    
    if (i != n-1)
      {
      region.End = pos;
      }
    else
      {
      region.End = this->End();
      }
    
    regionList.push_back(region);
    }
  
  return regionList;
}


} // end namespace itk

#endif
