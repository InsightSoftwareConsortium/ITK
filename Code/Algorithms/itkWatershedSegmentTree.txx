/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTree.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedSegmentTree_txx
#define __itkWatershedSegmentTree_txx

namespace itk
{
namespace watershed
{

//template <class TScalarType>
//void  SegmentTree<TScalarType>
//::PrintDeque()
//{
//  std::cout << "Deque has " << this->Size() << " entries. " << std::endl;
//  Iterator it = this->Begin();
//  while ( it != this->End() )
//    {
//      std::cout << (*it).from << " -> "
//                << (*it).to << " " << (*it).saliency
//                << std::endl;
//      ++it;
//    }
//}

template <class TScalarType>
void
SegmentTree<TScalarType>
::UpdateOutputInformation()
{
  if (this->GetSource())
    {
    this->GetSource()->UpdateOutputInformation();
    }
  else
    {
      
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( ! m_RequestedRegionInitialized)
    {
    this->SetRequestedRegionToLargestPossibleRegion();
    m_RequestedRegionInitialized = true;
    }
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}

template <class TScalarType>
void 
SegmentTree<TScalarType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
  
}// end namespace watershed
}// end namespace itk

#endif
