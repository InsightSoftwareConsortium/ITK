/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTable.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedSegmentTable_txx
#define __itkWatershedSegmentTable_txx

namespace itk
{
namespace watershed
{

template <class TScalarType>
void SegmentTable<TScalarType>
::PruneEdgeLists(ScalarType maximum_saliency)
{
  Iterator it;
  typename edge_list_t::iterator e;
  for (it = this->Begin(); it != this->End(); ++it)
    {
    for (e = (*it).second.edge_list.begin();
         e != (*it).second.edge_list.end();
         e++)
      {
      if ( ( e->height - (*it).second.min ) > maximum_saliency )
        {  // dump the rest of the list, assumes list is sorted
        e++;
        (*it).second.edge_list.erase(e, (*it).second.edge_list.end() );
        break;  // through with this segment
        }
      }
    }
}
  
template <class TScalarType>
void SegmentTable<TScalarType>
::SortEdgeLists()
{
  Iterator it;
  for (it = this->Begin(); it != this->End(); ++it)
    {
    (*it).second.edge_list.sort();
    }
}
  
template <class TScalarType>
bool SegmentTable<TScalarType>
::Add(unsigned long a, const segment_t &t)
{
  std::pair<Iterator, bool> result;
  result = m_HashMap.insert( ValueType(a, t) );
  if (result.second == false) return false;
  else return true;
}

//template <class TScalarType>
//void SegmentTable<TScalarType>
//::PrintHashTable() const
//{
//  std::cout << "This has table has " << m_HashMap.size() << " entries." <<
//    std::endl;
//  std::cout << "Maximum depth is " << m_MaximumDepth << std::endl;
//  unsigned long i = 0;
// 
//  edge_list_t::const_iterator e;
//  ConstIterator it = this->Begin();
//  while (it != this->End() )
//    {
//          std::cout << "KEY: " << (*it).first << " = " << std::endl;
//          std::cout << "         min " << (*it).second.min << std::endl;
//         std::cout << "  edge_list = { ";
//          e = (*it).second.edge_list.begin();
//          while (e != (*it).second.edge_list.end() )
//            {
//              std::cout << "(" << (*e).label << ", " << (*e).height << ") ";
//              e++;
//            }
//          std::cout << std::endl;
//      it++;
//      i++;
//    }
//  std::cout << "Verified " << i << " entries" << std::endl;
//}
  
template <class TScalarType>
void
SegmentTable<TScalarType>
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
SegmentTable<TScalarType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
  
}// end namespace watershed
}// end namespace itk

#endif
