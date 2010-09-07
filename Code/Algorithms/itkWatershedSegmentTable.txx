/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTable.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
template< class TScalarType >
void SegmentTable< TScalarType >
::PruneEdgeLists(ScalarType maximum_saliency)
{
  Iterator it;

  typename edge_list_t::iterator e;
  for ( it = this->Begin(); it != this->End(); ++it )
    {
    for ( e = ( *it ).second.edge_list.begin();
          e != ( *it ).second.edge_list.end();
          e++ )
      {
      if ( ( e->height - ( *it ).second.min ) > maximum_saliency )
        {   // dump the rest of the list, assumes list is sorted
        e++;
        ( *it ).second.edge_list.erase( e, ( *it ).second.edge_list.end() );
        break;  // through with this segment
        }
      }
    }
}

template< class TScalarType >
void SegmentTable< TScalarType >
::SortEdgeLists()
{
  Iterator it;

  for ( it = this->Begin(); it != this->End(); ++it )
    {
    ( *it ).second.edge_list.sort();
    }
}

template< class TScalarType >
bool SegmentTable< TScalarType >
::Add(unsigned long a, const segment_t & t)
{
  std::pair< Iterator, bool > result;
  result = m_HashMap.insert( ValueType(a, t) );
  if ( result.second == false ) { return false; }
  else { return true; }
}
} // end namespace watershed
} // end namespace itk

#endif
