/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkWatershedSegmentTable_hxx
#define itkWatershedSegmentTable_hxx

#include "itkWatershedSegmentTable.h"

namespace itk
{
namespace watershed
{
template< typename TScalar >
void SegmentTable< TScalar >
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

template< typename TScalar >
void SegmentTable< TScalar >
::SortEdgeLists()
{
  Iterator it;

  for ( it = this->Begin(); it != this->End(); ++it )
    {
    ( *it ).second.edge_list.sort();
    }
}

template< typename TScalar >
bool SegmentTable< TScalar >
::Add(IdentifierType a, const segment_t & t)
{
  std::pair< Iterator, bool > result;
  result = m_HashMap.insert( ValueType(a, t) );
  if ( result.second == false ) { return false; }
  else { return true; }
}
} // end namespace watershed
} // end namespace itk

#endif
