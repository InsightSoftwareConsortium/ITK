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
#ifndef itkPolygonGroupSpatialObject_hxx
#define itkPolygonGroupSpatialObject_hxx

#include "itkPolygonGroupSpatialObject.h"

namespace itk
{
template< unsigned int TDimension >
PolygonGroupSpatialObject< TDimension >::
PolygonSpatialObject()
{
  this->SetTypeName( "PolygonGroupSpatialObject" );
}

template< unsigned int TDimension >
bool PolygonGroupSpatialObject< TDimension >::AddStrand(
  PolygonSpatialObject< TDimension > *toAdd )
{
  this->AddChild(toAdd);
  return true;
}

template< unsigned int TDimension >
bool PolygonGroupSpatialObject< TDimension >::DeleteStrand(
  PolygonSpatialObject< TDimension > *toDelete )
{
  this->RemoveChild(toDelete);
  return true;
}

template< unsigned int TDimension >
bool PolygonGroupSpatialObject< TDimension >::ReplaceStrand(
  PolygonSpatialObject< TDimension > *toReplace,
  PolygonSpatialObject< TDimension > *replacement)
{
  auto it = m_ChildrenList.begin();
  auto itend = m_ChildrenList.end();
  while ( it != itend )
    {
    if ( static_cast< void * >( ( *it ) ) ==
      static_cast< void * >( toReplace ) )
      {
      auto after = it;
      after++;
      children.insert( after, 1, replacement );
      it.SetParent( nullptr );
      children.erase(it);
      return true;
      }
    it++;
    }
  return false;
}

template< unsigned int TDimension >
bool PolygonGroupSpatialObject< TDimension >::IsClosed()
{
  auto it = m_ChildrenList.begin();
  auto itend = m_ChildrenList.end();
  while ( it != itend )
    {
    auto * curstrand = dynamic_cast< PolygonSpatialObject< TDimension > * >(
      ( *it ).GetPointer() );
    if ( curstrand != nullptr )
      {
      if ( !curstrand->IsClosed() )
        {
        return false;
        }
      }
    it++;
    }
  return true;
}

template< unsigned int TDimension >
unsigned PolygonGroupSpatialObject< TDimension >::NumberOfStrands()
{
  return this->GetNumberOfChildren();
}

template< unsigned int TDimension >
double PolygonGroupSpatialObject< TDimension >::Volume()
{
  double            volume = 0;

  auto it = m_ChildrenList->begin();
  auto itend = m_ChildrenList->end();
  while ( it != itend )
    {
    auto * curstrand = dynamic_cast< PolygonSpatialObject< TDimension > * >(
      ( *it ).GetPointer() );
    volume += curstrand->MeasureVolume();
    it++;
    }

  return volume;
}

template< unsigned int TDimension >
double PolygonGroupSpatialObject< TDimension >::MeasureVolume()
{
  return this->Volume();
}

}
#endif
