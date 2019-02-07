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
#ifndef itkBoxSpatialObject_hxx
#define itkBoxSpatialObject_hxx

#include "itkBoxSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
BoxSpatialObject< TDimension >
::BoxSpatialObject()
{
  this->SetTypeName("BoxSpatialObject");
  m_ObjectSize.Fill(1);
  m_ObjectPosition.Fill(0);
  m_Corners = nullptr;
}

/** Destructor */
template< unsigned int TDimension >
BoxSpatialObject< TDimension >
::~BoxSpatialObject() = default;

template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::SetSize(const SizeType & s) const
{
  this->m_ObjectSize = s;
  this->Modified();
}

template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::SetPosition(const PointType & p) const
{
  this->m_ObjectPosition = p;
  this->Modified();
}

template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  itkDebugMacro("Checking the point [" << point << "] is in the box");

  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->GetObjectBounds()->IsInside( point ) )
      {
      PointType transformedPoint = this->GetObjectToWorldTransform()
        ->GetInverse()->TransformPoint(point);

      bool isOutside = false;
      for ( unsigned int i = 0; i < TDimension; i++ )
        {
        if ( ( transformedPoint[i] - m_ObjectPosition[i] > m_ObjectSize[i] )
          || ( transformedPoint[i] - m_ObjectPosition[i] < 0 ) )
          {
          isOutside = true;
          break;
          }
        }
      if( !isOutside )
        {
        return true;
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren(point, depth-1, name);
    }

  return false;
}

/** Compute the bounds of the box */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing BoxSpatialObject bounding box");

  // Computes m_Corners from ObjectSize and ObjectPosition
  //  in world coordinates.
  this->Update();

  // refresh the bounding box with the transformed corners
  const_cast< BoundingBoxType * >( this->GetObjectBounds() )
    ->SetPoints(m_Corners);
  this->GetObjectBounds()->ComputeBoundingBox();

  return true;
}

/** Compute the bounds of the box */
template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::Update() const
{
  itkDebugMacro("BoxSpatialObject: Update");

  Superclass::Update();

  // First we compute the bounding box in the object space
  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

  PointType    pnt1;
  PointType    pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt1[i] = m_ObjectPosition[i];
    pnt2[i] = m_ObjectPosition[i] + m_ObjectSize[i];
    }

  bb->SetMinimum(pnt1);
  bb->SetMaximum(pnt1);
  bb->ConsiderPoint(pnt2);
  bb->ComputeBoundingBox();

  // Next Transform the corners of the bounding box
  const PointsContainer *corners = bb->GetCorners();
  m_Corners = PointsContainer::New();
  m_Corners->Reserve( static_cast<typename PointsContainer::ElementIdentifier>(
    corners->size() ) );

  auto it = corners->begin();
  auto itWorldCorner = m_Corners->begin();
  while ( it != corners->end() )
    {
    PointType pnt = this->GetObjectToWorldTransform()->TransformPoint(*it);
    *itWorldCorner = pnt;
    ++it;
    ++itTrans;
    }
}

template< unsigned int TDimension >
const PointType &
BoxSpatialObject< TDimension >
::GetCorner( unsigned int cornerNumber ) const
{
  if( m_Corners != nullptr )
    {
    if( cornerNumber < m_Corners.size() )
       {
       return m_Croners[ cornerNumber ];
       }
    }
}

/** Print Self function */
template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Size: " << m_ObjectSize << std::endl;
  os << "Position: " << m_ObjectPosition << std::endl;
  os << "Corners: " << m_Corners << std::endl;
}
} // end namespace itk

#endif
