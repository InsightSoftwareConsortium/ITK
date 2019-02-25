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
#ifndef itkEllipseSpatialObject_hxx
#define itkEllipseSpatialObject_hxx

#include "itkEllipseSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::EllipseSpatialObject()
{
  this->SetTypeName("EllipseSpatialObject");
  m_RadiusInObjectSpace.Fill(1.0);
  m_CenterInObjectSpace.Fill(1.0);

  this->Update();
}

/** Destructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::~EllipseSpatialObject() = default;

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::IsInsideInObjectSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    double r = 0;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      if ( m_RadiusInObjectSpace[i] != 0.0 )
        {
        r += ( point[i] * point[i] )
             / ( m_RadiusInObjectSpace[i] * m_RadiusInObjectSpace[i] );
        }
      else if ( point[i] > 0.0 )  // Degenerate ellipse
        {
        r = 2; // Keeps function from returning true here
        break;
        }
      }

    if ( r < 1 )
      {
      return true;
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildrenInObjectSpace( point, depth-1, name );
    }

  return false;
}

/** Compute the bounds of the ellipse */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::ComputeMyBoundingBox() const
{
  itkDebugMacro("Computing ellipse bounding box");

  PointType    pnt1;
  PointType    pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt1[i] = m_CenterInObjectSpace[i] - m_RadiusInObjectSpace[i];
    pnt2[i] = m_CenterInObjectSpace[i] + m_RadiusInObjectSpace[i];
    }

  this->GetMyBoundingBoxInObjectSpace()->SetMinimum(pnt1);
  this->GetMyBoundingBoxInObjectSpace()->SetMaximum(pnt1);
  this->GetMyBoundingBoxInObjectSpace()->ConsiderPoint(pnt2);
  this->GetMyBoundingBoxInObjectSpace()->ComputeBoundingBox();

  return true;
}

/** Update world coordinate representation */
template < unsigned int TDimension >
void
EllipseSpatialObject< TDimension >
::Update()
{
  PointType center = this->GetCenterInObjectSpace();
  PointType worldCenter;

  Superclass::Update();

  worldCenter = this->GetObjectToWorldTransform()->TransformPoint( center );
  m_CenterInWorldSpace = worldCenter;
}

/** Print Self function */
template< unsigned int TDimension >
void
EllipseSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "EllipseSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
  os << "Object Radius: " << m_RadiusInObjectSpace << std::endl;
  os << "Object Center: " << m_CenterInObjectSpace << std::endl;
  os << "World Center: " << m_CenterInWorldSpace << std::endl;
}

} // end namespace itk

#endif
