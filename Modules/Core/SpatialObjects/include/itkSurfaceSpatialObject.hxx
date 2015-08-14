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
#ifndef itkSurfaceSpatialObject_hxx
#define itkSurfaceSpatialObject_hxx


#include "itkMath.h"
#include "itkSurfaceSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
SurfaceSpatialObject< TDimension >
::SurfaceSpatialObject()
{
  this->SetDimension(TDimension);
  this->SetTypeName("SurfaceSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);
  this->ComputeBoundingBox();
}

/** Destructor */
template< unsigned int TDimension >
SurfaceSpatialObject< TDimension >
::~SurfaceSpatialObject()
{}

/** Get the list of points composing the surface */
template< unsigned int TDimension >
typename SurfaceSpatialObject< TDimension >::PointListType &
SurfaceSpatialObject< TDimension >
::GetPoints()
{
  itkDebugMacro("Getting SurfacePoint list");
  return m_Points;
}
template< unsigned int TDimension >
const typename SurfaceSpatialObject< TDimension >::PointListType &
SurfaceSpatialObject< TDimension >
::GetPoints() const
{
  itkDebugMacro("Getting SurfacePoint list");
  return m_Points;
}

/** Set the list of points composing the surface */
template< unsigned int TDimension >
void
SurfaceSpatialObject< TDimension >
::SetPoints(PointListType & points)
{
  // in this function, passing a null pointer as argument will
  // just clear the list...
  m_Points.clear();

  typename PointListType::iterator it, end;
  it = points.begin();
  end = points.end();
  while ( it != end )
    {
    m_Points.push_back(*it);
    it++;
    }

  this->ComputeBoundingBox();
  this->Modified();
}

/** Print the surface object */
template< unsigned int TDimension >
void
SurfaceSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "SurfaceSpatialObject(" << this << ")" << std::endl;
  os << indent << "ID: " << this->GetId() << std::endl;
  os << indent << "nb of points: "
     << static_cast< SizeValueType >( m_Points.size() ) << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Compute the bounds of the surface */
template< unsigned int TDimension >
bool
SurfaceSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing surface bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    typename PointListType::const_iterator it  = m_Points.begin();
    typename PointListType::const_iterator end = m_Points.end();

    if ( it == end )
      {
      return false;
      }
    else
      {
      PointType pt =
        this->GetIndexToWorldTransform()->TransformPoint( ( *it ).GetPosition() );
      const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(pt);
      const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(pt);
      it++;
      while ( it != end )
        {
        pt = this->GetIndexToWorldTransform()->TransformPoint(
          ( *it ).GetPosition() );
        const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(pt);
        it++;
        }
      }
    }
  return true;
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
SurfaceSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  typename PointListType::const_iterator it = m_Points.begin();
  typename PointListType::const_iterator itEnd = m_Points.end();

  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  if ( this->GetBounds()->IsInside(transformedPoint) )
    {
    while ( it != itEnd )
      {
      if ( ( *it ).GetPosition() == transformedPoint )
        {
        return true;
        }
      it++;
      }
    }
  return false;
}

/** Return true is the given point is on the surface */
template< unsigned int TDimension >
bool
SurfaceSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point [" << point << "is on the surface");

  if ( name == ITK_NULLPTR )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }
  else if ( strstr(typeid( Self ).name(), name) )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
}

/** Return true if the surface is evaluable at a specified point */
template< unsigned int TDimension >
bool
SurfaceSpatialObject< TDimension >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the surface is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Return 1 if the point is on the surface */
template< unsigned int TDimension >
bool
SurfaceSpatialObject< TDimension >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  itkDebugMacro("Getting the value of the surface at " << point);
  if ( IsInside(point, 0, name) )
    {
    value = this->GetDefaultInsideValue();
    return true;
    }
  else if ( Superclass::IsEvaluableAt(point, depth, name) )
    {
    Superclass::ValueAt(point, value, depth, name);
    return true;
    }
  value = this->GetDefaultOutsideValue();
  return false;
}

/** Approximate the normals of the surface */
template< unsigned int TDimension >
bool
SurfaceSpatialObject< TDimension >
::Approximate3DNormals()
{
  if ( TDimension != 3 )
    {
    itkExceptionMacro("Approximate3DNormals works only in 3D");
    }

  if ( m_Points.size() < 3 )
    {
    itkExceptionMacro("Approximate3DNormals requires at least 3 points");
    }

  typename PointListType::iterator it = m_Points.begin();
  typename PointListType::iterator itEnd = m_Points.end();

  while ( it != itEnd )
    {
    // Try to find 3 points close to the corresponding point
    SurfacePointType pt = *it;
    PointType        pos = ( *it ).GetPosition();

    std::list< int > badId;
    unsigned int     identifier[3];
    double           absvec = 0;
    do
      {
      identifier[0] = 0;
      identifier[1] = 0;
      identifier[2] = 0;

      float max[3];
      max[0] = 99999999;
      max[1] = 99999999;
      max[2] = 99999999;

      typename PointListType::const_iterator it2 = m_Points.begin();

      int i = 0;
      while ( it2 != m_Points.end() )
        {
        if ( it2 == it )
          {
          i++;
          it2++;
          continue;
          }

        bool                             badPoint = false;
        std::list< int >::const_iterator itBadId = badId.begin();
        while ( itBadId != badId.end() )
          {
          if ( *itBadId == i )
            {
            badPoint = true;
            break;
            }
          itBadId++;
          }

        if ( badPoint )
          {
          i++;
          it2++;
          continue;
          }

        PointType pos2 = ( *it2 ).GetPosition();
        float     distance = ( pos2[0] - pos[0] ) * ( pos2[0] - pos[0] ) + ( pos2[1] - pos[1] )
                             * ( pos2[1] - pos[1] ) + ( pos2[2] - pos[2] ) * ( pos2[2] - pos[2] );

        // Check that the point is not the same as some previously defined
        bool valid = true;
        for ( unsigned int j = 0; j < 3; j++ )
          {
          PointType p = m_Points[identifier[j]].GetPosition();
          float     d = ( pos2[0] - p[0] ) * ( pos2[0] - p[0] ) + ( pos2[1] - p[1] )
                        * ( pos2[1] - p[1] ) + ( pos2[2] - p[2] ) * ( pos2[2] - p[2] );
          if ( Math::AlmostEquals( d, 0.0f ) )
            {
            valid = false;
            break;
            }
          }

        if ( Math::AlmostEquals( distance, 0.0f ) || !valid )
          {
          i++;
          it2++;
          continue;
          }

        if ( distance < max[0] )
          {
          max[2] = max[1];
          max[1] = max[0];
          max[0] = distance;
          identifier[0] = i;
          }
        else if ( distance < max[1] )
          {
          max[2] = max[1];
          max[1] = distance;
          identifier[1] = i;
          }
        else if ( distance < max[2] )
          {
          max[2] = distance;
          identifier[2] = i;
          }
        i++;
        it2++;
        }

      if ( ( identifier[0] == identifier[1] )
           || ( identifier[1] == identifier[2] )
           || ( identifier[0] == identifier[2] )
            )
        {
        std::cout << "Cannot find 3 distinct points!" << std::endl;
        std::cout << identifier[0] << " : " << identifier[1] << " : " << identifier[2] << std::endl;
        std::cout << max[0] << " : " << max[1] << " : " << max[2] << std::endl;
        return false;
        }

      PointType v1 = m_Points[identifier[0]].GetPosition();
      PointType v2 = m_Points[identifier[1]].GetPosition();
      PointType v3 = m_Points[identifier[2]].GetPosition();

      double coa = -( v1[1] * ( v2[2] - v3[2] )
                      + v2[1] * ( v3[2] - v1[2] )
                      + v3[1] * ( v1[2] - v2[2] ) );
      double cob = -( v1[2] * ( v2[0] - v3[0] )
                      + v2[2] * ( v3[0] - v1[0] )
                      + v3[2] * ( v1[0] - v2[0] ) );
      double coc = -( v1[0] * ( v2[1] - v3[1] )
                      + v2[0] * ( v3[1] - v1[1] )
                      + v3[0] * ( v1[1] - v2[1] ) );

      absvec = -std::sqrt ( (double)( ( coa * coa ) + ( cob * cob ) + ( coc * coc ) ) );

      if ( Math::AlmostEquals( absvec, 0.0 ) )
        {
        badId.push_back(identifier[2]);
        }
      else
        {
        CovariantVectorType normal;
        normal[0] = coa / absvec;
        normal[1] = cob / absvec;
        normal[2] = coc / absvec;
        ( *it ).SetNormal(normal);
        }
      }
    while ( ( Math::AlmostEquals( absvec, 0.0 ) ) && ( badId.size() < m_Points.size() - 1 ) );

    if ( Math::AlmostEquals( absvec, 0.0 ) )
      {
      std::cout << "Approximate3DNormals Failed!" << std::endl;
      std::cout << identifier[0] << " : " << identifier[1] << " : " << identifier[2] << std::endl;
      std::cout << badId.size() << " : " << m_Points.size() - 1 << std::endl;
      return false;
      }

    it++;
    }

  return true;
}
} // end namespace itk

#endif
