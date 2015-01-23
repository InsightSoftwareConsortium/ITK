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
#ifndef itkMeshSpatialObject_hxx
#define itkMeshSpatialObject_hxx

#include "itkMeshSpatialObject.h"
#include "itkSize.h"

namespace itk
{
/** Constructor */
template< typename TMesh >
MeshSpatialObject< TMesh >
::MeshSpatialObject()
{
  this->SetTypeName("MeshSpatialObject");
  m_Mesh = MeshType::New();
  this->ComputeBoundingBox();
  m_PixelType = typeid( typename TMesh::PixelType ).name();
  m_IsInsidePrecision = 1;
}

/** Destructor */
template< typename TMesh >
MeshSpatialObject< TMesh >
::~MeshSpatialObject()
{}

/** Return true if the given point is inside the Mesh */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  return IsInside(point, depth, name);
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::IsInside(const PointType & point) const
{
  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  if ( this->GetBounds()->IsInside(transformedPoint) )
    {
    typename MeshType::CellsContainerPointer cells =  m_Mesh->GetCells();
    typename MeshType::CellsContainer::ConstIterator it = cells->Begin();
    while ( it != cells->End() )
      {
      typedef typename MeshType::CoordRepType CoordRepType;
      CoordRepType position[Dimension];
      for ( unsigned int i = 0; i < Dimension; i++ )
        {
        position[i] = transformedPoint[i];
        }

      // If this is a triangle cell we need to check the distance
      if ( it.Value()->GetNumberOfPoints() == 3 )
        {
        double minDist = 0.0;
        const bool pointIsInside = it.Value()->EvaluatePosition(
          position, m_Mesh->GetPoints(), ITK_NULLPTR, ITK_NULLPTR, &minDist, ITK_NULLPTR);

        if ( pointIsInside  && minDist <= this->m_IsInsidePrecision )
          {
          return true;
          }
        }
      else
        {
        if ( it.Value()->EvaluatePosition(position, m_Mesh->GetPoints(),
                                          ITK_NULLPTR, ITK_NULLPTR, ITK_NULLPTR, ITK_NULLPTR) )
          {
          return true;
          }
        }
      ++it;
      }
    }
  return false;
}

/** Return true if the given point is inside the Mesh */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
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

/** Return the value of the Mesh at a specified point
 *  The value returned is always of type double */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  if ( IsEvaluableAt(point, 0, name) )
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

/** Compute the bounds of the object which is the same as the internal mesh */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::ComputeLocalBoundingBox() const
{
  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    PointType pnt;
    PointType pnt2;

    for ( unsigned int i = 0; i < itkGetStaticConstMacro(Dimension); i++ )
      {
      pnt[i] = m_Mesh->GetBoundingBox()->GetBounds()[2 * i];
      pnt2[i] = m_Mesh->GetBoundingBox()->GetBounds()[2 * i + 1];
      }

    pnt = this->GetIndexToWorldTransform()->TransformPoint(pnt);
    pnt2 = this->GetIndexToWorldTransform()->TransformPoint(pnt2);

    const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(pnt);
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(pnt2);
    }
  return true;
}

/** Set the Mesh in the spatial object */
template< typename TMesh >
void
MeshSpatialObject< TMesh >
::SetMesh(MeshType *mesh)
{
  m_Mesh = mesh;
  m_Mesh->Modified();
  this->ComputeBoundingBox();
}

/** Get the Mesh inside the spatial object */
template< typename TMesh >
typename MeshSpatialObject< TMesh >::MeshType *
MeshSpatialObject< TMesh >
::GetMesh(void)
{
  return m_Mesh.GetPointer();
}

template< typename TMesh >
const typename MeshSpatialObject< TMesh >::MeshType *
MeshSpatialObject< TMesh >
::GetMesh(void) const
{
  return m_Mesh.GetPointer();
}

/** Print the object */
template< typename TMesh >
void
MeshSpatialObject< TMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Mesh: " << std::endl;
  os << "m_IsInsidePrecision: " << m_IsInsidePrecision << std::endl;
  os << indent << m_Mesh << std::endl;
}

/** Get the modification time */
template< typename TMesh >
ModifiedTimeType
MeshSpatialObject< TMesh >
::GetMTime(void) const
{
  ModifiedTimeType latestMTime = Superclass::GetMTime();
  const ModifiedTimeType MeshMTime = m_Mesh->GetMTime();

  if ( MeshMTime > latestMTime )
    {
    latestMTime = MeshMTime;
    }

  return latestMTime;
}
} // end namespace itk

#endif //__MeshSpatialObject_hxx
