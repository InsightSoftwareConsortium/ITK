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
  m_IsInsideInWorldSpacePrecision = 1;
}

/** Destructor */
template< typename TMesh >
MeshSpatialObject< TMesh >
::~MeshSpatialObject() = default;

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::IsInsideInWorldSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->GetMyBoundingBoxInWorldSpace().IsInsideInWorldSpace( point ) )
      {
      PointType transformedPoint = this->GetObjectToWorldTransform()->
        GetInverseTransform()->TransformPoint(point);

      typename MeshType::CellsContainerPointer cells =  m_Mesh->GetCells();
      typename MeshType::CellsContainer::ConstIterator it = cells->Begin();
      while ( it != cells->End() )
        {
        using CoordRepType = typename MeshType::CoordRepType;
        CoordRepType position[Dimension];
        for ( unsigned int i = 0; i < Dimension; i++ )
          {
          position[i] = transformedPoint[i];
          }

        // If this is a triangle cell we need to check the distance
        if ( it.Value()->GetNumberOfPoints() == 3 )
          {
          double minDist = 0.0;
          const bool pointIsInsideInWorldSpace = it.Value()->EvaluatePositionInWorldSpace(
            position, m_Mesh->GetPoints(), nullptr, nullptr, &minDist, nullptr);

          if ( pointIsInsideInWorldSpace  && minDist <= this->m_IsInsideInWorldSpacePrecision )
            {
            return true;
            }
          }
        else
          {
          if ( it.Value()->EvaluatePositionInWorldSpace(position, m_Mesh->GetPoints(),
                                            nullptr, nullptr, nullptr, nullptr) )
            {
            return true;
            }
          }
        ++it;
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildrenInWorldSpace( point, depth-1, name );
    }

  return false;
}

/** Compute the bounds of the object which is the same as the internal mesh */
template< typename TMesh >
bool
MeshSpatialObject< TMesh >
::ComputeMyBoundingBoxInWorldSpace() const
{
  PointType pnt1;
  PointType pnt2;
  for ( unsigned int i = 0; i < ObjectDimension; i++ )
    {
    pnt1[i] = m_Mesh->GetBoundingBox()->GetBoundingBox()[2 * i];
    pnt2[i] = m_Mesh->GetBoundingBox()->GetBoundingBox()[2 * i + 1];
    }

  pnt1 = this->GetObjectToWorldTransform()->TransformPoint(pnt1);
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint(pnt2);

  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();
  bb->SetMinimum(pnt1);
  bb->SetMaximum(pnt1);
  bb->ConsiderPoint(pnt2);
  bb->ComputeBoundingBox();

  // Next Transform the corners of the bounding box
  using PointsContainer = typename BoundingBoxType::PointsContainer;
  const PointsContainer *corners = bb->GetCorners();
  typename PointsContainer::Pointer transformedCorners =
    PointsContainer::New();
  transformedCorners->Reserve(
    static_cast<typename PointsContainer::ElementIdentifier>(
      corners->size() ) );

  auto it = corners->begin();
  auto itTrans = transformedCorners->begin();
  while ( it != corners->end() )
    {
    PointType pnt = this->GetObjectToWorldTransform()->TransformPoint(*it);
    *itTrans = pnt;
    ++it;
    ++itTrans;
    }

  // refresh the bounding box with the transformed corners
  const_cast< BoundingBoxType * >( this->GetMyBoundingBoxInWorldSpace() )
    ->SetPoints(transformedCorners);
  this->GetMyBoundingBoxInWorldSpace()->ComputeBoundingBox();

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
}

/** Get the Mesh inside the spatial object */
template< typename TMesh >
typename MeshSpatialObject< TMesh >::MeshType *
MeshSpatialObject< TMesh >
::GetMesh()
{
  return m_Mesh.GetPointer();
}

template< typename TMesh >
const typename MeshSpatialObject< TMesh >::MeshType *
MeshSpatialObject< TMesh >
::GetMesh() const
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
  os << "m_IsInsideInWorldSpacePrecision: " << m_IsInsideInWorldSpacePrecision << std::endl;
  os << indent << m_Mesh << std::endl;
}

/** Get the modification time */
template< typename TMesh >
ModifiedTimeType
MeshSpatialObject< TMesh >
::GetMTime() const
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
