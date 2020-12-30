/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMeshSpatialObject_h
#define itkMeshSpatialObject_h

#include "itkMesh.h"
#include "itkSpatialObject.h"

namespace itk
{
/**
 *\class MeshSpatialObject
 * \brief Implementation of an Mesh as spatial object.
 *
 * This class combines functionalities from a spatial object,
 * and an itkMesh.
 *
 * \sa SpatialObject
 * \ingroup ITKSpatialObjects
 */

template <typename TMesh = Mesh<int>>
class ITK_TEMPLATE_EXPORT MeshSpatialObject : public SpatialObject<TMesh::PointDimension>
{
public:
  using ScalarType = double;
  using Self = MeshSpatialObject<TMesh>;

  static constexpr unsigned int Dimension = TMesh::PointDimension;

  using Superclass = SpatialObject<Self::Dimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MeshType = TMesh;
  using MeshPointer = typename MeshType::Pointer;
  using TransformType = typename Superclass::TransformType;
  using PointType = typename Superclass::PointType;
  using BoundingBoxType = typename Superclass::BoundingBoxType;

  using PointContainerType = VectorContainer<IdentifierType, PointType>;
  using PointContainerPointer = typename PointContainerType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeshSpatialObject, SpatialObject);

  /** Reset the spatial object to its initial condition, yet preserves
   *   Id, Parent, and Child information */
  void
  Clear() override;

  /** Set the Mesh. */
  void
  SetMesh(MeshType * mesh);

  /** Get a pointer to the Mesh currently attached to the object. */
  MeshType *
  GetMesh();
  const MeshType *
  GetMesh() const;

  /** Returns true if the point is inside, false otherwise. */
  bool
  IsInsideInObjectSpace(const PointType & point) const override;

  /* Avoid hiding the overload that supports depth and name arguments */
  using Superclass::IsInsideInObjectSpace;

  /** Returns the latest modified time of the object and its component. */
  ModifiedTimeType
  GetMTime() const override;

#if !defined(ITK_LEGACY_REMOVE)
  /** \deprecated Return the type of pixel used */
  itkLegacyMacro(const char * GetPixelTypeName()) { return m_PixelType.c_str(); }
#endif

  /** Set/Get the precision for the IsInsideInObjectSpace function.
   *  This is used when the cell is a triangle, in this case, it's more likely
   *  that the given point will not be falling exactly on the triangle surface.
   *  If the distance from the point to the surface is <= to
   *  m_IsInsidePrecisionInObjectSpace the point is considered inside the mesh.
   *  The default value is 1. */
  itkSetMacro(IsInsidePrecisionInObjectSpace, double);
  itkGetConstMacro(IsInsidePrecisionInObjectSpace, double);

protected:
  /** Compute the boundaries of the iamge spatial object. */
  void
  ComputeMyBoundingBox() override;

  MeshSpatialObject();
  ~MeshSpatialObject() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  typename LightObject::Pointer
  InternalClone() const override;

private:
  MeshPointer m_Mesh;
#if !defined(ITK_LEGACY_REMOVE)
  std::string m_PixelType;
#endif
  double m_IsInsidePrecisionInObjectSpace;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeshSpatialObject.hxx"
#endif

#endif // itkMeshSpatialObject_h
