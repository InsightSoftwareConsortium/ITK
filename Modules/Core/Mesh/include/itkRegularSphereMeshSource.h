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
#ifndef itkRegularSphereMeshSource_h
#define itkRegularSphereMeshSource_h

#include "itkIntTypes.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkTriangleCell.h"

namespace itk
{
/** \class RegularSphereMeshSource
 * \brief
 * Inputs are the center of the mesh, the scale (radius in each dimension) of the mesh
 * and a resolution parameter, which corresponds to the recursion
 * depth while creating a spherical triangle mesh.
 *
 * Don't use recursion depths larger than 5, because mesh generation gets very slow.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKMesh
 */
template <typename TOutputMesh>
class ITK_TEMPLATE_EXPORT RegularSphereMeshSource : public MeshSource<TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegularSphereMeshSource);

  /** Standard "Self" type alias. */
  using Self = RegularSphereMeshSource;
  using Superclass = itk::MeshSource<TOutputMesh>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegularSphereMeshSource, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  using OutputMeshType = TOutputMesh;
  using MeshTraits = typename OutputMeshType::MeshTraits;
  using PointType = typename OutputMeshType::PointType;
  using PixelType = typename MeshTraits::PixelType;
  using VectorType = typename PointType::VectorType;

  /** Some convenient type alias. */
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using CellTraits = typename OutputMeshType::CellTraits;
  using PointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using PointsContainer = typename OutputMeshType::PointsContainer;

  /** Define the triangular cell types which form the surface  */
  using CellInterfaceType = itk::CellInterface<PixelType, CellTraits>;
  using TriCellType = itk::TriangleCell<CellInterfaceType>;
  using TriCellAutoPointer = typename TriCellType::SelfAutoPointer;
  using CellAutoPointer = typename TriCellType::CellAutoPointer;

  using IndexPairType = std::pair<IdentifierType, IdentifierType>;
  using PointMapType = itk::MapContainer<IndexPairType, IdentifierType>;

  /** Set the resolution level to be used for generating cells in the Sphere.
   *  High values of this parameter will produce sphere with more triangles. */
  itkSetMacro(Resolution, unsigned int);
  itkGetConstMacro(Resolution, unsigned int);

  /** Set/Get Coordinates of the Sphere center. */
  itkSetMacro(Center, PointType);
  itkGetConstMacro(Center, PointType);

  /** Set/Get scales of the Sphere. This is a vector of values that can
   * actually be used for generating ellipsoids aligned with the coordinate
   * axis. */
  itkSetMacro(Scale, VectorType);
  itkGetConstMacro(Scale, VectorType);

protected:
  RegularSphereMeshSource();
  ~RegularSphereMeshSource() override = default;
  void
  PrintSelf(std::ostream & os, itk::Indent indent) const override;

  void
  GenerateData() override;

  PointType
  Divide(const PointType & p1, const PointType & p2) const;

  void
  AddCell(OutputMeshType * mesh, const typename OutputMeshType::PointIdentifier * pointIds, IdentifierType idx);

  /** model center */
  PointType m_Center;

  /** models resolution */
  unsigned int m_Resolution;

  /** model scales */
  VectorType m_Scale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegularSphereMeshSource.hxx"
#endif

#endif //_itkRegularSphereMeshSource_h
