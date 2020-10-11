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
#ifndef itkSphereMeshSource_h
#define itkSphereMeshSource_h

#include "vnl/vnl_matrix_fixed.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkVector.h"
#include "itkTriangleCell.h"
#include "itkDefaultStaticMeshTraits.h"

namespace itk
{
/** \class SphereMeshSource
 * \brief
 *
 * Input the center and resolutions in 2 directions(verizon and horizon)
 * to create a sphere-like deformable model. The cell on the surface is
 * in the shape of triangular.
 * More parameters are added to make the sphere mesh have global and local
 * deform ability.
 * \ingroup ITKMesh
 */
template <typename TOutputMesh>
class ITK_TEMPLATE_EXPORT SphereMeshSource : public MeshSource<TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SphereMeshSource);

  /** Standard "Self" type alias. */
  using Self = SphereMeshSource;
  using Superclass = MeshSource<TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SphereMeshSource, MeshSource);

  /** Hold on to the type information specified by the template parameters. */
  using OutputMeshType = TOutputMesh;
  using OMeshTraits = typename OutputMeshType::MeshTraits;
  using OPointType = typename OutputMeshType::PointType;
  using OPixelType = typename OMeshTraits::PixelType;

  /** Some convenient type alias. */
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using CellTraits = typename OutputMeshType::CellTraits;
  using PointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using PointsContainer = typename OutputMeshType::PointsContainer;

  /** Define the triangular cell types which forms the surface of the model
   * and will be used in FEM application. */
  using CellInterfaceType = CellInterface<OPixelType, CellTraits>;
  using TriCellType = TriangleCell<CellInterfaceType>;
  using TriCellAutoPointer = typename TriCellType::SelfAutoPointer;
  using CellAutoPointer = typename TriCellType::CellAutoPointer;

  /** All these parameter setting function are public temporarily to make the
   * test easier */
  itkSetMacro(ResolutionX, unsigned int);
  itkSetMacro(ResolutionY, unsigned int);

  itkSetMacro(Center, OPointType);
  itkSetMacro(Scale, OPointType);

  itkSetMacro(Squareness1, double);
  itkSetMacro(Squareness2, double);

protected:
  SphereMeshSource();
  ~SphereMeshSource() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /** model center */
  OPointType m_Center;

  /** model resolutions */
  unsigned int m_ResolutionX;
  unsigned int m_ResolutionY;

  /** model scales */
  OPointType m_Scale;

  /** model squareness */
  double m_Squareness1;
  double m_Squareness2;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSphereMeshSource.hxx"
#endif
#endif
