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
#ifndef itkConformalFlatteningMeshFilter_h
#define itkConformalFlatteningMeshFilter_h

#include "itkMesh.h"
#include "itkMeshToMeshFilter.h"

// vnl headers
#include "itkMath.h"
#include "vnl/vnl_sparse_matrix.h"


namespace itk
{
/** \class ConformalFlatteningMeshFilter
 * \brief
 *
 * ConformalFlatteningMeshFilter applies a conformal mapping from 3D to 2D.
 *
 * This code was contributed in the Insight Journal paper:
 * "Conformal Flattening ITK Filter"
 * by Gao Y., Melonakos J., Tannenbaum A.
 * https://hdl.handle.net/1926/225
 * http://www.insight-journal.org/browse/publication/112
 *
 * \ingroup MeshFilters
 * \sa TransformMeshFilter
 * \ingroup ITKReview
 */

template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT ConformalFlatteningMeshFilter : public MeshToMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConformalFlatteningMeshFilter);

  /** Standard class type aliases. */
  using Self = ConformalFlatteningMeshFilter;

  using InputMeshType = TInputMesh;
  using OutputMeshType = TOutputMesh;

  using Superclass = MeshToMeshFilter<TInputMesh, TOutputMesh>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshConstPointer = typename InputMeshType::ConstPointer;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using InputPointType = typename InputMeshType::PointType;
  using OutputPointType = typename OutputMeshType::PointType;

  /** Type for representing coordinates. */
  // using CoordRepType = typename TInputMesh::CoordRepType;
  using CoordRepType = double;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConformalFlatteningMeshFilter, MeshToMeshFilter);

  /** Convenient constants obtained from TMeshTraits template parameter. */
  static constexpr unsigned int InputPointDimension = TInputMesh::PointDimension;
  static constexpr unsigned int OutputPointDimension = TOutputMesh::PointDimension;

  using PointsContainer = typename InputMeshType::PointsContainer;
  using CellsContainer = typename InputMeshType::CellsContainer;
  using PointIdentifier = typename InputMeshType::PointIdentifier;
  using CellIdentifier = typename InputMeshType::CellIdentifier;
  using PointIterator = typename PointsContainer::ConstIterator;
  using CellIterator = typename CellsContainer::ConstIterator;
  using CellType = typename InputMeshType::CellType;
  using PointIdIterator = typename CellType::PointIdIterator;
  using CellAutoPointer = typename CellType::CellAutoPointer;

  /** Select the cell that will be used as reference for the flattening.
   * This value must be the identifier of a cell existing in the input Mesh.
   * A point of this cell will be mapped to infinity on the plane, or it
   * will be mapped to the north-pole on the sphere. It is recommended to
   * select a cell whose curvature is relatively flat. */
  void
  SetPolarCellIdentifier(CellIdentifier cellId);

  /** Define the scale of the mapping. The largest coordinates of the
   * furthest point in the plane is m_MapScale. */
  void
  SetScale(double);

  /** Define that the input surface will be mapped to a sphere */
  void
  MapToSphere();

  /** Define that the input surface will be mapped to a plane.
   *  This skips the steps of the stereographic projection. */
  void
  MapToPlane();

protected:
  ConformalFlatteningMeshFilter();
  ~ConformalFlatteningMeshFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Requested Data */
  void
  GenerateData() override;

private:
  using VectorCoordType = vnl_vector<CoordRepType>;
  using SparseMatrixCoordType = vnl_sparse_matrix<CoordRepType>;

  /** Cell Id  in which the point P, which is used
   * to define the mapping, lies in. */
  unsigned int m_PolarCellIdentifier;

  /** Whether the result is sphere or plane.  */
  bool m_MapToSphere;

  /** The scale when mapping to the plane.
   *  Determines how far the farthest point goes. */
  double m_MapScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConformalFlatteningMeshFilter.hxx"
#endif

#endif
