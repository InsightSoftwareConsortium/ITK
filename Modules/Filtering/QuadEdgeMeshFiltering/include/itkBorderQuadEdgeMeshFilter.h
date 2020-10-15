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
#ifndef itkBorderQuadEdgeMeshFilter_h
#define itkBorderQuadEdgeMeshFilter_h

#include "itkAutoPointer.h"
#include "itkQuadEdgeMesh.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshBoundaryEdgesMeshFunction.h"
#include "ITKQuadEdgeMeshFilteringExport.h"

namespace itk
{
/**\class BorderQuadEdgeMeshFilterEnums
 * \brief Contains all enum classes used by
 * \ingroup ITKQuadEdgeMeshFiltering
 */
class BorderQuadEdgeMeshFilterEnums
{
public:
  /**\class BorderTransform
   * \ingroup ITKQuadEdgeMeshFiltering
   * */
  enum class BorderTransform : uint8_t
  {
    SQUARE_BORDER_TRANSFORM = 0,
    DISK_BORDER_TRANSFORM
  };

  /**\class BorderPick
   * \ingroup ITKQuadEdgeMeshFiltering
   * */
  enum class BorderPick : uint8_t
  {
    LONGEST = 0,
    LARGEST
  };
};
// Define how to print enumeration
extern ITKQuadEdgeMeshFiltering_EXPORT std::ostream &
                                       operator<<(std::ostream & out, const BorderQuadEdgeMeshFilterEnums::BorderTransform value);
extern ITKQuadEdgeMeshFiltering_EXPORT std::ostream &
                                       operator<<(std::ostream & out, const BorderQuadEdgeMeshFilterEnums::BorderPick value);
/**
 * \class BorderQuadEdgeMeshFilter
 * \brief Transform one border of a QuadEdgeMesh into either a circle
 * (conformal) or a square (arclength-wise).
 *
 * This class is one important step when computing a planar parameterization
 * of one mesh.
 *
 * If the input mesh has several boundaries, one can choose
 * the one which would be transformed via the variable m_BorderPick.
 *
 * \li <tt>m_BorderPick == Self::LONGEST</tt> refers to the boundary
 * \f$ b \f$ which satisfies:
 * \f[ b = \arg \max_{b^k} \sum_{i=1}^{N^k} \left\| x_{i}^k - x_{i+1}^k \right\| \f]
 *
 * \li <tt>m_BorderPick == Self::LARGEST</tt> refers to the boundary
 * \f$ b \f$ which satisfies:
 * \f[ b = \arg \max_{b^k} N^k \f]
 *
 * \sa ParameterizationQuadEdgeMeshFilter
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class ITK_TEMPLATE_EXPORT BorderQuadEdgeMeshFilter : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BorderQuadEdgeMeshFilter);

  /** Basic types. */
  using Self = BorderQuadEdgeMeshFilter;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = TInputMesh;
  using InputMeshConstPointer = typename InputMeshType::ConstPointer;
  using InputCoordRepType = typename InputMeshType::CoordRepType;
  using InputPointType = typename InputMeshType::PointType;
  using InputTraits = typename InputMeshType::Traits;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEType = typename InputMeshType::QEType;
  using InputIteratorGeom = typename InputQEType::IteratorGeom;
  using InputVectorType = typename InputMeshType::VectorType;
  using InputEdgeListType = typename InputMeshType::EdgeListType;
  using InputEdgeListPointerType = AutoPointer<InputEdgeListType>;
  using InputEdgeListIterator = typename InputEdgeListType::iterator;
  using InputEdgeCellType = typename InputMeshType::EdgeCellType;
  using InputPolygonCellType = typename InputMeshType::PolygonCellType;
  using InputPointIdList = typename InputMeshType::PointIdList;
  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerConstIterator = typename InputMeshType::PointsContainerConstIterator;
  using InputCellsContainerConstIterator = typename InputMeshType::CellsContainerConstIterator;

  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputCoordRepType = typename OutputMeshType::CoordRepType;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputTraits = typename OutputMeshType::Traits;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputQEType = typename OutputMeshType::QEType;
  using OutputVectorType = typename OutputMeshType::VectorType;
  using OutputEdgeListType = typename OutputMeshType::EdgeListType;
  using OutputEdgeCellType = typename OutputMeshType::EdgeCellType;
  using OutputPolygonCellType = typename OutputMeshType::PolygonCellType;
  using OutputPointIdList = typename OutputMeshType::PointIdList;
  using OutputPointsContainer = typename OutputMeshType::PointsContainer;
  using OutputPointsContainerConstIterator = typename OutputMeshType::PointsContainerConstIterator;
  using OutputCellsContainerConstIterator = typename OutputMeshType::CellsContainerConstIterator;

  itkNewMacro(Self);
  itkTypeMacro(BorderQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
  static constexpr unsigned int PointDimension = InputTraits::PointDimension;

  using InputVectorPointType = std::vector<InputPointType>;
  using MapPointIdentifier = std::map<InputPointIdentifier, OutputPointIdentifier>;
  using MapPointIdentifierIterator = typename MapPointIdentifier::iterator;

  using BoundaryRepresentativeEdgesType = QuadEdgeMeshBoundaryEdgesMeshFunction<InputMeshType>;
  using BoundaryRepresentativeEdgesPointer = typename BoundaryRepresentativeEdgesType::Pointer;

  using BorderTransformEnum = itk::BorderQuadEdgeMeshFilterEnums::BorderTransform;
  using BorderPickEnum = itk::BorderQuadEdgeMeshFilterEnums::BorderPick;
#if !defined(ITK_LEGACY_REMOVE)
  /** Exposes enums values for backwards compatibility*/
  static constexpr BorderTransformEnum SQUARE_BORDER_TRANSFORM = BorderTransformEnum::SQUARE_BORDER_TRANSFORM;
  static constexpr BorderTransformEnum DISK_BORDER_TRANSFORM = BorderTransformEnum::DISK_BORDER_TRANSFORM;

  static constexpr BorderPickEnum LONGEST = BorderPickEnum::LONGEST;
  static constexpr BorderPickEnum LARGEST = BorderPickEnum::LARGEST;
#endif

  itkSetEnumMacro(TransformType, BorderTransformEnum);
  itkGetConstMacro(TransformType, BorderTransformEnum);

  itkSetEnumMacro(BorderPick, BorderPickEnum);
  itkGetConstMacro(BorderPick, BorderPickEnum);

  itkSetMacro(Radius, InputCoordRepType);
  itkGetConstMacro(Radius, InputCoordRepType);

  void
  ComputeTransform();

  MapPointIdentifier
  GetBoundaryPtMap();

  InputVectorPointType
  GetBorder();

protected:
  BorderQuadEdgeMeshFilter();

  ~BorderQuadEdgeMeshFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  BorderTransformEnum m_TransformType;
  BorderPickEnum      m_BorderPick;

  InputCoordRepType m_Radius;

  InputVectorPointType m_Border;

  MapPointIdentifier m_BoundaryPtMap;

  void
  GenerateData() override;

  void
  ComputeBoundary();

  InputQEType *
  ComputeLongestBorder();

  InputQEType *
  ComputeLargestBorder();

  void
  DiskTransform();

  InputPointType
  GetMeshBarycentre();

  InputCoordRepType
  RadiusMaxSquare();

  void
  ArcLengthSquareTransform();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBorderQuadEdgeMeshFilter.hxx"
#endif

#endif
