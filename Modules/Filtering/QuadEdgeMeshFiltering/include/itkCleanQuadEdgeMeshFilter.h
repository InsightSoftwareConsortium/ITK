/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkCleanQuadEdgeMeshFilter_h
#define itkCleanQuadEdgeMeshFilter_h

#include "itkIntTypes.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkBoundingBox.h"

#include "itkSquaredEdgeLengthDecimationQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshDecimationCriteria.h"

namespace itk
{
/**
 * \class CleanQuadEdgeMeshFilter
 * \brief TODO
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class ITK_TEMPLATE_EXPORT CleanQuadEdgeMeshFilter : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CleanQuadEdgeMeshFilter);

  using Self = CleanQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CleanQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  using InputMeshType = TInputMesh;
  using typename Superclass::InputMeshPointer;
  using typename Superclass::InputCoordRepType;
  using typename Superclass::InputPointType;
  using typename Superclass::InputPointIdentifier;
  using typename Superclass::InputQEPrimal;
  using typename Superclass::InputVectorType;

  using typename Superclass::InputEdgeCellType;
  using typename Superclass::InputPolygonCellType;
  using typename Superclass::InputPointIdList;
  using typename Superclass::InputCellTraits;
  using typename Superclass::InputPointsIdInternalIterator;
  using typename Superclass::InputQEIterator;

  using InputPointsContainer = typename InputMeshType::PointsContainer;
  using InputPointsContainerPointer = typename InputMeshType::PointsContainerPointer;
  using InputPointsContainerIterator = typename InputMeshType::PointsContainerIterator;

  using InputCellsContainerIterator = typename InputMeshType::CellsContainerIterator;

  static constexpr unsigned int PointDimension = InputMeshType::PointDimension;

  using OutputMeshType = TOutputMesh;
  using typename Superclass::OutputMeshPointer;
  using typename Superclass::OutputCoordRepType;
  using typename Superclass::OutputPointType;
  using typename Superclass::OutputPointIdentifier;
  using typename Superclass::OutputQEPrimal;
  using typename Superclass::OutputVectorType;

  using OutputQEType = typename OutputMeshType::QEType;
  using OutputPointsContainer = typename OutputMeshType::PointsContainer;
  using OutputPointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using OutputPointsContainerIterator = typename OutputMeshType::PointsContainerIterator;

  using OutputCellsContainerIterator = typename OutputMeshType::CellsContainerIterator;

  using BoundingBoxType =
    BoundingBox<InputPointIdentifier, Self::PointDimension, InputCoordRepType, InputPointsContainer>;

  using BoundingBoxPointer = typename BoundingBoxType::Pointer;

  using CriterionType = MaxMeasureBoundCriterion<OutputMeshType>;
  using CriterionPointer = typename CriterionType::Pointer;

  using DecimationType = SquaredEdgeLengthDecimationQuadEdgeMeshFilter<InputMeshType, InputMeshType, CriterionType>;
  using DecimationPointer = typename DecimationType::Pointer;

  /** TODO */
  itkSetMacro(AbsoluteTolerance, InputCoordRepType);
  itkGetConstMacro(AbsoluteTolerance, InputCoordRepType);

  /** TODO */
  itkSetClampMacro(RelativeTolerance, InputCoordRepType, 0.0, 1.0);
  itkGetConstMacro(RelativeTolerance, InputCoordRepType);

protected:
  CleanQuadEdgeMeshFilter();

  ~CleanQuadEdgeMeshFilter() override = default;

  void
  GenerateData() override;

  virtual void
  MergePoints(const InputCoordRepType absoluteToleranceSquared);

  virtual void
  CleanPoints();

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  InputCoordRepType m_AbsoluteTolerance;
  InputCoordRepType m_RelativeTolerance;

  BoundingBoxPointer m_BoundingBox;
  CriterionPointer   m_Criterion;
  DecimationPointer  m_Decimation;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCleanQuadEdgeMeshFilter.hxx"
#endif

#endif
