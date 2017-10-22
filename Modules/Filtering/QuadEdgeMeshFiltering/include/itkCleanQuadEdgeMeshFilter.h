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
template< typename TInputMesh, typename TOutputMesh=TInputMesh >
class ITK_TEMPLATE_EXPORT CleanQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef CleanQuadEdgeMeshFilter                             Self;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
                                                              Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CleanQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef TInputMesh                                InputMeshType;
  typedef typename Superclass::InputMeshPointer     InputMeshPointer;
  typedef typename Superclass::InputCoordRepType    InputCoordRepType;
  typedef typename Superclass::InputPointType       InputPointType;
  typedef typename Superclass::InputPointIdentifier InputPointIdentifier;
  typedef typename Superclass::InputQEPrimal        InputQEPrimal;
  typedef typename Superclass::InputVectorType      InputVectorType;

  typedef typename Superclass::InputEdgeCellType             InputEdgeCellType;
  typedef typename Superclass::InputPolygonCellType          InputPolygonCellType;
  typedef typename Superclass::InputPointIdList              InputPointIdList;
  typedef typename Superclass::InputCellTraits               InputCellTraits;
  typedef typename Superclass::InputPointsIdInternalIterator InputPointsIdInternalIterator;
  typedef typename Superclass::InputQEIterator               InputQEIterator;

  typedef typename InputMeshType::PointsContainer         InputPointsContainer;
  typedef typename InputMeshType::PointsContainerPointer  InputPointsContainerPointer;
  typedef typename InputMeshType::PointsContainerIterator InputPointsContainerIterator;

  typedef typename InputMeshType::CellsContainerIterator InputCellsContainerIterator;

  itkStaticConstMacro(PointDimension, unsigned int, InputMeshType::PointDimension);

  typedef TOutputMesh                                OutputMeshType;
  typedef typename Superclass::OutputMeshPointer     OutputMeshPointer;
  typedef typename Superclass::OutputCoordRepType    OutputCoordRepType;
  typedef typename Superclass::OutputPointType       OutputPointType;
  typedef typename Superclass::OutputPointIdentifier OutputPointIdentifier;
  typedef typename Superclass::OutputQEPrimal        OutputQEPrimal;
  typedef typename Superclass::OutputVectorType      OutputVectorType;

  typedef typename OutputMeshType::QEType                  OutputQEType;
  typedef typename OutputMeshType::PointsContainer         OutputPointsContainer;
  typedef typename OutputMeshType::PointsContainerPointer  OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator OutputPointsContainerIterator;

  typedef typename OutputMeshType::CellsContainerIterator OutputCellsContainerIterator;

  typedef BoundingBox< InputPointIdentifier, itkGetStaticConstMacro(PointDimension),
                       InputCoordRepType, InputPointsContainer > BoundingBoxType;

  typedef typename BoundingBoxType::Pointer BoundingBoxPointer;

  typedef MaxMeasureBoundCriterion< OutputMeshType > CriterionType;
  typedef typename CriterionType::Pointer            CriterionPointer;

  typedef SquaredEdgeLengthDecimationQuadEdgeMeshFilter< InputMeshType,
                                                   InputMeshType,
                                                   CriterionType >                          DecimationType;
  typedef typename DecimationType::Pointer DecimationPointer;

  /** TODO */
  itkSetMacro(AbsoluteTolerance, InputCoordRepType);
  itkGetConstMacro(AbsoluteTolerance, InputCoordRepType);

  /** TODO */
  itkSetClampMacro(RelativeTolerance, InputCoordRepType, 0.0, 1.0);
  itkGetConstMacro(RelativeTolerance, InputCoordRepType);

protected:
  CleanQuadEdgeMeshFilter();

  virtual ~CleanQuadEdgeMeshFilter() ITK_OVERRIDE {}

  virtual void GenerateData() ITK_OVERRIDE;

  virtual void MergePoints( const InputCoordRepType absoluteToleranceSquared );

  virtual void CleanPoints();

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CleanQuadEdgeMeshFilter);

  InputCoordRepType m_AbsoluteTolerance;
  InputCoordRepType m_RelativeTolerance;

  BoundingBoxPointer m_BoundingBox;
  CriterionPointer   m_Criterion;
  DecimationPointer  m_Decimation;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCleanQuadEdgeMeshFilter.hxx"
#endif

#endif
