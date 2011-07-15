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
#ifndef __itkCleanQuadEdgeMeshFilter_h
#define __itkCleanQuadEdgeMeshFilter_h

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
template< class TInput, class TOutput=TInput >
class ITK_EXPORT CleanQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput >
{
public:
  typedef CleanQuadEdgeMeshFilter                             Self;
  typedef SmartPointer< Self >                                Pointer;
  typedef SmartPointer< const Self >                          ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(CleanQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef TInput                                    InputMeshType;
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

  typedef TOutput                                    OutputMeshType;
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

  itkSetMacro(AbsoluteTolerance, InputCoordRepType);
  itkSetMacro(RelativeTolerance, InputCoordRepType);
protected:
  CleanQuadEdgeMeshFilter()
  {
    this->m_AbsoluteTolerance2 = itk::NumericTraits< InputCoordRepType >::Zero;
    this->m_AbsoluteTolerance  = itk::NumericTraits< InputCoordRepType >::Zero;
    this->m_RelativeTolerance  = itk::NumericTraits< InputCoordRepType >::Zero;
  }

  virtual ~CleanQuadEdgeMeshFilter() {}

  InputCoordRepType m_AbsoluteTolerance2;
  InputCoordRepType m_AbsoluteTolerance;
  InputCoordRepType m_RelativeTolerance;

  void GenerateData()
  {
    InputCoordRepType zeroValue = itk::NumericTraits< InputCoordRepType >::Zero;

    if ( ( m_AbsoluteTolerance == zeroValue ) && ( m_RelativeTolerance != zeroValue ) )
      {
      itkAssertOrThrowMacro( ( m_RelativeTolerance > zeroValue ) && ( m_RelativeTolerance < 1. ),
                             "Relative tolerance out of range" );
      BoundingBoxPointer bounding_box = BoundingBoxType::New();
      bounding_box->SetPoints( this->GetInput()->GetPoints() );
      bounding_box->ComputeBoundingBox();

      m_AbsoluteTolerance2 = m_RelativeTolerance * m_RelativeTolerance
                             * bounding_box->GetDiagonalLength2();
      }

    if ( m_AbsoluteTolerance != zeroValue )
      {
      m_AbsoluteTolerance2 = m_AbsoluteTolerance * m_AbsoluteTolerance;
      }

    this->MergePoints();
    this->CleanPoints();
  }

  void MergePoints()
  {
    OutputMeshPointer output = this->GetOutput();

    CriterionPointer criterion = CriterionType::New();

    criterion->SetTopologicalChange(false);
    criterion->SetMeasureBound(m_AbsoluteTolerance2);

    DecimationPointer decimate = DecimationType::New();
    decimate->SetInput( this->GetInput() );
    decimate->SetCriterion(criterion);
    decimate->Update();

    InputMeshPointer temp = decimate->GetOutput();

    InputPointsContainerIterator p_it = temp->GetPoints()->Begin();
    InputPointsContainerIterator p_end = temp->GetPoints()->End();

    OutputPointType pOut;

    while ( p_it != p_end )
      {
      pOut.CastFrom( p_it.Value() );
      output->SetPoint(p_it.Index(), pOut);
      ++p_it;
      }

    // Copy Edge Cells
    InputCellsContainerIterator c_it = temp->GetEdgeCells()->Begin();
    InputCellsContainerIterator c_end = temp->GetEdgeCells()->End();
    InputEdgeCellType *         qe;
    InputQEPrimal *             QEGeom;

    while ( c_it != c_end )
      {
      qe = dynamic_cast< InputEdgeCellType * >( c_it.Value() );
      QEGeom = qe->GetQEGeom();
      output->AddEdgeWithSecurePointList( QEGeom->GetOrigin(),
                                          QEGeom->GetDestination() );
      ++c_it;
      }

    // Copy cells
    c_it = temp->GetCells()->Begin();
    c_end = temp->GetCells()->End();
    InputPolygonCellType *pe;

    while ( c_it != c_end )
      {
      pe = dynamic_cast< InputPolygonCellType * >( c_it.Value() );
      if ( pe )
        {
        InputPointIdList points;

        for ( InputPointsIdInternalIterator pit = pe->InternalPointIdsBegin();
              pit != pe->InternalPointIdsEnd(); ++pit )
          {
          points.push_back( ( *pit ) );
          }
        output->AddFaceWithSecurePointList(points);
        }
      ++c_it;
      }
  }

  void CleanPoints()
  {
    OutputMeshPointer output = this->GetOutput();

    OutputPointsContainerIterator p_it = output->GetPoints()->Begin();
    OutputPointsContainerIterator p_end = output->GetPoints()->End();
    OutputPointIdentifier         id(0);

    while ( p_it != p_end )
      {
      id = p_it->Index();
      if ( output->FindEdge(id) == 0 )
        {
        output->DeletePoint(id);
        }
      ++p_it;
      }

    output->SqueezePointsIds();
  }

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "AbsoluteTolerance2: " << m_AbsoluteTolerance2 << std::endl;
    os << indent << "AbsoluteTolerance: " << m_AbsoluteTolerance << std::endl;
    os << indent << "RelativeTolerance: " << m_RelativeTolerance << std::endl;
  }

private:
  CleanQuadEdgeMeshFilter(const Self &);
  void operator=(const Self &);
};
}
#endif
