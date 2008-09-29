#ifndef __itkQuadEdgeMeshCleanFilter_h
#define __itkQuadEdgeMeshCleanFilter_h

#include <itkQuadEdgeMeshToQuadEdgeMeshFilter.h>
#include <itkBoundingBox.h>

#include "itkQuadEdgeMeshSquaredEdgeLengthDecimation.h"
#include "itkQuadEdgeMeshDecimationCriteria.h"

namespace itk
{
/**
 * \class QuadEdgeMeshCleanFilter
 * \brief
*/
template< class TInput, class TOutput >
class QuadEdgeMeshCleanFilter :
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput >
{
public:
  typedef QuadEdgeMeshCleanFilter Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInput, TOutput > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshCleanFilter, QuadEdgeMeshToQuadEdgeMeshFilter );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  typedef TInput InputMeshType;
  typedef typename Superclass::InputMeshPointer InputMeshPointer;
  typedef typename Superclass::InputCoordRepType    InputCoordRepType;
  typedef typename Superclass::InputPointType       InputPointType;
  typedef typename Superclass::InputPointIdentifier InputPointIdentifier;
  typedef typename Superclass::InputQEPrimal        InputQEPrimal;
  typedef typename Superclass::InputVectorType      InputVectorType;

  typedef typename Superclass::InputEdgeCellType InputEdgeCellType; 
  typedef typename Superclass::InputPolygonCellType InputPolygonCellType;
  typedef typename Superclass::InputPointIdList InputPointIdList;
  typedef typename Superclass::InputCellTraits InputCellTraits;
  typedef typename Superclass::InputPointsIdInternalIterator
    InputPointsIdInternalIterator;
  typedef typename Superclass::InputQEIterator InputQEIterator;

  typedef typename InputMeshType::PointsContainer InputPointsContainer;
  typedef typename InputMeshType::PointsContainerPointer
    InputPointsContainerPointer;
  typedef typename InputMeshType::PointsContainerIterator
    InputPointsContainerIterator;

  typedef typename InputMeshType::CellsContainerIterator
      InputCellsContainerIterator;

  itkStaticConstMacro( PointDimension, unsigned int,
                       InputMeshType::PointDimension );

  typedef TOutput OutputMeshType;
  typedef typename Superclass::OutputMeshPointer OutputMeshPointer;
  typedef typename Superclass::OutputCoordRepType    OutputCoordRepType;
  typedef typename Superclass::OutputPointType       OutputPointType;
  typedef typename Superclass::OutputPointIdentifier OutputPointIdentifier;
  typedef typename Superclass::OutputQEPrimal        OutputQEPrimal;
  typedef typename Superclass::OutputVectorType      OutputVectorType;
  typedef typename OutputMeshType::QEType            OutputQEType;

  typedef typename OutputMeshType::PointsContainer OutputPointsContainer;
  typedef typename OutputMeshType::PointsContainerPointer
    OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator
    OutputPointsContainerIterator;

  typedef typename OutputMeshType::CellsContainerIterator
      OutputCellsContainerIterator;

  typedef BoundingBox< InputPointIdentifier, PointDimension,
    InputCoordRepType, InputPointsContainer > BoundingBoxType;
  typedef typename BoundingBoxType::Pointer BoundingBoxPointer;

  typedef MaxMeasureBoundCriterion< OutputMeshType > CriterionType;
  typedef typename CriterionType::Pointer CriterionPointer;

  typedef QuadEdgeMeshSquaredEdgeLengthDecimation< InputMeshType,
    InputMeshType, CriterionType > DecimationType;
  typedef typename DecimationType::Pointer DecimationPointer;

  itkSetMacro( AbsoluteTolerance, InputCoordRepType );
  itkSetMacro( RelativeTolerance, InputCoordRepType );

protected:
  QuadEdgeMeshCleanFilter() : Superclass(),
    m_AbsoluteTolerance2( static_cast< InputCoordRepType >( 0. ) ),
    m_AbsoluteTolerance( static_cast< InputCoordRepType >( 0. ) ),
    m_RelativeTolerance( static_cast< InputCoordRepType >( 0. ) )
   {}
  ~QuadEdgeMeshCleanFilter() {}

  InputCoordRepType m_AbsoluteTolerance2;
  InputCoordRepType m_AbsoluteTolerance;
  InputCoordRepType m_RelativeTolerance;

  void GenerateData()
  {
    if( ( m_AbsoluteTolerance == 0. ) && ( m_RelativeTolerance != 0. ) )
      {
      assert( ( m_RelativeTolerance > 0. ) && ( m_RelativeTolerance < 1. ) );
      BoundingBoxPointer bounding_box = BoundingBoxType::New();
      bounding_box->SetPoints( this->GetInput()->GetPoints() );
      bounding_box->ComputeBoundingBox();

      m_AbsoluteTolerance2 = m_RelativeTolerance * m_RelativeTolerance *
        bounding_box->GetDiagonalLength2();
      }
    if( m_AbsoluteTolerance != 0. )
      m_AbsoluteTolerance2 = m_AbsoluteTolerance * m_AbsoluteTolerance;

    MergePoints();
    CleanPoints();
  }

  void MergePoints()
  {
    OutputMeshPointer output = this->GetOutput();

    CriterionPointer criterion = CriterionType::New();
    criterion->SetTopologicalChange( false );
    criterion->SetMeasureBound( m_AbsoluteTolerance2 );

    DecimationPointer decimate = DecimationType::New();
    decimate->SetInput( this->GetInput() );
    decimate->SetCriterion( criterion );
    decimate->Update();

    InputMeshPointer temp = decimate->GetOutput();

    InputPointsContainerIterator p_it = temp->GetPoints()->Begin();
    InputPointsContainerIterator p_end = temp->GetPoints()->End();

    OutputPointType pOut;

    for( ; p_it != p_end; ++p_it )
      {
      pOut.CastFrom( p_it.Value() );
      output->SetPoint( p_it.Index(), pOut );
      }

    // Copy cells
  InputCellsContainerIterator c_it = temp->GetCells()->Begin();
  InputCellsContainerIterator c_end = temp->GetCells()->End();
  InputEdgeCellType* qe( 0 );
  InputPolygonCellType* pe( 0 );
  InputQEPrimal* QEGeom( 0 );

  for( ; c_it != c_end; ++c_it )
    {
    qe = (InputEdgeCellType*)0;
    pe = (InputPolygonCellType*)0;
    if( ( qe = dynamic_cast< InputEdgeCellType* >( c_it.Value() ) ) )
      {
      QEGeom = qe->GetQEGeom( );
      output->AddEdgeWithSecurePointList( QEGeom->GetOrigin(),
        QEGeom->GetDestination() );
      }
    else
      {
      pe = dynamic_cast< InputPolygonCellType* >( c_it.Value());
      if( pe )
        {
        InputPointIdList points;

        for( InputPointsIdInternalIterator pit = pe->InternalPointIdsBegin();
             pit != pe->InternalPointIdsEnd( ); ++pit )
          {
          points.push_back( ( *pit ) );
          }
        output->AddFaceWithSecurePointList( points );
        } 
      }
    }
  }

  void CleanPoints()
  {
    OutputMeshPointer output = this->GetOutput();

    OutputPointsContainerIterator p_it = output->GetPoints()->Begin();
    OutputPointsContainerIterator p_end = output->GetPoints()->End();
    OutputPointIdentifier id( 0 );

    for( ; p_it != p_end; ++p_it )
      {
      id = p_it->Index();
      if( output->FindEdge( id ) == 0 )
        output->DeletePoint( id );
      }

    output->SqueezePointsIds( );
  }

private:
  QuadEdgeMeshCleanFilter( const Self& );
  void operator = ( const Self& );
};
}
#endif
