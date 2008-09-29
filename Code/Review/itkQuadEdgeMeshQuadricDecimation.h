#ifndef __itkQuadEdgeMeshQuadricDecimation_h
#define __itkQuadEdgeMeshQuadricDecimation_h

#include "itkQuadEdgeMeshEdgeMergeDecimationFilter.h"
#include "itkQuadEdgeMeshDecimationQuadricElementHelper.h"

namespace itk
  {
  /**
   * \class QuadEdgeMeshQuadricDecimation
   * \brief
  */
  template< class TInput, class TOutput, class TCriterion >
  class QuadEdgeMeshQuadricDecimation :
    public QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >
    {
    public:
      typedef QuadEdgeMeshQuadricDecimation Self;
      typedef SmartPointer< Self > Pointer;
      typedef SmartPointer< const Self > ConstPointer;
      typedef QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput,
        TCriterion > Superclass;

      /** Run-time type information (and related methods).   */
      itkTypeMacro( QuadEdgeMeshQuadricDecimation,
        QuadEdgeMeshEdgeMergeDecimationFilter );

      /** New macro for creation of through a Smart Pointer   */
      itkNewMacro( Self );

      typedef TInput InputMeshType;
      typedef typename InputMeshType::Pointer InputMeshPointer;

      typedef TOutput OutputMeshType;
      typedef typename OutputMeshType::Pointer OutputMeshPointer;
      typedef typename OutputMeshType::PointIdentifier OutputPointIdentifier;
      typedef typename OutputMeshType::PointType OutputPointType;
      typedef typename OutputPointType::CoordRepType OutputCoordType;
      typedef typename OutputMeshType::QEType OutputQEType;
      typedef typename OutputMeshType::EdgeCellType OutputEdgeCellType;
      typedef typename OutputMeshType::CellsContainerIterator
      OutputCellsContainerIterator;
      typedef typename OutputMeshType::PointsContainerPointer
        OutputPointsContainerPointer;
      typedef typename OutputMeshType::PointsContainerIterator
        OutputPointsContainerIterator;
      
      itkStaticConstMacro( OutputPointDimension, unsigned int,
        OutputMeshType::PointDimension );


      typedef TCriterion CriterionType;
      typedef typename CriterionType::MeasureType MeasureType;

      typedef typename Superclass::PriorityType PriorityType;
      typedef typename Superclass::PriorityQueueItemType PriorityQueueItemType;
      typedef typename Superclass::PriorityQueueType PriorityQueueType;
      typedef typename Superclass::PriorityQueuePointer PriorityQueuePointer;

      typedef typename Superclass::QueueMapType QueueMapType;
      typedef typename Superclass::QueueMapIterator QueueMapIterator;

      typedef typename Superclass::OperatorType OperatorType;
      typedef typename Superclass::OperatorPointer OperatorPointer;

      typedef QuadEdgeMeshDecimationQuadricElementHelper< OutputPointType > 
        QuadricElementType;
      typedef std::map< OutputPointIdentifier, QuadricElementType > 
        QuadricElementMapType;
      typedef typename QuadricElementMapType::iterator
        QuadricElementMapIterator;
          
    protected:

      QuadEdgeMeshQuadricDecimation() : Superclass( ) {}
      virtual ~QuadEdgeMeshQuadricDecimation() {}
      
      QuadricElementMapType m_Quadric;

      virtual void Initialize()
      {
        OutputMeshPointer output = this->GetOutput();
        OutputPointsContainerPointer points = output->GetPoints();
        OutputPointsContainerIterator it = points->Begin();
        OutputPointIdentifier p_id( 0 );
        OutputQEType* qe( 0 );
        OutputQEType* qe_it( 0 );
        
        for( ; it != points->End(); it++ )
          {
          p_id = it->Index();
          
          qe = output->FindEdge( p_id );
          if( qe != 0 )
            {
            qe_it = qe;
            do
              {
              QuadricAtOrigin( qe_it, m_Quadric[p_id] );
              qe_it = qe_it->GetOnext();
              } while( qe_it != qe );
            }
          }
      }
      
      inline void QuadricAtOrigin( OutputQEType* iEdge,
        QuadricElementType& oQ ) 
      {
        OutputMeshPointer output = this->GetOutput();
        
        OutputPointIdentifier id[3];
        id[0] = iEdge->GetOrigin();
        id[1] = iEdge->GetDestination();
        id[2] = iEdge->GetOnext()->GetDestination();
        
        OutputPointType p[3];
        
        for( int i = 0; i < 3; i++ )
          p[i] = output->GetPoint( id[i] );
          
        oQ.AddTriangle( p[0], p[1], p[2] );
      }
      
      /**
      * \brief Compute the measure value for iEdge
      * \param[in] iEdge
      * \return measure value, here the squared edge length
      */
      inline MeasureType MeasureEdge( OutputQEType* iEdge )
      {
        OutputPointIdentifier id_org = iEdge->GetOrigin();
        OutputPointIdentifier id_dest = iEdge->GetDestination();
        QuadricElementType Q = m_Quadric[ id_org ] + m_Quadric[ id_dest ];
        return static_cast< MeasureType >( Q.ComputeErrorAtOptimalLocation() );
      }
      
      virtual void DeletePoint( const OutputPointIdentifier& iIdToBeDeleted,
        const OutputPointIdentifier& iRemaining )
      {
        Superclass::DeletePoint( iIdToBeDeleted, iRemaining );
        m_Quadric[iRemaining] += m_Quadric[iIdToBeDeleted];
      }

      /**
      * \brief
      * \param[in]
      * \return
      */
      OutputPointType Relocate( OutputQEType* iEdge )
      {
        OutputPointIdentifier id_org = iEdge->GetOrigin();
        OutputPointIdentifier id_dest = iEdge->GetDestination();
        QuadricElementType Q = m_Quadric[ id_org ] + m_Quadric[ id_dest ];
        return Q.ComputeOptimalLocation();
      }

    private:
      QuadEdgeMeshQuadricDecimation( const Self& );
      void operator = ( const Self& );
    };

}
#endif
