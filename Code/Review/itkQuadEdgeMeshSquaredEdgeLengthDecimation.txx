#ifndef __itkQuadEdgeMeshSquaredEdgeLengthDecimation_txx
#define __itkQuadEdgeMeshSquaredEdgeLengthDecimation_txx

#include "itkQuadEdgeMeshSquaredEdgeLengthDecimation.h"

namespace itk
{

template< class TInput, class TOutput, class TCriterion >
QuadEdgeMeshSquaredEdgeLengthDecimation< TInput, TOutput, TCriterion >::
QuadEdgeMeshSquaredEdgeLengthDecimation() : Superclass()
{
}

template< class TInput, class TOutput, class TCriterion >
QuadEdgeMeshSquaredEdgeLengthDecimation< TInput, TOutput, TCriterion >::
~QuadEdgeMeshSquaredEdgeLengthDecimation()
{
}

template< class TInput, class TOutput, class TCriterion >
typename
QuadEdgeMeshSquaredEdgeLengthDecimation< TInput, TOutput, TCriterion
>::OutputPointType
QuadEdgeMeshSquaredEdgeLengthDecimation< TInput, TOutput, TCriterion >::
Relocate( OutputQEType* iEdge )
{
  OutputMeshPointer output = this->GetOutput();
  OutputPointIdentifier id_org = iEdge->GetOrigin();
  OutputPointIdentifier id_dest = iEdge->GetDestination();

  OutputPointType oPt;
  oPt.SetToMidPoint( output->GetPoint( id_org ),
                     output->GetPoint( id_dest ) );

  return oPt;
}

}
#endif
