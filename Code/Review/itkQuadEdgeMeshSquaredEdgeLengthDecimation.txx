/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshSquaredEdgeLengthDecimation.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
