/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshToQuadEdgeMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshToQuadEdgeMeshFilter_txx
#define __itkQuadEdgeMeshToQuadEdgeMeshFilter_txx

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::QuadEdgeMeshToQuadEdgeMeshFilter()
{
  this->Superclass::SetNumberOfRequiredInputs( 1 );
  this->Superclass::SetNumberOfRequiredOutputs( 1 );

  typename TInputMesh::Pointer out = TInputMesh::New();
  this->Superclass::SetNthOutput( 0, out.GetPointer() );
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void 
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::GenerateData()
{
  InputMeshConstPointer in = this->GetInput();
  OutputMeshPointer out = this->GetOutput();

  // Copy points
  InputPointsContainerConstIterator inIt = in->GetPoints()->Begin();
  while( inIt != in->GetPoints()->End() )
    {
    OutputPointType pOut;
    pOut.CastFrom( inIt.Value() );
    out->SetPoint( inIt.Index(), pOut );
    inIt++;
    } 

  // Copy cells
  InputCellsContainerConstIterator cIt = in->GetCells()->Begin();
  while( cIt != in->GetCells()->End() )
    {
    InputEdgeCellType* qe = (InputEdgeCellType*)0;
    InputPolygonCellType* pe = (InputPolygonCellType*)0;
    if( ( qe = dynamic_cast< InputEdgeCellType* >( cIt.Value() ) ) )
      {
      InputQEPrimal* QEGeom = qe->GetQEGeom( );
      out->AddEdgeWithSecurePointList( QEGeom->GetOrigin(), QEGeom->GetDestination() );
      }
    else
      {
      pe = dynamic_cast< InputPolygonCellType* >( cIt.Value());
      if( pe )
        {
        InputPointIdList points;
        InputPointsIdInternalIterator pit = pe->InternalPointIdsBegin();
        while( pit != pe->InternalPointIdsEnd( ) )
          {
          points.push_back( ( *pit ) );
          pit++;
          }
        out->AddFaceWithSecurePointList( points );
        } 
      }
    cIt++;
    }
}

} // end namespace itk

#endif 
