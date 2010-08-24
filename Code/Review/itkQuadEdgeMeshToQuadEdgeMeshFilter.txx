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
  this->Superclass::SetNumberOfRequiredInputs(1);
  this->Superclass::SetNumberOfRequiredOutputs(1);

  this->Superclass::SetNthOutput( 0, OutputMeshType::New() );
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyMeshToMesh(const TInputMesh *in, TOutputMesh *out)
{
  CopyMeshToMeshPoints(in, out);
  CopyMeshToMeshEdgeCells(in, out);
  CopyMeshToMeshCells(in, out);
  CopyMeshToMeshPointData(in, out);
  CopyMeshToMeshCellData(in, out);
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMesh()
{
  this->CopyInputMeshToOutputMeshGeometry();
  this->CopyInputMeshToOutputMeshFieldData();
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshGeometry()
{
  this->CopyInputMeshToOutputMeshPoints();
  this->CopyInputMeshToOutputMeshEdgeCells();
  this->CopyInputMeshToOutputMeshCells();
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshFieldData()
{
  this->CopyInputMeshToOutputMeshPointData();
  this->CopyInputMeshToOutputMeshCellData();
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshPoints()
{
  const InputMeshType *in = this->GetInput();
  OutputMeshType *     out = this->GetOutput();

  CopyMeshToMeshPoints(in, out);
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshEdgeCells()
{
  const InputMeshType *in = this->GetInput();
  OutputMeshType *     out = this->GetOutput();

  CopyMeshToMeshEdgeCells(in, out);
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCells()
{
  const InputMeshType *in = this->GetInput();
  OutputMeshType *     out = this->GetOutput();

  CopyMeshToMeshCells(in, out);
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshPointData()
{
  const InputMeshType *in = this->GetInput();
  OutputMeshType *     out = this->GetOutput();

  CopyMeshToMeshPointData(in, out);
}

// ---------------------------------------------------------------------
template< class TInputMesh, class TOutputMesh >
void
QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::CopyInputMeshToOutputMeshCellData()
{
  const InputMeshType *in = this->GetInput();
  OutputMeshType *     out = this->GetOutput();

  CopyMeshToMeshCellData(in, out);
}
} // end namespace itk

#endif
