/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConformalFlatteningMeshFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConformalFlatteningMeshFilter_txx
#define __itkConformalFlatteningMeshFilter_txx

#include "itkConformalFlatteningMeshFilter.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"

#include <float.h>  // for DBL_MIN


namespace itk
{
/**
 *
 */
template <class TInputMesh, class TOutputMesh>
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::ConformalFlatteningMeshFilter()
{
  this->m_PolarCellIdentifier = 0;
  this->m_MapToSphere = false;

  this->m_MapScale = -1.0;
  // If during the stage when this parameter is used and it is still
  // -1.0, then it indicates that the user doesn't assign a scale
  // factor. Then automatically calculate it s.t. after doing the
  // stereo-graphic projection, upper and lowwer hemi-sphere will have
  // same number of vertics.
}

/**
 * Set the triangle used to define the boundary of the flattened region.
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter<TInputMesh, TOutputMesh>
::SetPolarCellIdentifier( CellIdentifier cellId )
{
  this->m_PolarCellIdentifier = cellId;
};

/**
 * Define the scale of the mapping. The largest coordinates of the
 * furthest point in the plane is m_MapScale.
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::SetScale( double scale )
{
  this->m_MapScale = scale;
};

/**
 * Define that the input surface will be mapped to a sphere
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::MapToSphere( void )
{
  this->m_MapToSphere = true;
};

/** Define that the input surface will be mapped to a plane.
 *  This skips the steps of the stereographic projection.
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::MapToPlane( void )
{
  this->m_MapToSphere = false;
};

/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

/**
 *
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::SetInput(TInputMesh *input)
{
  this->ProcessObject::SetNthInput(0, input);
}

/**
 * This method causes the filter to generate its output.
 */
template <class TInputMesh, class TOutputMesh>
void
ConformalFlatteningMeshFilter< TInputMesh, TOutputMesh >
::GenerateData(void)
{
  typedef typename TInputMesh::PointsContainer  InputPointsContainer;
  typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

  typedef typename TInputMesh::PointsContainerPointer
    InputPointsContainerPointer;
  typedef typename TOutputMesh::PointsContainerPointer
    OutputPointsContainerPointer;

  InputMeshPointer    inputMesh      =  this->GetInput();
  OutputMeshPointer   outputMesh     =  this->GetOutput();

  if( !inputMesh )
    {
    itkExceptionMacro(<<"Missing Input Mesh");
    }

  if( !outputMesh )
    {
    itkExceptionMacro(<<"Missing Output Mesh");
    }

  outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

  InputPointsContainerPointer  inPoints  = inputMesh->GetPoints();
  OutputPointsContainerPointer outPoints = outputMesh->GetPoints();

  const unsigned int numberOfPoints = inputMesh->GetNumberOfPoints();

  outPoints->Reserve( numberOfPoints );
  outPoints->Squeeze();  // in case the previous mesh had
                         // allocated a larger memory
                  
  unsigned int i;

  SparseMatrixCoordType D(numberOfPoints,numberOfPoints);

  VectorCoordType bx(numberOfPoints, 0.0);
  VectorCoordType by(numberOfPoints, 0.0);

  itkDebugMacro("m_PolarCellIdentifier " << this->m_PolarCellIdentifier);

  CellAutoPointer cell;
  inputMesh->GetCell( this->m_PolarCellIdentifier, cell );

  unsigned int cellNumberOfPoints = cell->GetNumberOfPoints();

  if( cellNumberOfPoints != 3 )
    {
    itkExceptionMacro("Polar cell has " << cellNumberOfPoints << " points"
        "\nThis filter can only process triangle meshes. "
        "Use vtkTriangleFilter to convert your mesh to a triangle mesh.");
    return;
    }

  PointIdIterator pointIditer = cell->PointIdsBegin();

  unsigned int boundaryId0 = *pointIditer;
  pointIditer++;
  unsigned int boundaryId1 = *pointIditer;
  pointIditer++;
  unsigned int boundaryId2 = *pointIditer;

  InputPointType ptA; ptA.Fill(0.0);
  InputPointType ptB; ptB.Fill(0.0);
  InputPointType ptC; ptC.Fill(0.0);

  inputMesh->GetPoint( boundaryId0, &ptA );
  inputMesh->GetPoint( boundaryId1, &ptB );
  inputMesh->GetPoint( boundaryId2, &ptC );

  double AB[3];
  double BC[3];
  double CA[3];

  double normAB2;
  double normBC2;
  double normCA2;

  double normBC;
  double normCA;

  double prodABBC;
  double prodBCCA;
  double prodCAAB;

  AB[0] = ptB[0] - ptA[0];
  AB[1] = ptB[1] - ptA[1];
  AB[2] = ptB[2] - ptA[2];

  BC[0] = ptC[0] - ptB[0];
  BC[1] = ptC[1] - ptB[1];
  BC[2] = ptC[2] - ptB[2];

  CA[0] = ptA[0] - ptC[0];
  CA[1] = ptA[1] - ptC[1];
  CA[2] = ptA[2] - ptC[2];

  normAB2 = AB[0] * AB[0] + AB[1] * AB[1] + AB[2] * AB[2];

  if( normAB2 < 1e-6 )
    {
    itkExceptionMacro("||AB||^2 = " << normAB2
        << "\nRisk of division by zero");
    return;
    }

  double E[3];
  double CE[3];

  prodCAAB = CA[0] * AB[0] + CA[1] * AB[1] + CA[2] * AB[2];

  // E = projection of C onto AB orthogonal to AB.
  // t = Parameter to find E = A + t * ( B - A ).
  //
  // If t = 0.0,  E = A.
  // If t = 1.0,  E = B.
  //
  // |AC| * cos(alpha) = t * |AB|
  // AB * AC = |AB| |AC| cos(alpha)
  //
  // t = (|AC| / |AB|)  *  ((AB * AC) / (|AB| * |AC|))
  //   = (AB * AC) / (|AB| * |AB|)

  double t = -prodCAAB / normAB2;

  E[0] = ptA[0] + t * AB[0];
  E[1] = ptA[1] + t * AB[1];
  E[2] = ptA[2] + t * AB[2];

  CE[0] = ptC[0] - E[0];
  CE[1] = ptC[1] - E[1];
  CE[2] = ptC[2] - E[2];

  double normCE2 = CE[0] * CE[0] + CE[1] * CE[1] + CE[2] * CE[2];

  double normAB = vcl_sqrt(normAB2);
  double normCE = vcl_sqrt(normCE2);

  itkDebugMacro("scale " << this->m_MapScale);

  double tmp = 2.0 / normAB;
  //double factor = normAB / normCE;

  bx( boundaryId0 ) = - tmp; // -t * factor;
  bx( boundaryId1 ) = tmp; // (1.0 - t) * factor;

  double tmp2 = 2.0 / normCE;

  by( boundaryId0 ) = tmp2 * (1.0 - t); // 0.0;
  by( boundaryId1 ) = tmp2 * t; // 0.0;
  by( boundaryId2 ) = - tmp2; // 1.0;

  CellIterator cellIterator = inputMesh->GetCells()->Begin();
  CellIterator cellEnd      = inputMesh->GetCells()->End();

  PointIdentifier ptIdA;
  PointIdentifier ptIdB;
  PointIdentifier ptIdC;

  double cosABC;
  double cosBCA;
  double cosCAB;

  double sinABC;
  double sinBCA;
  double sinCAB;

  double cotgABC;
  double cotgBCA;
  double cotgCAB;

  while( cellIterator != cellEnd )
    {
    CellType * aCell = cellIterator.Value();
    unsigned int aCellNumberOfPoints = aCell->GetNumberOfPoints();

    if( aCellNumberOfPoints > 3 )
      {
      itkExceptionMacro("cell has " << aCellNumberOfPoints << " points\n"
      "This filter can only process triangle meshes.");
      return;
      }

    while( aCellNumberOfPoints < 3 ) // leave the edges and points untouched
      {
      cellIterator++;
      if( cellIterator !=cellEnd )
        {
        aCell = cellIterator.Value();
        aCellNumberOfPoints = aCell->GetNumberOfPoints();
        }
      }
    if( cellIterator == cellEnd ) break;

    pointIditer = aCell->PointIdsBegin();

    ptIdA = *pointIditer;
    pointIditer++;

    ptIdB = *pointIditer;
    pointIditer++;

    ptIdC = *pointIditer;

    inputMesh->GetPoint( ptIdA, &ptA );
    inputMesh->GetPoint( ptIdB, &ptB );
    inputMesh->GetPoint( ptIdC, &ptC );

    AB[0] = ptB[0] - ptA[0];
    AB[1] = ptB[1] - ptA[1];
    AB[2] = ptB[2] - ptA[2];

    BC[0] = ptC[0] - ptB[0];
    BC[1] = ptC[1] - ptB[1];
    BC[2] = ptC[2] - ptB[2];

    CA[0] = ptA[0] - ptC[0];
    CA[1] = ptA[1] - ptC[1];
    CA[2] = ptA[2] - ptC[2];

    normAB2 = AB[0] * AB[0] + AB[1] * AB[1] + AB[2] * AB[2];
    normBC2 = BC[0] * BC[0] + BC[1] * BC[1] + BC[2] * BC[2];
    normCA2 = CA[0] * CA[0] + CA[1] * CA[1] + CA[2] * CA[2];

    if( normAB2 < 1e-6 )
      {
      itkExceptionMacro("normAB2 " << normAB2);
      return;
      }

    if( normBC2 < 1e-6 )
      {
      itkExceptionMacro("normBC2 " << normBC2);
      return;
      }

    if( normCA2 < 1e-6 )
      {
      itkExceptionMacro("normCA2 " << normCA2);
      return;
      }

    normAB = vcl_sqrt( normAB2 );
    normBC = vcl_sqrt( normBC2 );
    normCA = vcl_sqrt( normCA2 );

    prodABBC = AB[0] * BC[0] + AB[1] * BC[1] + AB[2] * BC[2];
    prodBCCA = BC[0] * CA[0] + BC[1] * CA[1] + BC[2] * CA[2];
    prodCAAB = CA[0] * AB[0] + CA[1] * AB[1] + CA[2] * AB[2];

    cosABC = -prodABBC / ( normAB * normBC );
    cosBCA = -prodBCCA / ( normBC * normCA );
    cosCAB = -prodCAAB / ( normCA * normAB );

    if( cosABC <= -1.0 || cosABC >= 1.0 )
      {
      itkExceptionMacro("cosABC= " << cosABC);
      return;
      }

    if( cosBCA <= -1.0 || cosBCA >= 1.0 )
      {
      itkExceptionMacro("cosBCA= " << cosBCA);
      return;
      }

    if( cosCAB <= -1.0 || cosCAB >= 1.0 )
      {
      itkExceptionMacro("cosCAB= " << cosCAB);
      return;
      }

    sinABC = vcl_sqrt( 1.0 - cosABC * cosABC );
    sinBCA = vcl_sqrt( 1.0 - cosBCA * cosBCA );
    sinCAB = vcl_sqrt( 1.0 - cosCAB * cosCAB );

    if( sinABC < 1e-6 )
      {
      itkExceptionMacro("sinABC= " << sinABC);
      return;
      }

    if( sinBCA < 1e-6 )
      {
      itkExceptionMacro("sinBCA= " << sinBCA);
      return;
      }

    if( sinCAB < 1e-6 )
      {
      itkExceptionMacro("sinCAB= " << sinCAB);
      return;
      }

    cotgABC = cosABC / sinABC;
    cotgBCA = cosBCA / sinBCA;
    cotgCAB = cosCAB / sinCAB;

    D( ptIdA, ptIdA ) += cotgABC + cotgBCA;
    D( ptIdA, ptIdB ) -= cotgBCA;
    D( ptIdA, ptIdC ) -= cotgABC;

    D( ptIdB, ptIdB ) += cotgBCA + cotgCAB;
    D( ptIdB, ptIdA ) -= cotgBCA;
    D( ptIdB, ptIdC ) -= cotgCAB;

    D( ptIdC, ptIdC ) += cotgCAB + cotgABC;
    D( ptIdC, ptIdB ) -= cotgCAB;
    D( ptIdC, ptIdA ) -= cotgABC;

    cellIterator++;
    }


  VectorCoordType x(numberOfPoints, 0.0);
  VectorCoordType y(numberOfPoints, 0.0);
  {
  // solving Ax = b (D x = bx)
  VectorCoordType Dx;
  D.pre_mult(x, Dx);

  VectorCoordType Dy;
  D.pre_mult(y, Dy);


  VectorCoordType rx = bx - Dx;
  VectorCoordType px = rx;

  VectorCoordType ry = by - Dy;
  VectorCoordType py = ry;


  unsigned int numIter = bx.size();
  numIter += numIter/10; // let the iteration times a little more than the dimension

  double tol = 1e-10;
  
  for ( i = 0; i <= numIter; ++i)
    {
    VectorCoordType Dpx;
    D.pre_mult(px, Dpx);
    VectorCoordType Dpy;
    D.pre_mult(py, Dpy);
 
    double pDpx = inner_product(px, Dpx);
    double pDpy = inner_product(py, Dpy);
    
    double alphax = inner_product(px, rx)/(pDpx + DBL_MIN);
    double alphay = inner_product(py, ry)/(pDpy + DBL_MIN);

    x += alphax*px;
    y += alphay*py;

    rx -= alphax*Dpx;
    ry -= alphay*Dpy;

    if ((inner_product(rx, rx) < tol) && (inner_product(ry, ry) < tol))
      {
      break;
      }

    VectorCoordType Drx;
    D.pre_mult(rx, Drx);
    VectorCoordType Dry;
    D.pre_mult(ry, Dry);

    double betax = -inner_product(px, Drx)/(pDpx + DBL_MIN);
    double betay = -inner_product(py, Dry)/(pDpy + DBL_MIN);

    px = rx + betax*px;
    py = ry + betay*py;
    }
  }

  typename OutputPointsContainer::Iterator      outputPointIterator =
    outPoints->Begin();
  typename OutputPointsContainer::Iterator      outputPointEnd =
    outPoints->End();

  OutputPointType point;
  point[2] = 0.0;

  double bounds[6];

  bounds[0] = vcl_numeric_limits<double>::max();
  bounds[1] = -vcl_numeric_limits<double>::max();

  bounds[2] = vcl_numeric_limits<double>::max();
  bounds[3] = -vcl_numeric_limits<double>::max();

  bounds[4] = vcl_numeric_limits<double>::max();
  bounds[5] = -vcl_numeric_limits<double>::max();

  if( this->m_MapToSphere )
    {
    if (m_MapScale < 0)
      {
      // < 0 means user doesn't explictly assign it. Then
      // automatically calculate it s.t. after doing the
      // stereo-graphic projection, upper and lowwer hemi-sphere will have
      // same number of vertics.

      std::vector<double> v_r2(numberOfPoints);
      std::vector<double>::iterator itv_r2=v_r2.begin();
    
      for (i = 0; i < numberOfPoints;  ++i, ++itv_r2) 
        {
          *itv_r2 = x(i)*x(i) + y(i)*y(i);
        }
          
      std::sort(v_r2.begin(), v_r2.end());
      unsigned int uiMidPointIdx = 0;
      if( numberOfPoints % 2 )
        {
        uiMidPointIdx = (numberOfPoints-1)/2;
        }
      else
        {
        uiMidPointIdx = numberOfPoints/2;
        }
      this->m_MapScale = 1.0/sqrt(v_r2[uiMidPointIdx]);
      }

    i = 0;
    while( outputPointIterator != outputPointEnd )
      {
      double xx = (this->m_MapScale) * x(i);
      double yy = (this->m_MapScale) * y(i);

      double radius2 = xx*xx + yy*yy;

      point[0] = 2.0*xx/(1.0 + radius2);
      point[1] = 2.0*yy/(1.0 + radius2);
      point[2] = 2.0*radius2/(1.0 + radius2) - 1.0;

      if( point[0] < bounds[0] ) { bounds[0] = point[0]; }
      if( point[0] > bounds[1] ) { bounds[1] = point[0]; }

      if( point[1] < bounds[2] ) { bounds[2] = point[1]; }
      if( point[1] > bounds[3] ) { bounds[3] = point[1]; }

      if( point[2] < bounds[4] ) { bounds[4] = point[2]; }
      if( point[2] > bounds[5] ) { bounds[5] = point[2]; }

      outputPointIterator.Value() = point;
      outputPointIterator++;
      i++;
      }
    }
  else
    {
    i = 0;
    while( outputPointIterator != outputPointEnd )
      {
      point[0] = x(i);
      point[1] = y(i);

      if( point[0] < bounds[0] ) { bounds[0] = point[0]; }
      if( point[0] > bounds[1] ) { bounds[1] = point[0]; }

      if( point[1] < bounds[2] ) { bounds[2] = point[1]; }
      if( point[1] > bounds[3] ) { bounds[3] = point[1]; }

      if( point[2] < bounds[4] ) { bounds[4] = point[2]; }
      if( point[2] > bounds[5] ) { bounds[5] = point[2]; }

      outputPointIterator.Value() = point;
      outputPointIterator++;
      i++;
      }
    }

  itkDebugMacro("bounds"
    << " " << bounds[0] << " " << bounds[1]
    << " " << bounds[2] << " " << bounds[3]
    << " " << bounds[4] << " " << bounds[5]);

  //Create duplicate references to the rest of data on the mesh

  outputMesh->SetPointData( inputMesh->GetPointData() );
  outputMesh->SetCellLinks( inputMesh->GetCellLinks() );
  outputMesh->SetCells( inputMesh->GetCells() );
  outputMesh->SetCellData( inputMesh->GetCellData() );

  unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

  for( unsigned int dim = 0; dim < maxDimension; dim++ )
    {
    outputMesh->SetBoundaryAssignments( dim,
        inputMesh->GetBoundaryAssignments(dim) );
    }
}

} // end namespace itk

#endif
