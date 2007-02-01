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
#ifndef _itkConformalFlatteningMeshFilter_txx
#define _itkConformalFlatteningMeshFilter_txx

#include "itkConformalFlatteningMeshFilter.h"
#include "itkExceptionObject.h"

#include "vnl/vnl_math.h"
#include "vcl_algorithm.h"

#include <vnl/vnl_cost_function.h>
#include <vnl/algo/vnl_conjugate_gradient.h>

namespace itk
{

class ConformalFlatteningFunction :  public vnl_cost_function
  {
  public:
    typedef vnl_vector<double>        VectorType;
    typedef vnl_sparse_matrix<double> MatrixType;

    ConformalFlatteningFunction( MatrixType const& A, VectorType const& b );

    double f(const VectorType& x);

    void gradf(const VectorType& x, VectorType& g);

    inline unsigned int GetDimension() { return m_Dimension; }

  private:
    MatrixType const* m_Matrix;
    VectorType const* m_Vector;
    unsigned int m_Dimension;
};

ConformalFlatteningFunction
::ConformalFlatteningFunction(vnl_sparse_matrix<double> const& A,
    VectorType const& b) : vnl_cost_function(b.size())
{
  this->m_Matrix = &A;
  this->m_Vector = &b;
  this->m_Dimension = b.size();

  if( A.rows() != b.size() )
    {
    itk::ExceptionObject excp(__FILE__,__LINE__,
    "The # of rows in A must be the same as the length of b");
    throw excp;
    }
}

double
ConformalFlatteningFunction
::f(const VectorType& x)
{
  VectorType tmp;
  this->m_Matrix->pre_mult(x, tmp);
  return 0.5 * inner_product(tmp,x) - inner_product(*this->m_Vector,x);
}

void
ConformalFlatteningFunction
::gradf(const VectorType& x, VectorType& gradient)
{
  VectorType tmp;
  this->m_Matrix->mult(x, tmp);
  gradient = tmp - *this->m_Vector;
}

/**
 *
 */
template <class TPixelType>
ConformalFlatteningMeshFilter<TPixelType>
::ConformalFlatteningMeshFilter()
{
  this->m_PolarCellIdentifier = 0;
  this->m_MapToSphere = false;
  this->m_MapScale = 1.0;
}
/**
 * Define the scale of the mapping. The largest coordinates of the
 * furthest point in the plane is m_MapScale.
 */
template <class TPixelType>
void ConformalFlatteningMeshFilter<TPixelType>
::SetScale( double scale )
{
  this->m_MapScale = scale;
};

/**
 * Define that the input surface will be mapped to a sphere
 */
template <class TPixelType>
void ConformalFlatteningMeshFilter<TPixelType>
::MapToSphere( void )
{
  this->m_MapToSphere = true;
};

/** Define that the input surface will be mapped to a plane.
 *  This skips the steps of the stereographic projection.
 */
template <class TPixelType>
void ConformalFlatteningMeshFilter<TPixelType>
::MapToPlane( void )
{
  this->m_MapToSphere = false;
};

/**
 *
 */
template <class TPixelType>
void
ConformalFlatteningMeshFilter<TPixelType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

/**
 *
 */
template <class TPixelType>
void
ConformalFlatteningMeshFilter<TPixelType>
::SetInput(TInputMesh *input)
{
  this->ProcessObject::SetNthInput(0, input);
}

/**
 * This method causes the filter to generate its output.
 */
template <class TPixelType>
void
ConformalFlatteningMeshFilter<TPixelType>
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

  SparseMatrixCoordType D(numberOfPoints,numberOfPoints);

  VectorCoordType x(numberOfPoints, 0.0);
  VectorCoordType y(numberOfPoints, 0.0);
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
  PointIdIterator pointIdend  = cell->PointIdsEnd();

  unsigned int boundaryId0 = *pointIditer;
  pointIditer++;
  unsigned int boundaryId1 = *pointIditer;
  pointIditer++;
  unsigned int boundaryId2 = *pointIditer;
  pointIditer++;

  InputPointType ptA;
  InputPointType ptB;
  InputPointType ptC;

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
    CellType * cell = cellIterator.Value();
    unsigned int cellNumberOfPoints = cell->GetNumberOfPoints();

    if( cellNumberOfPoints != 3 )
      {
      itkExceptionMacro("cell has " << cellNumberOfPoints << " points\n"
      "This filter can only process triangle meshes.");
      return;
      }

    PointIdIterator pointIditer = cell->PointIdsBegin();
    PointIdIterator pointIdend  = cell->PointIdsEnd();

    ptIdA = *pointIditer;
    pointIditer++;

    ptIdB = *pointIditer;
    pointIditer++;

    ptIdC = *pointIditer;
    pointIditer++;

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

  ConformalFlatteningFunction functionX(D,bx);
  vnl_conjugate_gradient conjugateGradientX(functionX);

  conjugateGradientX.minimize(x);
  itkDebugMacro("Conjugate Gradient min of " << functionX.f(x));
  //itkDebugMacro("x " << x );
  conjugateGradientX.diagnose_outcome();

  ConformalFlatteningFunction functionY(D,by);
  vnl_conjugate_gradient conjugateGradientY(functionY);

  conjugateGradientY.minimize(y);
  itkDebugMacro("Conjugate Gradient min of " << functionY.f(y));
  //itkDebugMacro("y " << y );
  conjugateGradientY.diagnose_outcome();

  typename OutputPointsContainer::Iterator      outputPointIterator =
    outPoints->Begin();
  typename OutputPointsContainer::Iterator      outputPointEnd =
    outPoints->End();

  typedef typename OutputMeshType::PointType OutputPointType;

  OutputPointType point;
  point[2] = 0.0;

  double bounds[6];

  bounds[0] = vcl_numeric_limits<double>::max();
  bounds[1] = -vcl_numeric_limits<double>::max();

  bounds[2] = vcl_numeric_limits<double>::max();
  bounds[3] = -vcl_numeric_limits<double>::max();

  bounds[4] = vcl_numeric_limits<double>::max();
  bounds[5] = -vcl_numeric_limits<double>::max();

  unsigned int i=0;

  if( this->m_MapToSphere )
    {
    while( outputPointIterator != outputPointEnd )
      {
      double radius2 = x(i) * x(i) + y(i) * y(i);

      point[0] = 2.0 * this->m_MapScale * x(i) / (1.0 + radius2);
      point[1] = 2.0 * this->m_MapScale * y(i) / (1.0 + radius2);
      point[2] = 2.0 * this->m_MapScale * radius2 / (1.0 + radius2) - 1.0;

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
    while( outputPointIterator != outputPointEnd )
      {
      point[0] = this->m_MapScale * x(i);
      point[1] = this->m_MapScale * y(i);

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
