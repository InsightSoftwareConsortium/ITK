/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshSmoothing.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshSmoothing_h
#define __itkQuadEdgeMeshSmoothing_h

#include <itkQuadEdgeMeshToQuadEdgeMeshFilter.h>
#include "itkQuadEdgeMeshDelaunayConformingFilter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class QuadEdgeMeshSmoothing
 * \brief Quad Edge Mesh Smoothing Filter
 */
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshSmoothing :
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshSmoothing       Self;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
                                      Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshSmoothing, QuadEdgeMeshToQuadEdgeMeshFilter );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutputMesh                               OutputMeshType;
  typedef typename OutputMeshType::Pointer          OutputMeshPointer;
  typedef typename OutputMeshType::EdgeCellType     OutputEdgeCellType;
  typedef typename OutputMeshType::PolygonCellType  OutputPolygonCellType;
  typedef typename OutputMeshType::QEType           OutputQEType;
  typedef typename OutputMeshType::PointIdentifier  OutputPointIdentifier;
  typedef typename OutputMeshType::PointType        OutputPointType;
  typedef typename OutputPointType::VectorType      OutputVectorType;
  typedef typename OutputPointType::CoordRepType    OutputCoordType;
  typedef typename OutputMeshType::PointsContainer  OutputPointsContainer;
  typedef typename OutputMeshType::PointsContainerPointer
    OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator
    OutputPointsContainerIterator;
  typedef typename OutputMeshType::CellsContainerPointer
    OutputCellsContainerPointer;
  typedef typename OutputMeshType::CellsContainerIterator
    OutputCellsContainerIterator;

  itkStaticConstMacro( PointDimension, unsigned int,
                       OutputMeshType::PointDimension );

  typedef QuadEdgeMeshDelaunayConformingFilter< InputMeshType, OutputMeshType >
    InputOutputDelaunayConformingType;
  typedef typename InputOutputDelaunayConformingType::Pointer
    InputOutputDelaunayConformingPointer;

  typedef QuadEdgeMeshDelaunayConformingFilter< OutputMeshType, OutputMeshType >
    OutputDelaunayConformingType;
  typedef typename OutputDelaunayConformingType::Pointer
    OutputDelaunayConformingPointer;

  typedef MatrixCoefficients< OutputMeshType > CoefficientsComputation;

  void SetCoefficientsMethod( CoefficientsComputation* iMethod )
    { m_CoefficientsMethod = iMethod; }

  itkSetMacro( NumberOfIterations, unsigned int );
  itkGetConstMacro( NumberOfIterations, unsigned int );

  itkSetMacro( DelaunayConforming, bool );
  itkGetConstMacro( DelaunayConforming, bool );

  itkSetMacro( RelaxationFactor, OutputCoordType );
  itkGetConstMacro( RelaxationFactor, OutputCoordType );

protected:
  QuadEdgeMeshSmoothing();
  ~QuadEdgeMeshSmoothing();

  CoefficientsComputation*              m_CoefficientsMethod;
  InputOutputDelaunayConformingPointer  m_InputDelaunayFilter;
  OutputDelaunayConformingPointer       m_OutputDelaunayFilter;
  bool                                  m_DelaunayConforming;
  unsigned int                          m_NumberOfIterations;
  OutputCoordType                       m_RelaxationFactor;

  void GenerateData();

private:
  QuadEdgeMeshSmoothing( const Self& );
  void operator = ( const Self& );
};
}

#include "itkQuadEdgeMeshSmoothing.txx"
#endif
