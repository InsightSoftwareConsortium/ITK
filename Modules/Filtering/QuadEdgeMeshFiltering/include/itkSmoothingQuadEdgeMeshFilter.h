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
#ifndef __itkSmoothingQuadEdgeMeshFilter_h
#define __itkSmoothingQuadEdgeMeshFilter_h

#include "itkDelaunayConformingQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class SmoothingQuadEdgeMeshFilter
 * \brief Quad Edge Mesh Smoothing Filter
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TInputMesh, typename TOutputMesh=TInputMesh >
class SmoothingQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef SmoothingQuadEdgeMeshFilter                                       Self;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh > Superclass;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SmoothingQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef TOutputMesh                                      OutputMeshType;
  typedef typename OutputMeshType::Pointer                 OutputMeshPointer;
  typedef typename OutputMeshType::EdgeCellType            OutputEdgeCellType;
  typedef typename OutputMeshType::PolygonCellType         OutputPolygonCellType;
  typedef typename OutputMeshType::QEType                  OutputQEType;
  typedef typename OutputMeshType::PointIdentifier         OutputPointIdentifier;
  typedef typename OutputMeshType::PointType               OutputPointType;
  typedef typename OutputPointType::VectorType             OutputVectorType;
  typedef typename OutputPointType::CoordRepType           OutputCoordType;
  typedef typename OutputMeshType::PointsContainer         OutputPointsContainer;
  typedef typename OutputMeshType::PointsContainerPointer  OutputPointsContainerPointer;
  typedef typename OutputMeshType::PointsContainerIterator OutputPointsContainerIterator;
  typedef typename OutputMeshType::CellsContainerPointer   OutputCellsContainerPointer;
  typedef typename OutputMeshType::CellsContainerIterator  OutputCellsContainerIterator;

  itkStaticConstMacro(PointDimension, unsigned int, OutputMeshType::PointDimension);

  typedef DelaunayConformingQuadEdgeMeshFilter< InputMeshType, OutputMeshType > InputOutputDelaunayConformingType;
  typedef typename InputOutputDelaunayConformingType::Pointer                   InputOutputDelaunayConformingPointer;

  typedef DelaunayConformingQuadEdgeMeshFilter< OutputMeshType, OutputMeshType > OutputDelaunayConformingType;
  typedef typename OutputDelaunayConformingType::Pointer                         OutputDelaunayConformingPointer;

  typedef MatrixCoefficients< OutputMeshType > CoefficientsComputation;

  void SetCoefficientsMethod(CoefficientsComputation *iMethod)
  { m_CoefficientsMethod = iMethod; }

  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

  itkSetMacro(DelaunayConforming, bool);
  itkGetConstMacro(DelaunayConforming, bool);

  itkSetMacro(RelaxationFactor, OutputCoordType);
  itkGetConstMacro(RelaxationFactor, OutputCoordType);

protected:
  SmoothingQuadEdgeMeshFilter();
  ~SmoothingQuadEdgeMeshFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  CoefficientsComputation *m_CoefficientsMethod;

  InputOutputDelaunayConformingPointer m_InputDelaunayFilter;

  OutputDelaunayConformingPointer m_OutputDelaunayFilter;

  bool m_DelaunayConforming;

  unsigned int m_NumberOfIterations;

  OutputCoordType m_RelaxationFactor;

  void GenerateData();

private:
  SmoothingQuadEdgeMeshFilter(const Self &);
  void operator=(const Self &);
};
}

#include "itkSmoothingQuadEdgeMeshFilter.hxx"
#endif
