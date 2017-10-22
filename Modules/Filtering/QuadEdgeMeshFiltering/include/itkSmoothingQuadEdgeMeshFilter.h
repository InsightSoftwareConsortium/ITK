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
#ifndef itkSmoothingQuadEdgeMeshFilter_h
#define itkSmoothingQuadEdgeMeshFilter_h

#include "itkDelaunayConformingQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/** \class SmoothingQuadEdgeMeshFilter
 *
 * \brief QuadEdgeMesh Smoothing Filter
 *
 * This filter adjusts point coordinates using Laplacian smoothing. The
 * effect is to "relax" the mesh, making the cells better shaped and the
 * vertices more evenly distributed.
 *
 * For one iteration the location of one vertex is computed as follows:
 * \f[
 * \boldsymbol{ v' }_i = v_i + m_RelaxationFactor \cdot \frac{ \sum_j w_{ij} ( \boldsymbol{ v_j } - \boldsymbol{ v_i } ) }{ \sum_j w_{ij} }
 * \f]
 *
 * where \f$ w_{ij} \f$ is computed by the means of the set functor
 * CoefficientsComputation
 *
 * This process is then repeated for m_NumberOfIterations (the more iterations,
 * the smoother the output mesh will be).
 *
 * At each iteration, one can run DelaunayConformingQuadEdgeMeshFilter
 * resulting a more regular (in terms of connectivity) and smoother mesh.
 * Depending on the mesh size and configuration it could be an expensive
 * process to run it at each iterations, especially if the number of iterations
 * is large.  Note that one can still run N iterations without
 * DelaunayConformingQuadEdgeMeshFilter, then run this filter and apply this
 * process M times.
 *
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */

template< typename TInputMesh, typename TOutputMesh=TInputMesh >
class ITK_TEMPLATE_EXPORT SmoothingQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef SmoothingQuadEdgeMeshFilter                                 Self;
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

  typedef MatrixCoefficients< OutputMeshType > CoefficientsComputation;

  void SetCoefficientsMethod(CoefficientsComputation *iMethod);

  /** Set/Get the number of iterations */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

  /** Set/Get if DelaunayConformingQuadEdgeMeshFilter is used at the end of each iterations */
  itkSetMacro(DelaunayConforming, bool);
  itkGetConstMacro(DelaunayConforming, bool);

  /** Set/Get relaxation factor applied for each iteration */
  itkSetMacro(RelaxationFactor, OutputCoordType);
  itkGetConstMacro(RelaxationFactor, OutputCoordType);

protected:
  SmoothingQuadEdgeMeshFilter();
  ~SmoothingQuadEdgeMeshFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  CoefficientsComputation *m_CoefficientsMethod;

  typedef DelaunayConformingQuadEdgeMeshFilter< InputMeshType, OutputMeshType > InputOutputDelaunayConformingType;
  typedef typename InputOutputDelaunayConformingType::Pointer                   InputOutputDelaunayConformingPointer;

  InputOutputDelaunayConformingPointer m_InputDelaunayFilter;

  typedef DelaunayConformingQuadEdgeMeshFilter< OutputMeshType, OutputMeshType > OutputDelaunayConformingType;
  typedef typename OutputDelaunayConformingType::Pointer                         OutputDelaunayConformingPointer;

  OutputDelaunayConformingPointer m_OutputDelaunayFilter;

  bool m_DelaunayConforming;

  unsigned int m_NumberOfIterations;

  OutputCoordType m_RelaxationFactor;

  void GenerateData() ITK_OVERRIDE;

private:
  SmoothingQuadEdgeMeshFilter(const Self &);
  void operator=(const Self &);
};
}

#include "itkSmoothingQuadEdgeMeshFilter.hxx"
#endif
