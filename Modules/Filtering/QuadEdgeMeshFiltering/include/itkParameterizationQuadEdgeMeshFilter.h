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
#ifndef itkParameterizationQuadEdgeMeshFilter_h
#define itkParameterizationQuadEdgeMeshFilter_h

#include "itkBorderQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 *  \class ParameterizationQuadEdgeMeshFilter
 *
 *  \brief Compute a planar parameterization of the input mesh.
 *  \note Here we have only implemented some parameterizations with fixed
 *        boundary.
 * \ingroup ITKQuadEdgeMeshFiltering
 *
 * \wiki
 * \wikiexample{Meshes/QuadEdgeMeshParameterizationFilter,Planar parameterization of a mesh}
 * \endwiki
 */
template< typename TInputMesh, typename TOutputMesh, typename TSolverTraits >
class ParameterizationQuadEdgeMeshFilter:
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  /** Basic types. */
  typedef ParameterizationQuadEdgeMeshFilter              Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh,
                                            TOutputMesh > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Input types. */
  typedef TInputMesh                              InputMeshType;
  typedef typename InputMeshType::Pointer         InputMeshPointer;
  typedef typename InputMeshType::ConstPointer    InputMeshConstPointer;
  typedef typename InputMeshType::CoordRepType    InputCoordRepType;
  typedef typename InputMeshType::PointType       InputPointType;
  typedef typename InputPointType::VectorType     InputPointVectorType;
  typedef typename InputMeshType::PointIdentifier InputPointIdentifier;
  typedef typename InputMeshType::QEType          InputQEType;
  typedef typename InputMeshType::VectorType      InputVectorType;
  typedef typename InputMeshType::EdgeListType    InputEdgeListType;
  typedef typename InputMeshType::PixelType       InputPixelType;
  typedef typename InputMeshType::Traits          InputTraits;

  itkStaticConstMacro(InputVDimension, unsigned int, InputMeshType::PointDimension);

  typedef typename InputMeshType::PointsContainer               InputPointsContainer;
  typedef typename InputMeshType::PointsContainerConstIterator  InputPointsContainerConstIterator;

  typedef typename InputMeshType::CellsContainerConstIterator   InputCellsContainerConstIterator;
  typedef typename InputMeshType::EdgeCellType                  InputEdgeCellType;
  typedef typename InputMeshType::PolygonCellType               InputPolygonCellType;
  typedef typename InputMeshType::PointIdList                   InputPointIdList;

  typedef typename InputQEType::IteratorGeom                      InputQEIterator;

  typedef std::map< InputPointIdentifier, InputPointIdentifier >  InputMapPointIdentifier;
  typedef typename InputMapPointIdentifier::iterator              InputMapPoinIdentifierIterator;

  /** Output types. */
  typedef TOutputMesh                                      OutputMeshType;
  typedef typename OutputMeshType::Pointer                 OutputMeshPointer;
  typedef typename OutputMeshType::ConstPointer            OutputMeshConstPointer;
  typedef typename OutputMeshType::CoordRepType            OutputCoordRepType;
  typedef typename OutputMeshType::PointType               OutputPointType;
  typedef typename OutputMeshType::PointIdentifier         OutputPointIdentifier;
  typedef typename OutputMeshType::QEType                  OutputQEType;
  typedef typename OutputMeshType::VectorType              OutputVectorType;
  typedef typename OutputQEType::IteratorGeom              OutputQEIterator;
  typedef typename OutputMeshType::PointsContainerIterator OutputPointsContainerIterator;

  itkStaticConstMacro(OutputVDimension, unsigned int, OutputMeshType::PointDimension);

  typedef TSolverTraits                     SolverTraits;
  typedef typename SolverTraits::ValueType  ValueType;
  typedef typename SolverTraits::MatrixType MatrixType;
  typedef typename SolverTraits::VectorType VectorType;

  typedef BorderQuadEdgeMeshFilter< InputMeshType, InputMeshType >  MeshBorderTransform;
  typedef typename MeshBorderTransform::Pointer                     MeshBorderTransformPointer;

  typedef MatrixCoefficients< InputMeshType >                       CoefficientsComputation;

public:

  void SetCoefficientsMethod(CoefficientsComputation *iMethod)
  {
    this->m_CoefficientsMethod = iMethod;
  }

  itkNewMacro(Self);
  itkTypeMacro(ParameterizationQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

  itkSetObjectMacro(BorderTransform, MeshBorderTransform);
  itkGetModifiableObjectMacro(BorderTransform, MeshBorderTransform);

protected:

  ParameterizationQuadEdgeMeshFilter();
  virtual ~ParameterizationQuadEdgeMeshFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  CoefficientsComputation *m_CoefficientsMethod;

  MeshBorderTransformPointer m_BorderTransform;

  // first is the id of the input mesh and second is the corresponding id
  // in m_BoundaryPtMap
  InputMapPointIdentifier m_BoundaryPtMap;

  // first is the id of the input mesh and second is the corresponding id
  // in m_InternalPtList
  InputMapPointIdentifier m_InternalPtMap;

  std::vector< OutputPointType > m_Border;

  void CopyToOutputBorder();

  /**
   *  \brief From the list of all vertices from the input mesh InputList
   *  and the list of boundary vertices BoundaryList, Store in
   *  m_InternalPtList the list of interior vertices (i.e. vertices in
   *  InputList and not in BoundaryList )
   *
   *  \note I consider ids of points are well chosen (from 0 to
   *        NumberOfPoints)
   */
  void ComputeListOfInteriorVertices();

  /**
   *  \brief Fill matrix iM and vectors Bx and m_By depending on if one
   *  vertex is on the border or not.
   *  \param[in] iM
   *  \param[in,out] ioBx
   *  \param[in,out] ioBy
   */
  void FillMatrix(MatrixType & iM, VectorType & ioBx, VectorType & ioBy);

  /**
   *  \brief Solve linears systems : \f$ iM \cdot oX = iBx \f$ and
   * \f$ iM \cdot oY = iBy \f$
   *
   *  \param[in] iM
   *  \param[in] iBx
   *  \param[in] iBy
   *  \param[out] oX
   *  \param[out] oY
   */
  void SolveLinearSystems(const MatrixType & iM,
                          const VectorType & iBx,
                          const VectorType & iBy,
                          VectorType & oX,
                          VectorType & oY);

  void GenerateData();

private:

  ParameterizationQuadEdgeMeshFilter(const Self &); // purposely not implemented
  void operator=(const Self &);    // purposely not implemented
};
} // end namespace itk

#include "itkParameterizationQuadEdgeMeshFilter.hxx"

#endif
