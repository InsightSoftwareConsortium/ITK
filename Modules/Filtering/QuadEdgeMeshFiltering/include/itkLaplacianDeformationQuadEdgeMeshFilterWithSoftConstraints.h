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
#ifndef itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints_h
#define itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints_h

#include "itkLaplacianDeformationQuadEdgeMeshFilter.h"

namespace itk {
/** \class LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints
 *
 *  \brief Laplacian deformation with soft constraints (approximating
 *  displacement for handle points).
 *
 * Laplacian mesh deformation offers the ability to deform 3D surface mesh
 * while preserving local details.
 *
 * In this context output mesh vertices are somehow closed to provided output
 * locations, the weight associated to one constrain can be set by the means of
 * m_Lambda (for all constraints), or for one particular constraint by using
 * SetLocalLambda
 *
 * For details, see https://hdl.handle.net/10380/3410
 *
 *  \ingroup ITKQuadEdgeMeshFiltering
 */
template< class TInputMesh, class TOutputMesh, class TSolverTraits >
class ITK_TEMPLATE_EXPORT LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints:
  public LaplacianDeformationQuadEdgeMeshFilter< TInputMesh, TOutputMesh, TSolverTraits >
{
public:
  /** Basic types. */
  typedef LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints Self;
  typedef LaplacianDeformationQuadEdgeMeshFilter< TInputMesh,
                                                  TOutputMesh,
                                                  TSolverTraits >   Superclass;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;

  /** Input types. */
  typedef TInputMesh                              InputMeshType;

  itkStaticConstMacro(InputPointDimension, unsigned int, InputMeshType::PointDimension);

  /** Output types. */
  typedef TOutputMesh                                         OutputMeshType;
  typedef typename Superclass::OutputPointType                OutputPointType;
  typedef typename Superclass::OutputCoordRepType             OutputCoordRepType;
  typedef typename Superclass::OutputPointIdentifier          OutputPointIdentifier;

  itkStaticConstMacro(OutputPointDimension, unsigned int, OutputMeshType::PointDimension);

  typedef TSolverTraits                     SolverTraits;
  typedef typename Superclass::ValueType    ValueType;
  typedef typename Superclass::MatrixType   MatrixType;
  typedef typename Superclass::VectorType   VectorType;

  itkNewMacro(Self);
  itkTypeMacro(LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints, LaplacianDeformationQuadEdgeMeshFilter);

  itkSetMacro(Lambda,OutputCoordRepType);
  itkGetMacro(Lambda,OutputCoordRepType);

  void SetLocalLambda( OutputPointIdentifier vId, OutputCoordRepType iL );

protected:

  LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints();
  virtual ~LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   *  \brief Fill matrix iM and vectors Bx and m_By depending on if one
   *  vertex is on the border or not.
   */
  void FillMatrix(MatrixType & iM, VectorType & iBx, VectorType & iBy, VectorType & iBz);

  virtual void GenerateData() ITK_OVERRIDE;

  typedef typename Superclass::ConstraintMapType                ConstraintMapType;
  typedef typename Superclass::ConstraintMapConstIterator       ConstraintMapConstIterator;

  typedef typename Superclass::OutputMapPointIdentifier               OutputMapPointIdentifier;
  typedef typename Superclass::OutputMapPointIdentifierIterator       OutputMapPointIdentifierIterator;
  typedef typename Superclass::OutputMapPointIdentifierConstIterator  OutputMapPointIdentifierConstIterator;


  typedef typename Superclass::RowType          RowType;
  typedef typename Superclass::RowConstIterator RowConstIterator;
  typedef typename Superclass::RowIterator      RowIterator;

  virtual void ComputeVertexIdMapping() ITK_OVERRIDE;

  OutputCoordRepType m_Lambda;
  OutputCoordRepType m_LambdaSquare;

  itksys::hash_map< OutputPointIdentifier, OutputCoordRepType > m_LocalLambdaSquare;


private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints);
};
} // end namespace itk

#include "itkLaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints.hxx"

#endif
