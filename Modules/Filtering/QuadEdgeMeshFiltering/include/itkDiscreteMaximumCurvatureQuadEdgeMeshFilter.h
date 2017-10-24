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
#ifndef itkDiscreteMaximumCurvatureQuadEdgeMeshFilter_h
#define itkDiscreteMaximumCurvatureQuadEdgeMeshFilter_h

#include "itkDiscretePrincipalCurvaturesQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class DiscreteMaximumCurvatureQuadEdgeMeshFilter
 *
 * \brief FIXME     Add documentation here
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template< typename TInputMesh, typename TOutputMesh=TInputMesh >
class DiscreteMaximumCurvatureQuadEdgeMeshFilter:
  public DiscretePrincipalCurvaturesQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef DiscreteMaximumCurvatureQuadEdgeMeshFilter  Self;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef DiscretePrincipalCurvaturesQuadEdgeMeshFilter<
    TInputMesh, TOutputMesh >                         Superclass;

  typedef typename Superclass::InputMeshType    InputMeshType;
  typedef typename Superclass::InputMeshPointer InputMeshPointer;

  typedef typename Superclass::OutputMeshType                OutputMeshType;
  typedef typename Superclass::OutputMeshPointer             OutputMeshPointer;
  typedef typename Superclass::OutputPointsContainerPointer  OutputPointsContainerPointer;
  typedef typename Superclass::OutputPointsContainerIterator OutputPointsContainerIterator;
  typedef typename Superclass::OutputPointType               OutputPointType;
  typedef typename Superclass::OutputVectorType              OutputVectorType;
  typedef typename Superclass::OutputCoordType               OutputCoordType;
  typedef typename Superclass::OutputPointIdentifier         OutputPointIdentifier;
  typedef typename Superclass::OutputCellIdentifier          OutputCellIdentifier;
  typedef typename Superclass::OutputQEType                  OutputQEType;
  typedef typename Superclass::OutputMeshTraits              OutputMeshTraits;
  typedef typename Superclass::OutputCurvatureType           OutputCurvatureType;

  typedef typename Superclass::TriangleType TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DiscreteMaximumCurvatureQuadEdgeMeshFilter, DiscretePrincipalCurvaturesQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputCurvatureType > ) );
  // End concept checking
#endif

protected:
  DiscreteMaximumCurvatureQuadEdgeMeshFilter() {}
  ~DiscreteMaximumCurvatureQuadEdgeMeshFilter() ITK_OVERRIDE {}

  virtual OutputCurvatureType EstimateCurvature(const OutputPointType & iP) ITK_OVERRIDE
  {
    this->ComputeMeanAndGaussianCurvatures(iP);
    return this->m_Mean + std::sqrt( this->ComputeDelta() );
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DiscreteMaximumCurvatureQuadEdgeMeshFilter);
};
}

#endif
