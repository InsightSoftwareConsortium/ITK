/*=========================================================================
 *
 *  Copyright NumFOCUS
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
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class DiscreteMaximumCurvatureQuadEdgeMeshFilter
  : public DiscretePrincipalCurvaturesQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscreteMaximumCurvatureQuadEdgeMeshFilter);

  using Self = DiscreteMaximumCurvatureQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = DiscretePrincipalCurvaturesQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;

  using InputMeshType = typename Superclass::InputMeshType;
  using InputMeshPointer = typename Superclass::InputMeshPointer;

  using OutputMeshType = typename Superclass::OutputMeshType;
  using OutputMeshPointer = typename Superclass::OutputMeshPointer;
  using OutputPointsContainerPointer = typename Superclass::OutputPointsContainerPointer;
  using OutputPointsContainerIterator = typename Superclass::OutputPointsContainerIterator;
  using OutputPointType = typename Superclass::OutputPointType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using OutputCoordType = typename Superclass::OutputCoordType;
  using OutputPointIdentifier = typename Superclass::OutputPointIdentifier;
  using OutputCellIdentifier = typename Superclass::OutputCellIdentifier;
  using OutputQEType = typename Superclass::OutputQEType;
  using OutputMeshTraits = typename Superclass::OutputMeshTraits;
  using OutputCurvatureType = typename Superclass::OutputCurvatureType;

  using TriangleType = typename Superclass::TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DiscreteMaximumCurvatureQuadEdgeMeshFilter, DiscretePrincipalCurvaturesQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputCurvatureType>));
  // End concept checking
#endif

protected:
  DiscreteMaximumCurvatureQuadEdgeMeshFilter() = default;
  ~DiscreteMaximumCurvatureQuadEdgeMeshFilter() override = default;

  OutputCurvatureType
  EstimateCurvature(const OutputPointType & iP) override
  {
    this->ComputeMeanAndGaussianCurvatures(iP);
    return this->m_Mean + std::sqrt(this->ComputeDelta());
  }
};
} // namespace itk

#endif
