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
#ifndef itkDiscreteGaussianCurvatureQuadEdgeMeshFilter_h
#define itkDiscreteGaussianCurvatureQuadEdgeMeshFilter_h

#include "itkDiscreteCurvatureQuadEdgeMeshFilter.h"
#include "itkMath.h"


namespace itk
{
/**
 * \class DiscreteGaussianCurvatureQuadEdgeMeshFilter
 * \brief see the following paper
 * title: Discrete Differential-Geometry Operators for Triangulated 2-Manifolds
 * authors: Mark Meyer, Mathieu Desbrun, Peter Schroder, Alan H. Barr
 * conference: VisMath '02
 * location: Berlin (Germany)
 * \author: Arnaud Gelas, Alexandre Gouaillard
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class DiscreteGaussianCurvatureQuadEdgeMeshFilter : public DiscreteCurvatureQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscreteGaussianCurvatureQuadEdgeMeshFilter);

  using Self = DiscreteGaussianCurvatureQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = DiscreteCurvatureQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;

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
  itkTypeMacro(DiscreteGaussianCurvatureQuadEdgeMeshFilter, DiscreteCurvatureQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputCurvatureType>));
  // End concept checking
#endif

protected:
  DiscreteGaussianCurvatureQuadEdgeMeshFilter() = default;
  ~DiscreteGaussianCurvatureQuadEdgeMeshFilter() override = default;

  OutputCurvatureType
  EstimateCurvature(const OutputPointType & iP) override
  {
    OutputMeshPointer output = this->GetOutput();

    OutputQEType * qe = iP.GetEdge();

    if (qe != nullptr)
    {
      OutputQEType * qe_it = qe;
      OutputQEType * qe_it2;

      OutputPointType q0, q1;

      OutputCurvatureType sum_theta = 0.;
      OutputCurvatureType area = 0.;

      do
      {
        // cell_id = qe_it->GetLeft();
        qe_it2 = qe_it->GetOnext();
        q0 = output->GetPoint(qe_it->GetDestination());
        q1 = output->GetPoint(qe_it2->GetDestination());

        // Compute Angle;
        sum_theta += static_cast<OutputCurvatureType>(TriangleType::ComputeAngle(q0, iP, q1));
        area += this->ComputeMixedArea(qe_it, qe_it2);
        qe_it = qe_it2;
      } while (qe_it != qe);

      return (2.0 * itk::Math::pi - sum_theta) / area;
    }

    return 0.;
  }
};
} // namespace itk

#endif
