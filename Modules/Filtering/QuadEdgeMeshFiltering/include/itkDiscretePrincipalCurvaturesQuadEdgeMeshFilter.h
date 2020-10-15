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
#ifndef itkDiscretePrincipalCurvaturesQuadEdgeMeshFilter_h
#define itkDiscretePrincipalCurvaturesQuadEdgeMeshFilter_h

#include "itkDiscreteCurvatureQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class DiscretePrincipalCurvaturesQuadEdgeMeshFilter
 *
 * \brief  FIXME   add documentation here
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class DiscretePrincipalCurvaturesQuadEdgeMeshFilter
  : public DiscreteCurvatureQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscretePrincipalCurvaturesQuadEdgeMeshFilter);

  using Self = DiscretePrincipalCurvaturesQuadEdgeMeshFilter;
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
  itkTypeMacro(DiscretePrincipalCurvaturesQuadEdgeMeshFilter, DiscreteCurvatureQuadEdgeMeshFilter);

  using CoefficientType = ConformalMatrixCoefficients<OutputMeshType>;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputCurvatureType>));
  // End concept checking
#endif

protected:
  DiscretePrincipalCurvaturesQuadEdgeMeshFilter()
    : m_Gaussian(0.0)
    , m_Mean(0.0)
  {}
  ~DiscretePrincipalCurvaturesQuadEdgeMeshFilter() override = default;

  OutputCurvatureType m_Gaussian;
  OutputCurvatureType m_Mean;

  void
  ComputeMeanAndGaussianCurvatures(const OutputPointType & iP)
  {
    OutputMeshPointer output = this->GetOutput();

    OutputQEType * qe = iP.GetEdge();

    m_Mean = 0.;
    m_Gaussian = 0.;

    if (qe != nullptr)
    {
      OutputVectorType Laplace;
      Laplace.Fill(0.);

      OutputQEType * qe_it = qe;

      OutputCurvatureType area(0.), sum_theta(0.);

      if (qe_it != qe_it->GetOnext())
      {
        qe_it = qe;
        OutputQEType * qe_it2;

        OutputPointType  q0, q1;
        OutputVectorType face_normal;

        OutputVectorType normal;
        normal.Fill(0.);

        OutputCurvatureType temp_area;
        OutputCoordType     temp_coeff;

        CoefficientType coefficent;

        do
        {
          qe_it2 = qe_it->GetOnext();
          q0 = output->GetPoint(qe_it->GetDestination());
          q1 = output->GetPoint(qe_it2->GetDestination());

          temp_coeff = coefficent(output, qe_it);
          Laplace += temp_coeff * (iP - q0);

          // Compute Angle;
          sum_theta += static_cast<OutputCurvatureType>(TriangleType::ComputeAngle(q0, iP, q1));

          temp_area = this->ComputeMixedArea(qe_it, qe_it2);
          area += temp_area;

          face_normal = TriangleType::ComputeNormal(q0, iP, q1);
          normal += face_normal;

          qe_it = qe_it2;
        } while (qe_it != qe);

        if (area > 1e-10)
        {
          area = 1. / area;
          Laplace *= 0.25 * area;
          m_Mean = Laplace * normal;
          m_Gaussian = (2. * itk::Math::pi - sum_theta) * area;
        }
      }
    }
  }

  virtual OutputCurvatureType
  ComputeDelta()
  {
    return std::max(static_cast<OutputCurvatureType>(0.), m_Mean * m_Mean - m_Gaussian);
  }
};
} // namespace itk

#endif
