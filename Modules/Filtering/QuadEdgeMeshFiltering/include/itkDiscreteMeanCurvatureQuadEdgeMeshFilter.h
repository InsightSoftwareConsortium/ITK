/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkDiscreteMeanCurvatureQuadEdgeMeshFilter_h
#define itkDiscreteMeanCurvatureQuadEdgeMeshFilter_h

#include "itkDiscreteCurvatureQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class DiscreteMeanCurvatureQuadEdgeMeshFilter
 * \brief see the following paper
 * title: Discrete Differential-Geometry Operators for Triangulated 2-Manifolds
 * authors: Mark Meyer, Mathieu Desbrun, Peter Schroder, Alan H. Barr
 * conference: VisMath '02
 * location: Berlin (Germany)
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class ITK_TEMPLATE_EXPORT DiscreteMeanCurvatureQuadEdgeMeshFilter
  : public DiscreteCurvatureQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscreteMeanCurvatureQuadEdgeMeshFilter);

  using Self = DiscreteMeanCurvatureQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = DiscreteCurvatureQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;

  using typename Superclass::InputMeshType;
  using typename Superclass::InputMeshPointer;

  using typename Superclass::OutputMeshType;
  using typename Superclass::OutputMeshPointer;
  using typename Superclass::OutputPointsContainerPointer;
  using typename Superclass::OutputPointsContainerIterator;
  using typename Superclass::OutputPointType;
  using typename Superclass::OutputVectorType;
  using typename Superclass::OutputCoordType;
  using typename Superclass::OutputPointIdentifier;
  using typename Superclass::OutputCellIdentifier;
  using typename Superclass::OutputQEType;
  using typename Superclass::OutputMeshTraits;
  using typename Superclass::OutputCurvatureType;

  using typename Superclass::TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DiscreteMeanCurvatureQuadEdgeMeshFilter, DiscreteCurvatureQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  using CoefficientType = ConformalMatrixCoefficients<OutputMeshType>;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputCurvatureType>));
  // End concept checking
#endif

protected:
  DiscreteMeanCurvatureQuadEdgeMeshFilter() = default;
  ~DiscreteMeanCurvatureQuadEdgeMeshFilter() override = default;

  OutputCurvatureType
  EstimateCurvature(const OutputPointType & iP) override
  {
    OutputMeshPointer output = this->GetOutput();

    OutputQEType * qe = iP.GetEdge();

    OutputCurvatureType oH(0.);

    OutputVectorType Laplace;

    Laplace.Fill(0.);

    OutputCurvatureType area(0.);
    OutputVectorType    normal;
    normal.Fill(0.);

    if (qe != nullptr)
    {
      if (qe != qe->GetOnext())
      {
        CoefficientType coefficent;

        OutputQEType * qe_it = qe;
        OutputQEType * qe_it2;

        OutputCurvatureType temp_area;
        OutputCoordType     temp_coeff;

        OutputPointType  q0, q1;
        OutputVectorType face_normal;

        do
        {
          qe_it2 = qe_it->GetOnext();
          q0 = output->GetPoint(qe_it->GetDestination());
          q1 = output->GetPoint(qe_it2->GetDestination());

          temp_coeff = coefficent(output, qe_it);
          Laplace += temp_coeff * (iP - q0);

          temp_area = this->ComputeMixedArea(qe_it, qe_it2);
          area += temp_area;

          face_normal = TriangleType::ComputeNormal(q0, iP, q1);
          normal += face_normal;

          qe_it = qe_it2;
        } while (qe_it != qe);

        if (area < 1e-6)
        {
          oH = 0.;
        }
        else
        {
          if (normal.GetSquaredNorm() > 0.)
          {
            normal.Normalize();
            Laplace *= 0.25 / area;
            oH = Laplace * normal;
          }
          else
          {
            oH = 0.;
          }
        }
      }
    }
    return oH;
  }
};
} // namespace itk
#endif
