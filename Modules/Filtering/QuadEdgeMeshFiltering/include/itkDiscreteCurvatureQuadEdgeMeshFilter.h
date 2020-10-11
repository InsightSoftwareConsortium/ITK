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
#ifndef itkDiscreteCurvatureQuadEdgeMeshFilter_h
#define itkDiscreteCurvatureQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkConceptChecking.h"
#include "itkTriangleHelper.h"

namespace itk
{
/**
 * \class DiscreteCurvatureQuadEdgeMeshFilter
 *
 * \brief FIXME
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class DiscreteCurvatureQuadEdgeMeshFilter : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscreteCurvatureQuadEdgeMeshFilter);

  using Self = DiscreteCurvatureQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;

  using OutputMeshType = TOutputMesh;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputPointsContainerPointer = typename OutputMeshType::PointsContainerPointer;
  using OutputPointsContainerIterator = typename OutputMeshType::PointsContainerIterator;
  using OutputPointType = typename OutputMeshType::PointType;
  using OutputCoordType = typename OutputPointType::CoordRepType;
  using OutputPointIdentifier = typename OutputMeshType::PointIdentifier;
  using OutputCellIdentifier = typename OutputMeshType::CellIdentifier;
  using OutputQEType = typename OutputMeshType::QEType;
  using OutputMeshTraits = typename OutputMeshType::MeshTraits;
  using OutputCurvatureType = typename OutputMeshTraits::PixelType;

  using TriangleType = TriangleHelper<OutputPointType>;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(DiscreteCurvatureQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputCurvatureType>));
  // End concept checking
#endif

protected:
  DiscreteCurvatureQuadEdgeMeshFilter()
    : m_OutputMesh(nullptr)
  {}
  ~DiscreteCurvatureQuadEdgeMeshFilter() override = default;

  virtual OutputCurvatureType
  EstimateCurvature(const OutputPointType & iP) = 0;

  OutputCurvatureType
  ComputeMixedArea(OutputQEType * iQE1, OutputQEType * iQE2)
  {

    OutputPointIdentifier id[3];

    id[0] = iQE1->GetOrigin();
    id[1] = iQE1->GetDestination();
    id[2] = iQE2->GetDestination();

    OutputPointType p[3];

    for (int i = 0; i < 3; i++)
    {
      p[i] = this->m_OutputMesh->GetPoint(id[i]);
    }

    return static_cast<OutputCurvatureType>(TriangleType::ComputeMixedArea(p[0], p[1], p[2]));
  }

  void
  GenerateData() override
  {
    this->CopyInputMeshToOutputMesh();

    OutputMeshPointer output = this->GetOutput();

    OutputPointsContainerPointer  points = output->GetPoints();
    OutputPointsContainerIterator p_it = points->Begin();

    OutputCurvatureType curvature;

    this->m_OutputMesh = this->GetOutput();
    while (p_it != points->End())
    {
      curvature = this->EstimateCurvature(p_it->Value());
      output->SetPointData(p_it->Index(), curvature);
      ++p_it;
    }
  }

private:
  /** Cache output pointer to avoid calls in inner loop to GetOutput() */
  OutputMeshType * m_OutputMesh;
};
} // end namespace itk

#endif
