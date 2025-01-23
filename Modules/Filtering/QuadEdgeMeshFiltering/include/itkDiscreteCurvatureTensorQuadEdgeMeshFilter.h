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
#ifndef itkDiscreteCurvatureTensorQuadEdgeMeshFilter_h
#define itkDiscreteCurvatureTensorQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class DiscreteCurvatureTensorQuadEdgeMeshFilter
 *
 * \brief FIXME Add documentation here
 *
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class ITK_TEMPLATE_EXPORT DiscreteCurvatureTensorQuadEdgeMeshFilter
  : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscreteCurvatureTensorQuadEdgeMeshFilter);

  using Self = DiscreteCurvatureTensorQuadEdgeMeshFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(DiscreteCurvatureTensorQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

  // itkConceptMacro( OutputIsFloatingPointCheck,
  //                  ( Concept::IsFloatingPoint< OutputCurvatureType > ) );

protected:
  DiscreteCurvatureTensorQuadEdgeMeshFilter() = default;
  ~DiscreteCurvatureTensorQuadEdgeMeshFilter() override = default;

  /// TODO to be implemented
  void
  GenerateData() override
  {}
};
} // namespace itk

#endif
