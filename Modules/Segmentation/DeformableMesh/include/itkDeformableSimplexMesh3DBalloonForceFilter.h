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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkDeformableSimplexMesh3DBalloonForceFilter_h
#define itkDeformableSimplexMesh3DBalloonForceFilter_h

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkMesh.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkCovariantVector.h"

#include <set>

namespace itk
{
/**
 * \class DeformableSimplexMesh3DBalloonForceFilter
 * \brief
 * Additional to its superclass this model adds an balloon force component to the
 * internal forces.
 *
 * The balloon force can be scaled, by setting the parameter kappa.
 *
 * \author Thomas Boettger. Division Medical and Biological Informatics, German Cancer Research Center, Heidelberg.
 *
 * \ingroup ITKDeformableMesh
 */
template <typename TInputMesh, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT DeformableSimplexMesh3DBalloonForceFilter
  : public DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
{
public:
  /** Standard "Self" type alias. */
  using Self = DeformableSimplexMesh3DBalloonForceFilter;

  /** Standard "Superclass" type alias. */
  using Superclass = DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method of creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(DeformableSimplexMesh3DBalloonForceFilter);

  /** Some type alias. */
  using InputMeshType = TInputMesh;
  using OutputMeshType = TOutputMesh;
  using typename Superclass::PointType;
  using typename Superclass::GradientIndexType;
  using typename Superclass::GradientIndexValueType;
  using typename Superclass::GradientImageType;

  /* Mesh pointer definition. */
  using InputMeshPointer = typename InputMeshType::Pointer;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  using PixelType = typename InputMeshType::PixelType;

  using GradientIntensityImageType = Image<PixelType, 3>;
  using GradientIntensityImagePointer = typename GradientIntensityImageType::Pointer;

  /** Set/Get the scalar for balloon force. */
  /** @ITKStartGrouping */
  itkSetMacro(Kappa, double);
  itkGetConstMacro(Kappa, double);
  /** @ITKEndGrouping */
protected:
  DeformableSimplexMesh3DBalloonForceFilter();
  ~DeformableSimplexMesh3DBalloonForceFilter() override = default;
  DeformableSimplexMesh3DBalloonForceFilter(const Self &) {}

  void
  operator=(const Self &)
  {}

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Compute the external force component.
   *
   * Computes the model Displacement according to image gradient forces.
   */
  void
  ComputeExternalForce(SimplexMeshGeometry * data, const GradientImageType * gradientImage) override;

  double m_Kappa{};
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDeformableSimplexMesh3DBalloonForceFilter.hxx"
#endif

#endif // itkDeformableSimplexMesh3DBalloonForceFilter_h
