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
#ifndef itkAdditiveGaussianNoiseMeshFilter_h
#define itkAdditiveGaussianNoiseMeshFilter_h

#include <itkMeshToMeshFilter.h>

namespace itk
{
/** \class AdditiveGaussianNoiseMeshFilter
 * \brief Add Gaussian noise to the points defining an itkMesh.
 *
 * \author Davis Vigneault
 *
 * This class adds Gaussian noise with a specified mean and standard
 * deviation to the coordinates of the points defining an itkMesh.
 * Mesh topology and other data is passed unaltered.  This may be
 * useful in testing the robustness of an algorithm to small changes
 * in the input mesh, augmenting datasets for machine learning, and
 * counteracting deleterious effects which highly regular regions
 * of a mesh may occassionally have on mesh processing.
 *
 * \ingroup MeshNoise
 */
template <typename TInput, typename TOutput = TInput>
class AdditiveGaussianNoiseMeshFilter : public MeshToMeshFilter<TInput, TOutput>
{

public:
  ITK_DISALLOW_COPY_AND_MOVE(AdditiveGaussianNoiseMeshFilter);

  /** Standard class type alias. */
  using Self = AdditiveGaussianNoiseMeshFilter;
  using Superclass = MeshToMeshFilter<TInput, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = TInput;
  using OutputMeshType = TOutput;
  using InputMeshPointer = typename InputMeshType::Pointer;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  /** Type for representing coordinates. */
  using CoordinateType = typename TInput::CoordinateType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(AdditiveGaussianNoiseMeshFilter);

  /** Mean of noise. */
  itkGetConstMacro(Mean, CoordinateType);
  itkSetMacro(Mean, CoordinateType);

  /** Variance of noise. */
  itkGetConstMacro(Sigma, CoordinateType);
  itkSetMacro(Sigma, CoordinateType);

  /** Initialization seed. */
  itkGetConstMacro(Seed, int);
  itkSetMacro(Seed, int);

protected:
  AdditiveGaussianNoiseMeshFilter();
  ~AdditiveGaussianNoiseMeshFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Requested Data */
  void
  GenerateData() override;

  CoordinateType m_Mean;
  CoordinateType m_Sigma;
  int          m_Seed;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAdditiveGaussianNoiseMeshFilter.hxx"
#endif

#endif
