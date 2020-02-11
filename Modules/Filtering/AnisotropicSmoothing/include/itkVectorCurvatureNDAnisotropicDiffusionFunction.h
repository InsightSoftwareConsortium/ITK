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
#ifndef itkVectorCurvatureNDAnisotropicDiffusionFunction_h
#define itkVectorCurvatureNDAnisotropicDiffusionFunction_h

#include "itkVectorAnisotropicDiffusionFunction.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkVectorNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk
{
/** \class VectorCurvatureNDAnisotropicDiffusionFunction
 *
 * This class is a simple extension of the
 * CurvatureNDAnisotropicDiffusionFunction to pixel types of multiple
 * components.  Vector components are diffused separately, but diffusion of
 * each component is limited by a conductance term which depends on all
 * components.
 *
 * For more information, please see CurvatureNDAnisotropicDiffusionFunction.
 *
 * \sa CurvatureNDAnisotropicDiffusionFunction
 * \sa VectorGradientNDAnisotropicDiffusionFunction
 * \sa AnisotropicDiffusionFunction
 * \ingroup ITKAnisotropicSmoothing
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT VectorCurvatureNDAnisotropicDiffusionFunction
  : public VectorAnisotropicDiffusionFunction<TImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorCurvatureNDAnisotropicDiffusionFunction);

  /** Standard itk Self & Superclass type alias */
  using Self = VectorCurvatureNDAnisotropicDiffusionFunction;
  using Superclass = VectorAnisotropicDiffusionFunction<TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorCurvatureNDAnisotropicDiffusionFunction, VectorAnisotropicDiffusionFunction);

  /** Inherit some parameters from the superclass type. */
  using ImageType = typename Superclass::ImageType;
  using PixelType = typename Superclass::PixelType;
  using TimeStepType = typename Superclass::TimeStepType;
  using RadiusType = typename Superclass::RadiusType;
  using NeighborhoodType = typename Superclass::NeighborhoodType;
  using FloatOffsetType = typename Superclass::FloatOffsetType;
  using ScalarValueType = typename PixelType::ValueType;

  /** Extract the image and vector dimension. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;
  static constexpr unsigned int VectorDimension = Superclass::VectorDimension;

  /** Compute the equation value. */
  PixelType
  ComputeUpdate(const NeighborhoodType & neighborhood,
                void *                   globalData,
                const FloatOffsetType &  offset = FloatOffsetType(0.0)) override;

  /** This method is called prior to each iteration of the solver. */
  void
  InitializeIteration() override
  {
    m_K = this->GetAverageGradientMagnitudeSquared() * this->GetConductanceParameter() *
          this->GetConductanceParameter() * -2.0f;
  }

protected:
  VectorCurvatureNDAnisotropicDiffusionFunction();
  ~VectorCurvatureNDAnisotropicDiffusionFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }

private:
  /** Inner product function. */
  VectorNeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /** Slices for the ND neighborhood. */
  std::slice x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension][ImageDimension];
  std::slice xd_slice[ImageDimension][ImageDimension];

  /** Derivative operator */
  DerivativeOperator<ScalarValueType, Self::ImageDimension> m_DerivativeOperator;

  /** Modified global average gradient magnitude term. */
  double m_K{ 0.0 };

  static double m_MIN_NORM;
  SizeValueType m_Center;
  SizeValueType m_Stride[ImageDimension];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorCurvatureNDAnisotropicDiffusionFunction.hxx"
#endif

#endif
