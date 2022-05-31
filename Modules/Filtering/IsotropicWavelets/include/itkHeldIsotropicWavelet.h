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
#ifndef itkHeldIsotropicWavelet_h
#define itkHeldIsotropicWavelet_h

#include <itkIsotropicWaveletFrequencyFunction.h>

namespace itk
{
/** \class HeldIsotropicWavelet
 * \brief Wavelet based on paper Steerable Wavelet Frames Based on the Held
 * Transform (Held et al 2010).
 *
 * Implement function in frequency space.
 *
 * h(w) = cos(2*pi*q(|w|)) for w in (1/8, 1/4]
 * h(w) = sin(2*pi*q(|w/2|)) for w in (1/4, 1/2]
 * h(w) = 0 elsewhere.
 *
 * Where q(t) is a m grade polynomial (m can be chosen) which elements are
 * calculated so the wavelet has desirable properties.
 * ie, tight frame, Held Paritition of Unity, etc. (see paper for more info)
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class HeldIsotropicWavelet : public IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HeldIsotropicWavelet);

  /** Standard class type alias. */
  using Self = HeldIsotropicWavelet;
  using Superclass = IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HeldIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** FunctionValue type for the function. */
  using FunctionValueType = typename Superclass::FunctionValueType;

  /** Type used to store gaussian parameters. */
  using ArrayType = FixedArray<double, VImageDimension>;

  /** Evaluate the function */
  FunctionValueType
  EvaluateMagnitude(const FunctionValueType & freq_norm_in_hz) const override;

  /** Gets and sets parameters */
  itkSetMacro(PolynomialOrder, unsigned int);
  itkGetConstMacro(PolynomialOrder, unsigned int);

  FunctionValueType
  ComputePolynom(const FunctionValueType & freq_norm_in_hz, const unsigned int & order) const;

protected:
  HeldIsotropicWavelet();
  ~HeldIsotropicWavelet() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The order of the polynom. */
  unsigned int m_PolynomialOrder{ 5 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHeldIsotropicWavelet.hxx"
#endif

#endif
