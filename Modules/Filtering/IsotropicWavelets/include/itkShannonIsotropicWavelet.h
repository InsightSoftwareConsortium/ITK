/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkShannonIsotropicWavelet_h
#define itkShannonIsotropicWavelet_h

#include <itkIsotropicWaveletFrequencyFunction.h>

namespace itk
{
/** \class ShannonIsotropicWavelet
 *
 * Shannon Wavelet
 *
 * Implement function in frequency space.
 *
 \f{equation}{
   h(\omega) =
     \begin{cases}
     \begin{aligned}
       &1, &\omega \in [\frac{\pi}{2} , \pi] \\
       &0, &\text{otherwise}
     \end{aligned}
     \end{cases}
 \f}
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class ShannonIsotropicWavelet : public IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShannonIsotropicWavelet);

  /** Standard class type alias. */
  using Self = ShannonIsotropicWavelet;
  using Superclass = IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShannonIsotropicWavelet, IsotropicWaveletFrequencyFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** FunctionValue type for the function. */
  using FunctionValueType = typename Superclass::FunctionValueType;

  /** Evaluate the function */
  FunctionValueType
  EvaluateMagnitude(const FunctionValueType & freq_norm_in_hz) const override;

protected:
  ShannonIsotropicWavelet();
  ~ShannonIsotropicWavelet() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShannonIsotropicWavelet.hxx"
#endif

#endif
