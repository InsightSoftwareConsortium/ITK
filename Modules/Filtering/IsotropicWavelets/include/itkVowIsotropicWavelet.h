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
#ifndef itkVowIsotropicWavelet_h
#define itkVowIsotropicWavelet_h

#include <itkIsotropicWaveletFrequencyFunction.h>

namespace itk
{
/** \class VowIsotropicWavelet
 * \brief Wavelet based on paper VOW: Variance-Optimal Wavelets for Steerable Pyramid (P.Pad et al 2014).
 *
 * Implement function in frequency space.
 \f{equation}{
   h(\omega) =
     \begin{cases}
     \begin{aligned}
       &\sqrt{\frac{1}{2} + \frac{\tan(\kappa(1+2\log_2\frac{2\omega}{\pi})}{2\tan(\kappa)}} , &\omega \in
 [\frac{\pi}{4} , \frac{\pi}{2} [ \\
       &\sqrt{\frac{1}{2} - \frac{\tan(\kappa(1+2\log_2\frac{\omega}{\pi}))}{2\tan(\kappa)}} , &\omega \in
 [\frac{\pi}{2} , \pi ] \\ &0, &\text{otherwise} \end{aligned} \end{cases} \f} where \f$\kappa \in [0, \frac{\pi}{2}]
 \text{ is found to be } 0.75 \f$
 *
 * Where q(t) is a m grade polynomial (m can be chosen) which elements are
 * calculated so the wavelet has desirable properties.
 * ie, tight frame, Vow Paritition of Unity, etc. (see paper for more info)
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class VowIsotropicWavelet : public IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VowIsotropicWavelet);

  /** Standard class type alias. */
  using Self = VowIsotropicWavelet;
  using Superclass = IsotropicWaveletFrequencyFunction<TFunctionValue, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VowIsotropicWavelet, IsotropicWaveletFrequencyFunction);

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
  itkSetMacro(Kappa, TFunctionValue);
  itkGetConstMacro(Kappa, TFunctionValue);

protected:
  VowIsotropicWavelet();
  ~VowIsotropicWavelet() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** kappa value, default is optimal:0.75 */
  FunctionValueType m_Kappa;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVowIsotropicWavelet.hxx"
#endif

#endif
