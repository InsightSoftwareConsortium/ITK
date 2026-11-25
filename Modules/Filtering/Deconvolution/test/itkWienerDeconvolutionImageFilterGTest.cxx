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

#include "itkWienerDeconvolutionImageFilter.h"
#include "itkGTest.h"
#include <complex>

// When the input power at a frequency equals the noise power spectral density, the
// regularization term divides by zero; the functor must return zero instead of NaN
// (issue #6575, B4).
TEST(WienerDeconvolutionFunctor, ZeroSignalToNoiseYieldsZero)
{
  using ComplexType = std::complex<double>;
  itk::Functor::WienerDeconvolutionFunctor<ComplexType> functor;
  functor.SetNoisePowerSpectralDensityConstant(4.0);
  functor.SetKernelZeroMagnitudeThreshold(1.0e-4);

  // |I|^2 == Pn triggers the degenerate branch.
  const ComplexType value = functor(ComplexType(2.0, 0.0), ComplexType(1.0, 0.0));
  EXPECT_EQ(value, ComplexType(0.0, 0.0));

  // A non-degenerate frequency still deconvolves finitely.
  const ComplexType regular = functor(ComplexType(3.0, 0.0), ComplexType(1.0, 0.0));
  EXPECT_TRUE(std::isfinite(regular.real()) && std::isfinite(regular.imag()));
  EXPECT_NE(regular, ComplexType(0.0, 0.0));
}
