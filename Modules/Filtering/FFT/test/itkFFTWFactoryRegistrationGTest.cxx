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

#include "gtest/gtest.h"
#include "itkConfigure.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)

#  include "itkImage.h"
#  include "itkForwardFFTImageFilter.h"
#  include "itkInverseFFTImageFilter.h"
#  include "itkForward1DFFTImageFilter.h"
#  include "itkInverse1DFFTImageFilter.h"
#  include "itkComplexToComplexFFTImageFilter.h"
#  include "itkComplexToComplex1DFFTImageFilter.h"
#  include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#  include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#  include "itkTestDriverIncludeRequiredFactories.h"
#  include <complex>
#  include <string>

namespace
{

// Call RegisterRequiredFactories() once for the entire test suite.
class FFTWFactoryRegistrationTestSuite : public ::testing::Test
{
protected:
  static void
  SetUpTestSuite()
  {
    RegisterRequiredFactories();
  }
};

// Helper: verify the filter created by New() has "FFTW" in its class name.
void
ExpectFFTWBackend(const itk::LightObject * filter, const std::string & filterDescription)
{
  ASSERT_NE(filter, nullptr) << filterDescription << "::New() returned nullptr";
  const std::string name = filter->GetNameOfClass();
  EXPECT_NE(name.find("FFTW"), std::string::npos)
    << filterDescription << "::New() resolved to '" << name << "' (expected FFTW backend)";
}

} // namespace


#  if defined(ITK_USE_FFTWF)

using FloatImage2D = itk::Image<float, 2>;
using FloatComplexImage2D = itk::Image<std::complex<float>, 2>;

TEST_F(FFTWFactoryRegistrationTestSuite, ForwardFFT_float)
{
  auto filter = itk::ForwardFFTImageFilter<FloatImage2D>::New();
  ExpectFFTWBackend(filter, "ForwardFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, InverseFFT_float)
{
  auto filter = itk::InverseFFTImageFilter<FloatComplexImage2D, FloatImage2D>::New();
  ExpectFFTWBackend(filter, "InverseFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, Forward1DFFT_float)
{
  auto filter = itk::Forward1DFFTImageFilter<FloatImage2D>::New();
  ExpectFFTWBackend(filter, "Forward1DFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, Inverse1DFFT_float)
{
  auto filter = itk::Inverse1DFFTImageFilter<FloatComplexImage2D, FloatImage2D>::New();
  ExpectFFTWBackend(filter, "Inverse1DFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, ComplexToComplexFFT_float)
{
  auto filter = itk::ComplexToComplexFFTImageFilter<FloatComplexImage2D>::New();
  ExpectFFTWBackend(filter, "ComplexToComplexFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, ComplexToComplex1DFFT_float)
{
  auto filter = itk::ComplexToComplex1DFFTImageFilter<FloatComplexImage2D>::New();
  ExpectFFTWBackend(filter, "ComplexToComplex1DFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, RealToHalfHermitianForwardFFT_float)
{
  auto filter = itk::RealToHalfHermitianForwardFFTImageFilter<FloatImage2D>::New();
  ExpectFFTWBackend(filter, "RealToHalfHermitianForwardFFTImageFilter<float>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, HalfHermitianToRealInverseFFT_float)
{
  auto filter = itk::HalfHermitianToRealInverseFFTImageFilter<FloatComplexImage2D, FloatImage2D>::New();
  ExpectFFTWBackend(filter, "HalfHermitianToRealInverseFFTImageFilter<float>");
}

#  endif // ITK_USE_FFTWF


#  if defined(ITK_USE_FFTWD)

using DoubleImage2D = itk::Image<double, 2>;
using DoubleComplexImage2D = itk::Image<std::complex<double>, 2>;

TEST_F(FFTWFactoryRegistrationTestSuite, ForwardFFT_double)
{
  auto filter = itk::ForwardFFTImageFilter<DoubleImage2D>::New();
  ExpectFFTWBackend(filter, "ForwardFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, InverseFFT_double)
{
  auto filter = itk::InverseFFTImageFilter<DoubleComplexImage2D, DoubleImage2D>::New();
  ExpectFFTWBackend(filter, "InverseFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, Forward1DFFT_double)
{
  auto filter = itk::Forward1DFFTImageFilter<DoubleImage2D>::New();
  ExpectFFTWBackend(filter, "Forward1DFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, Inverse1DFFT_double)
{
  auto filter = itk::Inverse1DFFTImageFilter<DoubleComplexImage2D, DoubleImage2D>::New();
  ExpectFFTWBackend(filter, "Inverse1DFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, ComplexToComplexFFT_double)
{
  auto filter = itk::ComplexToComplexFFTImageFilter<DoubleComplexImage2D>::New();
  ExpectFFTWBackend(filter, "ComplexToComplexFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, ComplexToComplex1DFFT_double)
{
  auto filter = itk::ComplexToComplex1DFFTImageFilter<DoubleComplexImage2D>::New();
  ExpectFFTWBackend(filter, "ComplexToComplex1DFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, RealToHalfHermitianForwardFFT_double)
{
  auto filter = itk::RealToHalfHermitianForwardFFTImageFilter<DoubleImage2D>::New();
  ExpectFFTWBackend(filter, "RealToHalfHermitianForwardFFTImageFilter<double>");
}

TEST_F(FFTWFactoryRegistrationTestSuite, HalfHermitianToRealInverseFFT_double)
{
  auto filter = itk::HalfHermitianToRealInverseFFTImageFilter<DoubleComplexImage2D, DoubleImage2D>::New();
  ExpectFFTWBackend(filter, "HalfHermitianToRealInverseFFTImageFilter<double>");
}

#  endif // ITK_USE_FFTWD

#endif // ITK_USE_FFTWF || ITK_USE_FFTWD
