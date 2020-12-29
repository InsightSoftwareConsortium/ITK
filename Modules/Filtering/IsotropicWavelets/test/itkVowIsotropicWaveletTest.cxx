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

#include "itkVowIsotropicWavelet.h"
#include "itkTestingMacros.h"

#include <string>

int
itkVowIsotropicWaveletTest(int, char *[])
{
  bool testPassed = true;

  using Double = double;
  using Float = float;

  constexpr unsigned int D3 = 3;
  constexpr unsigned int D2 = 2;
  constexpr unsigned int D1 = 1;

  using Default = itk::VowIsotropicWavelet<>;
  Default::New();

  using Point3D = itk::Point<itk::SpacePrecisionType, D3>;
  using Point2D = itk::Point<itk::SpacePrecisionType, D2>;
  using Point1D = itk::Point<itk::SpacePrecisionType, D1>;

  using Wavelet3D = itk::VowIsotropicWavelet<Double, D3, Point3D>;
  using Wavelet2D = itk::VowIsotropicWavelet<Double, D2, Point2D>;
  using Wavelet1D = itk::VowIsotropicWavelet<Double, D1, Point1D>;
  Wavelet3D::New();
  Wavelet2D::New();
  Wavelet1D::New();

  using Wavelet3DFloat = itk::VowIsotropicWavelet<Float, D3, Point3D>;
  using Wavelet2DFloat = itk::VowIsotropicWavelet<Float, D2, Point2D>;
  using Wavelet1DFloat = itk::VowIsotropicWavelet<Float, D1, Point1D>;
  Wavelet3DFloat::New();
  Wavelet2DFloat::New();
  Wavelet1DFloat::New();

  auto    wavelet2Dfloat = Wavelet2DFloat::New();
  Point2D point2D;
  point2D[0] = 0.2;
  point2D[1] = 0.2;
  double freq2D = wavelet2Dfloat->Magnitude(point2D);
  std::cout << "freq2D: " << freq2D << std::endl;
  // Check that inherits from IsotropicFrequencyFunction
  unsigned int defaultBands = wavelet2Dfloat->GetHighPassSubBands();
  if (defaultBands != 1)
  {
    testPassed = false;
  }

  // Specific methods for this type:
  Wavelet2DFloat::FunctionValueType differentKappa = 0.750001;
  wavelet2Dfloat->SetKappa(differentKappa);
  ITK_TEST_SET_GET_VALUE(differentKappa, wavelet2Dfloat->GetKappa());

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
