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

#include "itkSimoncelliIsotropicWavelet.h"
#include "itkTestingMacros.h"

#include <string>

int
itkSimoncelliIsotropicWaveletTest(int, char *[])
{
  bool testPassed = true;

  typedef double Double;
  typedef float  Float;

  const unsigned int D3 = 3;
  const unsigned int D2 = 2;
  const unsigned int D1 = 1;

  typedef itk::SimoncelliIsotropicWavelet<> Default;
  Default::New();

  typedef itk::Point<itk::SpacePrecisionType, D3> Point3D;
  typedef itk::Point<itk::SpacePrecisionType, D2> Point2D;
  typedef itk::Point<itk::SpacePrecisionType, D1> Point1D;

  typedef itk::SimoncelliIsotropicWavelet<Double, D3, Point3D> Wavelet3D;
  typedef itk::SimoncelliIsotropicWavelet<Double, D2, Point2D> Wavelet2D;
  typedef itk::SimoncelliIsotropicWavelet<Double, D1, Point1D> Wavelet1D;
  Wavelet3D::New();
  Wavelet2D::New();
  Wavelet1D::New();

  typedef itk::SimoncelliIsotropicWavelet<Float, D3, Point3D> Wavelet3DFloat;
  typedef itk::SimoncelliIsotropicWavelet<Float, D2, Point2D> Wavelet2DFloat;
  typedef itk::SimoncelliIsotropicWavelet<Float, D1, Point1D> Wavelet1DFloat;
  Wavelet3DFloat::New();
  Wavelet2DFloat::New();
  Wavelet1DFloat::New();

  Wavelet2DFloat::Pointer wavelet2Dfloat = Wavelet2DFloat::New();
  Point2D                 point2D;
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

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}
