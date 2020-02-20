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

#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNumberToString.h"

#include "itkTestingMacros.h"

#include <string>
#include <fstream>

namespace itk
{
namespace Testing
{
// Generate values from a linear space
std::vector<double>
linSpaceForIWFF(double init = 0.0, double end = 1.0, size_t points = 1000)
{
  std::vector<double> wArray(points);
  if (points <= 1)
  {
    throw("linSpace needs more points");
  }
  double interval = (end - init) / (points - 1);
  for (unsigned int i = 0; i < points; ++i)
  {
    wArray[i] = init + interval * i;
  }
  return wArray;
}
} // namespace Testing
} // namespace itk

template <unsigned int VDimension, typename TWaveletFunction>
int
runIsotropicWaveletFrequencyFunctionTest(const std::string & profileDataRootPath,
                                         const std::string &, // outputImage,
                                         const unsigned int & inputBands,
                                         const std::string &  waveletTypeName)
{
  itk::NumberToString<float> n2s;
  using WaveletFunctionType = TWaveletFunction;
  auto motherWavelet = WaveletFunctionType::New();
  motherWavelet->SetHighPassSubBands(inputBands);

  double              init = 0.0;
  double              end = 1.0;
  size_t              points = 1000;
  std::vector<double> wArray = itk::Testing::linSpaceForIWFF(init, end, points);
  // Generate profile data for sub-bands
  std::vector<std::vector<double>> subBandsResults;
  for (unsigned int k = 0; k < inputBands + 1; ++k)
  {
    std::vector<double> bandResults;
    for (unsigned int i = 0; i < points; ++i)
    {
      bandResults.push_back(motherWavelet->EvaluateForwardSubBand(wArray[i], k));
    }
    subBandsResults.push_back(bandResults);
  }

  // Generate mother wavelet profile h(2^i w) at different levels
  std::vector<std::vector<double>> radialFrequenciesMotherWavelet;
  unsigned int                     numLevels = 4;
  for (unsigned int levels = 0; levels < numLevels; ++levels)
  {
    std::vector<double> resultPerLevel;
    double              levelFactor = (levels == 0) ? 1.0 : std::pow(2.0, static_cast<double>(levels));
    for (unsigned int i = 0; i < points; ++i)
    {
      resultPerLevel.push_back(motherWavelet->EvaluateMagnitude(levelFactor * wArray[i]));
    }
    radialFrequenciesMotherWavelet.push_back(resultPerLevel);
  }

  // Write subbands
  const std::string outputFilePathSubBands =
    profileDataRootPath + "_" + waveletTypeName + "_" + n2s(inputBands) + "_SubBands.txt";
  std::ofstream ofsSB(outputFilePathSubBands.c_str(), std::ofstream::out);
  for (unsigned int i = 0; i < points; ++i)
  {
    ofsSB << wArray[i];
    for (unsigned int k = 0; k < inputBands + 1; ++k)
    {
      ofsSB << "," << subBandsResults[k][i];
    }
    ofsSB << std::endl;
  }
  ofsSB.close();
  // Write mother wavelets
  const std::string outputFilePathMotherWavelet =
    profileDataRootPath + "_" + waveletTypeName + "_" + n2s(inputBands) + "_Mother.txt";
  std::ofstream ofsMW(outputFilePathMotherWavelet.c_str(), std::ofstream::out);
  for (unsigned int i = 0; i < points; ++i)
  {
    ofsMW << wArray[i];
    for (unsigned int levels = 0; levels < numLevels; ++levels)
    {
      ofsMW << "," << radialFrequenciesMotherWavelet[levels][i];
    }
    ofsMW << std::endl;
  }
  ofsMW.close();
  return EXIT_SUCCESS;
}

int
itkIsotropicWaveletFrequencyFunctionTest(int argc, char * argv[])
{
  if (argc < 5 || argc > 6)
  {
    std::cerr << "Usage: " << argv[0] << "profileDataRootPath outputImage inputBands waveletFunction [dimension]"
              << std::endl;
    return EXIT_FAILURE;
  }
  const std::string  profileDataRootPath = argv[1];
  const std::string  outputImage = argv[2];
  const unsigned int inputBands = std::stoi(argv[3]);
  const std::string  waveletFunction = argv[4];

  unsigned int dimension = 3;
  if (argc == 6)
  {
    dimension = std::stoi(argv[5]);
  }

  using HeldWavelet = itk::HeldIsotropicWavelet<>;
  using VowWavelet = itk::VowIsotropicWavelet<>;
  using SimoncelliWavelet = itk::SimoncelliIsotropicWavelet<>;
  using ShannonWavelet = itk::ShannonIsotropicWavelet<>;
  if (dimension == 2)
  {
    if (waveletFunction == "Held")
    {
      return runIsotropicWaveletFrequencyFunctionTest<2, HeldWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else if (waveletFunction == "Vow")
    {
      return runIsotropicWaveletFrequencyFunctionTest<2, VowWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runIsotropicWaveletFrequencyFunctionTest<2, SimoncelliWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else if (waveletFunction == "Shannon")
    {
      return runIsotropicWaveletFrequencyFunctionTest<2, ShannonWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else
    {
      std::cerr << argv[4] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
    {
      return runIsotropicWaveletFrequencyFunctionTest<3, HeldWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else if (waveletFunction == "Vow")
    {
      return runIsotropicWaveletFrequencyFunctionTest<3, VowWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else if (waveletFunction == "Simoncelli")
    {
      return runIsotropicWaveletFrequencyFunctionTest<3, SimoncelliWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else if (waveletFunction == "Shannon")
    {
      return runIsotropicWaveletFrequencyFunctionTest<3, ShannonWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    }
    else
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << argv[4] << " wavelet type not supported." << std::endl;
      return EXIT_FAILURE;
    }
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
