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
#include <string>
#include "itkHeldIsotropicWavelet.h"
#include "itkVowIsotropicWavelet.h"
#include "itkSimoncelliIsotropicWavelet.h"
#include "itkShannonIsotropicWavelet.h"

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNumberToString.h"
using namespace std;
using namespace itk;
namespace itk
{
namespace Testing
{
// Generate values from a linear space
std::vector<double>
linSpaceForIWFF(double init = 0.0, double end = 1.0, size_t points = 1000)
{
  std::vector<double> w_array(points);
  if (points <= 1)
    throw("linSpace needs more points");
  double interval = (end - init) / (points - 1);
  for (unsigned int i = 0; i < points; ++i)
  {
    w_array[i] = init + interval * i;
  }
  return w_array;
}
} // namespace Testing
} // namespace itk

template <unsigned int N, typename TWaveletFunction>
int
runIsotropicWaveletFrequencyFunctionTest(const std::string &  profileDataRootPath,
                                         const std::string &  outputImage,
                                         const unsigned int & inputBands,
                                         const std::string &  waveletTypeName)
{
  itk::NumberToString<float>            n2s;
  typedef TWaveletFunction              WaveletFunctionType;
  typename WaveletFunctionType::Pointer motherWavelet = WaveletFunctionType::New();
  motherWavelet->SetHighPassSubBands(inputBands);

  double              init = 0.0;
  double              end = 1.0;
  size_t              points = 1000;
  std::vector<double> w_array = itk::Testing::linSpaceForIWFF(init, end, points);
  // Generate profile data for sub-bands and mother wavelet itself
  std::vector<std::vector<double>> subBandsResults;
  for (unsigned int k = 0; k < inputBands + 2; ++k)
  {
    std::vector<double> bandResults;
    for (unsigned int i = 0; i < points; ++i)
    {
      if (k == inputBands + 1) // mother wavelet
        bandResults.push_back(motherWavelet->EvaluateMagnitude(w_array[i]));
      else // bands
        bandResults.push_back(motherWavelet->EvaluateForwardSubBand(w_array[i], k));
    }
    subBandsResults.push_back(bandResults);
  }

  // Write profile.
  std::ofstream ofs(profileDataRootPath + "_" + waveletTypeName + "_" + n2s(inputBands) + ".txt", std::ofstream::out);
  for (unsigned int i = 0; i < points; ++i)
  {
    ofs << w_array[i];
    for (unsigned int k = 0; k < inputBands + 2; ++k)
    {
      ofs << "," << subBandsResults[k][i];
    }
    ofs << "\n";
  }
  ofs.close();
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
  const string       profileDataRootPath = argv[1];
  const string       outputImage = argv[2];
  const unsigned int inputBands = atoi(argv[3]);
  const string       waveletFunction = argv[4];
  unsigned int       dimension = 3;
  if (argc == 6)
  {
    dimension = atoi(argv[5]);
  }

  typedef itk::HeldIsotropicWavelet<>       HeldWavelet;
  typedef itk::VowIsotropicWavelet<>        VowWavelet;
  typedef itk::SimoncelliIsotropicWavelet<> SimoncelliWavelet;
  typedef itk::ShannonIsotropicWavelet<>    ShannonWavelet;
  if (dimension == 2)
  {
    if (waveletFunction == "Held")
      return runIsotropicWaveletFrequencyFunctionTest<2, HeldWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else if (waveletFunction == "Vow")
      return runIsotropicWaveletFrequencyFunctionTest<2, VowWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else if (waveletFunction == "Simoncelli")
      return runIsotropicWaveletFrequencyFunctionTest<2, SimoncelliWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else if (waveletFunction == "Shannon")
      return runIsotropicWaveletFrequencyFunctionTest<2, ShannonWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else
    {
      std::cerr << argv[4] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else if (dimension == 3)
  {
    if (waveletFunction == "Held")
      return runIsotropicWaveletFrequencyFunctionTest<3, HeldWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else if (waveletFunction == "Vow")
      return runIsotropicWaveletFrequencyFunctionTest<3, VowWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else if (waveletFunction == "Simoncelli")
      return runIsotropicWaveletFrequencyFunctionTest<3, SimoncelliWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else if (waveletFunction == "Shannon")
      return runIsotropicWaveletFrequencyFunctionTest<3, ShannonWavelet>(
        profileDataRootPath, outputImage, inputBands, waveletFunction);
    else
    {
      std::cerr << argv[4] << " is an unknown wavelet type " << std::endl;
      return EXIT_FAILURE;
    }
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}
