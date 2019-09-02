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

#include "itkMontageTestHelper.hxx"
#include "itkPairwiseTestHelper.hxx"
#include "itkTileConfiguration.h"
#include "itkRGBPixel.h"

int
itkMontageTest2D(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << argv[0] << " <directoryWithInputData> <montageTSV> <mockTSV>";
    std::cerr << " [ varyPaddingMethods peakMethod loadIntoMemory streamSubdivisions doPairs";
    std::cerr << " writeTransforms allowDrift positionTolerance writeImage ]" << std::endl;
    return EXIT_FAILURE;
  }

  bool varyPaddingMethods = true;
  if (argc > 4)
  {
    varyPaddingMethods = std::stoi(argv[4]);
  }
  int peakMethod = -1;
  if (argc > 5)
  {
    peakMethod = std::stoi(argv[5]);
  }
  bool loadIntoMemory = false;
  if (argc > 6)
  {
    loadIntoMemory = std::stoi(argv[6]);
  }
  unsigned streamSubdivisions = 1;
  if (argc > 7)
  {
    streamSubdivisions = std::stoul(argv[7]);
  }
  bool doPairs = true;
  if (argc > 8)
  {
    doPairs = std::stoi(argv[8]);
  }
  bool writeTransforms = false;
  if (argc > 9)
  {
    writeTransforms = std::stoi(argv[9]);
  }
  bool allowDrift = false;
  if (argc > 10)
  {
    allowDrift = std::stoi(argv[10]);
  }
  unsigned positionTolerance = 0;
  if (argc > 11)
  {
    positionTolerance = std::stoul(argv[11]);
  }
  bool writeImage = true;
  if (argc > 12)
  {
    writeImage = std::stoi(argv[12]);
  }

  std::string inputPath = argv[1];
  if (inputPath.back() != '/' && inputPath.back() != '\\')
  {
    inputPath += '/';
  }

  itk::TileLayout2D stageTiles = itk::ParseTileConfiguration2D(inputPath + "TileConfiguration.txt");
  itk::TileLayout2D actualTiles = itk::ParseTileConfiguration2D(inputPath + "TileConfiguration.registered.txt");

  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
    (inputPath + stageTiles[0][0].FileName).c_str(), itk::ImageIOFactory::FileModeType::ReadMode);
  imageIO->SetFileName(inputPath + stageTiles[0][0].FileName);
  imageIO->ReadImageInformation();
  const itk::ImageIOBase::IOPixelType pixelType = imageIO->GetPixelType();

  int r1, r2 = EXIT_SUCCESS;
  if (pixelType == itk::ImageIOBase::IOPixelType::RGB)
  {
    r1 = montageTest<itk::RGBPixel<unsigned char>, itk::RGBPixel<unsigned int>>(stageTiles,
                                                                                actualTiles,
                                                                                inputPath,
                                                                                argv[2],
                                                                                varyPaddingMethods,
                                                                                peakMethod,
                                                                                loadIntoMemory,
                                                                                streamSubdivisions,
                                                                                writeTransforms,
                                                                                allowDrift,
                                                                                positionTolerance,
                                                                                writeImage);
    if (doPairs)
    {
      r2 = pairwiseTests<unsigned char>(
        stageTiles, actualTiles, inputPath, argv[3], varyPaddingMethods, positionTolerance);
    }
  }
  else
  {
    r1 = montageTest<unsigned short, double>(stageTiles,
                                             actualTiles,
                                             inputPath,
                                             argv[2],
                                             varyPaddingMethods,
                                             peakMethod,
                                             loadIntoMemory,
                                             streamSubdivisions,
                                             writeTransforms,
                                             allowDrift,
                                             positionTolerance,
                                             writeImage);
    if (doPairs)
    {
      r2 = pairwiseTests<unsigned short>(
        stageTiles, actualTiles, inputPath, argv[3], varyPaddingMethods, positionTolerance);
    }
  }

  if (r1 == EXIT_FAILURE || r2 == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
