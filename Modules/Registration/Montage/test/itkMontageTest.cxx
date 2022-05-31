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

#include "itkMontageTestHelper.hxx"
#include "itkPairwiseTestHelper.hxx"
#include "itkTileConfiguration.h"
#include "itkRGBPixel.h"
#include "itkNumericTraits.h"

template <typename PixelType, typename AccumulatePixelType, unsigned Dimension>
int
itkMontageTestHelper2(int                               argc,
                      char *                            argv[],
                      const std::string &               inputPath,
                      itk::TileConfiguration<Dimension> stageTiles,
                      itk::TileConfiguration<Dimension> actualTiles)
{
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

  int r1, r2 = EXIT_SUCCESS;
  r1 = montageTest<PixelType, AccumulatePixelType>(stageTiles,
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
    r2 = pairwiseTests<typename itk::NumericTraits<PixelType>::ValueType>(
      stageTiles, actualTiles, inputPath, argv[3], varyPaddingMethods, positionTolerance);
  }

  if (r1 == EXIT_FAILURE || r2 == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

template <unsigned Dimension>
int
itkMontageTestHelper(int argc, char * argv[], const std::string & inputPath)
{
  itk::TileConfiguration<Dimension> stageTiles, actualTiles;
  stageTiles.Parse(inputPath + "TileConfiguration.txt");
  try
  {
    actualTiles.Parse(inputPath + "TileConfiguration.GroundTruth.txt");
  }
  catch (...)
  {
    actualTiles.Parse(inputPath + "TileConfiguration.registered.txt");
  }

  itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
    (inputPath + stageTiles.Tiles[0].FileName).c_str(), itk::IOFileModeEnum::ReadMode);
  imageIO->SetFileName(inputPath + stageTiles.Tiles[0].FileName);
  imageIO->ReadImageInformation();
  const itk::IOPixelEnum     pixelType = imageIO->GetPixelType();
  const itk::IOComponentEnum cType = imageIO->GetComponentType();

  if (pixelType == itk::IOPixelEnum::RGB)
  {
    return itkMontageTestHelper2<itk::RGBPixel<unsigned char>, itk::RGBPixel<unsigned int>>(
      argc, argv, inputPath, stageTiles, actualTiles);
  }
  else if (cType == itk::IOComponentEnum::SHORT)
  {
    return itkMontageTestHelper2<short, double>(argc, argv, inputPath, stageTiles, actualTiles);
  }
  else // cast everything else to USHORT
  {
    return itkMontageTestHelper2<unsigned short, double>(argc, argv, inputPath, stageTiles, actualTiles);
  }
}

int
itkMontageTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << argv[0] << " <directoryWithInputData> <montageTSV> <mockTSV>";
    std::cerr << " [ varyPaddingMethods peakMethod loadIntoMemory streamSubdivisions doPairs";
    std::cerr << " writeTransforms allowDrift positionTolerance writeImage ]" << std::endl;
    return EXIT_FAILURE;
  }

  std::string inputPath = argv[1];
  if (inputPath.back() != '/' && inputPath.back() != '\\')
  {
    inputPath += '/';
  }

  try
  {
    unsigned dim;
    itk::TileConfiguration<2>::TryParse(inputPath + "TileConfiguration.txt", dim);

    switch (dim)
    {
      case 2:
        return itkMontageTestHelper<2>(argc, argv, inputPath);
      case 3:
        return itkMontageTestHelper<3>(argc, argv, inputPath);
      default:
        std::cerr << "Only dimensions 2 and 3 are supported. You are attempting to montage dimension " << dim;
        return EXIT_FAILURE;
    }
  }
  catch (itk::ExceptionObject & exc)
  {
    std::cerr << exc;
  }
  catch (std::runtime_error & exc)
  {
    std::cerr << exc.what();
  }
  catch (...)
  {
    std::cerr << "Unknown error has occurred" << std::endl;
  }
  return EXIT_FAILURE;
}
