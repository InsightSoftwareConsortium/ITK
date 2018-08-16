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

#define DISABLE_SETTING_MONTAGE_DIRECTLY
#include "itkRGBPixel.h"
#include "itkMockMontageHelper.hxx"
#include "itkMontageTestHelper.hxx"

int itkMontageTestRGB(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " <directoryWtihInputData> <mockTSV> <montageTSV>" << std::endl;
    return EXIT_FAILURE;
    }

  using PixelType = itk::RGBPixel<unsigned char>;
  constexpr unsigned Dimension = 2;
  using PointType = itk::Point<double, Dimension>;
  using VectorType = itk::Vector<double, Dimension>;
  using TransformType = itk::TranslationTransform<double, Dimension>;

  constexpr unsigned xMontageSize = 2;
  constexpr unsigned yMontageSize = 2;
  using PositionTableType = std::array<std::array<PointType, xMontageSize>, yMontageSize>;
  using FilenameTableType = std::array<std::array<std::string, xMontageSize>, yMontageSize>;

  PositionTableType stageCoords, actualCoords;
  FilenameTableType filenames;

  //read coordinates from files
  std::ifstream fStage(std::string(argv[1]) + "/StageCoords.txt");
  std::ifstream fActual(std::string(argv[1]) + "/ActualCoords.txt");
  std::string temp;
  std::getline(fStage, temp); //throw away header
  std::getline(fActual, temp); //throw away header

  for (unsigned x = 0; x < xMontageSize; x++)
    {
    for (unsigned y = 0; y < yMontageSize; y++)
      {
      PointType p;
      for (unsigned d = 0; d < Dimension; d++)
        {
        fStage >> p[d];
        }
      stageCoords[y][x] = p;
      for (unsigned d = 0; d < Dimension; d++)
        {
        fActual >> p[d];
        }
      actualCoords[y][x] = p;
      filenames[y][x] = std::string(argv[1]) + "/VisibleHumanMale1608_" + std::to_string(x + 1) + "_" + std::to_string(y + 1) + ".png";
      }
    }

  int r2 = montageTest<PixelType, itk::RGBPixel<unsigned int>, xMontageSize, yMontageSize>(
      stageCoords, actualCoords, filenames, argv[3], true, -1, false, 1);
  int r1 = mockMontageTest<unsigned char, xMontageSize, yMontageSize>(
      stageCoords, actualCoords, filenames, argv[2], false);

  if (r1 == EXIT_FAILURE || r2 == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
