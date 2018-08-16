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

#include "itkMockMontageHelper.hxx"
#include "itkMontageTestHelper.hxx"

int itkMontageTestTiles(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " <directoryWtihInputData> <mockTSV> <montageTSV> [NamePrefix]" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned Dimension = 2;
  using PointType = itk::Point<double, Dimension>;
  using VectorType = itk::Vector<double, Dimension>;
  using TransformType = itk::TranslationTransform<double, Dimension>;

  constexpr unsigned xMontageSize = 10;
  constexpr unsigned yMontageSize = 10;
  using PositionTableType = std::array<std::array<PointType, xMontageSize>, yMontageSize>;
  using FilenameTableType = std::array<std::array<std::string, xMontageSize>, yMontageSize>;

  std::string namePrefix = "Image";
  if (argc >= 5)
    {
    namePrefix = argv[4];
    }
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
      filenames[y][x] = std::string(argv[1]) + "/" + namePrefix + "_" + std::to_string(x + 1) + "_" + std::to_string(y + 1) + ".tif";
      }
    }

  //do not vary padding methods, because padding is not required for images in this test 
  int r2 = montageTest<unsigned short, double, xMontageSize, yMontageSize>(
      stageCoords, actualCoords, filenames, argv[3], false, -1, true, 1);
  int r1 = mockMontageTest<unsigned short, xMontageSize, yMontageSize>(
      stageCoords, actualCoords, filenames, argv[2], false);

  if (r1 == EXIT_FAILURE || r2 == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
