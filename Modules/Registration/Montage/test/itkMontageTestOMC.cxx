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

int itkMontageTestOMC(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " <directoryWtihInputData> <mockTSV> <montageTSV>" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned Dimension = 2;
  using PointType = itk::Point<double, Dimension>;
  using VectorType = itk::Vector<double, Dimension>;
  using TransformType = itk::TranslationTransform<double, Dimension>;

  constexpr unsigned xMontageSize = 3;
  constexpr unsigned yMontageSize = 3;
  using PositionTableType = std::array<std::array<PointType, xMontageSize>, yMontageSize>;
  using FilenameTableType = std::array<std::array<std::string, xMontageSize>, yMontageSize>;

  PositionTableType stageCoords, actualCoords;
  FilenameTableType filenames;

  //read coordinates from files
  std::ifstream fStage(std::string(argv[1]) + "/TileConfiguration.txt");
  std::ifstream fActual(std::string(argv[1]) + "/TileConfiguration.registered.txt");
  std::string temp;
  std::getline(fStage, temp); //throw away header
  std::getline(fStage, temp); //throw away header
  std::getline(fStage, temp); //throw away header
  std::getline(fStage, temp); //throw away header
  std::getline(fActual, temp); //throw away header
  std::getline(fActual, temp); //throw away header
  std::getline(fActual, temp); //throw away header
  std::getline(fActual, temp); //throw away header

  for (unsigned y = 0; y < yMontageSize; y++)
    {
    for (unsigned x = 0; x < xMontageSize; x++)
      {
      std::getline(fStage, temp, ';');
      filenames[y][x] = std::string(argv[1]) + std::string("/") + temp;
      std::getline(fActual, temp, ';');
      itkAssertOrThrowMacro(filenames[y][x] == std::string(argv[1]) + std::string("/") + temp,
          "Filenames in TileConfiguration.txt and TileConfiguration.registered.txt must match!"
          << " Problem at y=" << y << " and x=" << x);
      std::getline(fStage, temp, '(');
      std::getline(fActual, temp, '(');

      PointType p;
      fStage >> p[0];
      fStage.ignore();
      fStage >> p[1];
      stageCoords[y][x] = p;
      std::getline(fStage, temp); //throw away rest of line

      fActual >> p[0];
      fActual.ignore();
      fActual >> p[1];
      actualCoords[y][x] = p;
      std::getline(fActual, temp); //throw away rest of line
      }
    }

  int r2 = montageTest<unsigned short, double, xMontageSize, yMontageSize>(
      stageCoords, actualCoords, filenames, argv[3], true, -1, false, 1);
  int r1 = mockMontageTest<unsigned short, xMontageSize, yMontageSize>(
      stageCoords, actualCoords, filenames, argv[2], true);

  if (r1 == EXIT_FAILURE || r2 == EXIT_FAILURE)
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
