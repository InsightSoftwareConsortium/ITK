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

#include <stdlib.h>
#include "itkTileConfiguration.h"

template <unsigned Dimension>
int
mainHelper(char * argv[])
{
  itk::TileConfiguration<Dimension> baseline;
  baseline.Parse(argv[1]);
  itk::TileConfiguration<Dimension> test;
  test.Parse(argv[2]);

  if (baseline.AxisSizes != test.AxisSizes)
  {
    std::cout << "TileConfigurations are not the same size\n"
              << "baseline: " << baseline.AxisSizes << "test: " << test.AxisSizes << std::endl;
    return EXIT_FAILURE;
  }

  double error = 0.0;
  for (size_t i = 0; i < baseline.LinearSize(); i++)
  {
    double error1 = (baseline.Tiles[i].Position - test.Tiles[i].Position).GetNorm();
    error += error1;
    if (error1 > 1.0 * Dimension)
    {
      std::cout << "Error at index " << baseline.LinearIndexToNDIndex(i) << " is " << error1 << std::endl;
    }
  }

  error /= (Dimension * baseline.LinearSize());
  std::cout << "Average error per dimension is " << error << std::endl;
  if (error > 0.5)
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <baselineTileConfiguration> <testTileConfiguration>" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    unsigned dim;
    itk::TileConfiguration<2>::TryParse(argv[1], dim);

    switch (dim)
    {
      case 2:
        return mainHelper<2>(argv);
      case 3:
        return mainHelper<3>(argv);
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
