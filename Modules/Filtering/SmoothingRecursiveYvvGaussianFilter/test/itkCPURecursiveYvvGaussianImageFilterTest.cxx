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

#include "yvvFilter.hxx"
#include <fstream>
#include <iostream>

#ifdef WITH_DOUBLE
using PixelType = double;
#else
using PixelType = float;
#endif

int
itkCPURecursiveYvvGaussianImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "[Usage] " << argv[0] << " image_file ndimension sigma num_runs" << std::endl;
    return EXIT_FAILURE;
  }

  std::string  inputFilename(argv[1]);
  unsigned int dim = std::stoi(argv[2]);
  float        sigma = std::stod(argv[3]);
  unsigned int ntests = std::stoi(argv[4]);

  int                          res = EXIT_SUCCESS;
  itk::TimeProbesCollectorBase timeCollector;

  if (dim == 2)
  {
    res = testImage<itk::Image<PixelType, 2>>(inputFilename, sigma, &timeCollector, ntests);
  }
  else if (dim == 3)
  {
    res = testImage<itk::Image<PixelType, 3>>(inputFilename, sigma, &timeCollector, ntests);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    res = EXIT_FAILURE;
  }

  timeCollector.Report();
  std::cout << "\n(!) GPU results will only be shown if GPU support has been detected and activated by the user.\n";
  return res;
}
