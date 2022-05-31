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

#include "yvvFilter.hxx"
#include <fstream>
#include <iostream>

#ifdef GPU
#  include "itkGPUContextManager.h"
#endif

#ifdef WITH_DOUBLE
using PixelType = double;
#else
using PixelType = float;
#endif

#define die(error_msg)                                                                                   \
  std::cerr << "Error: " << error_msg << std::endl;                                                      \
  std::cerr << "Usage: " << argv[0] << " ndimension sigma num_runs width [height] [depth]" << std::endl; \
  return EXIT_FAILURE;

int
itkYvvWhiteImageTest(int argc, char * argv[])
{
#ifdef GPU
  if (!itk::IsGPUAvailable())
  {
    std::cerr << "OpenCL-enabled GPU is not present." << std::endl;
    return EXIT_FAILURE;
  }
#endif

  if (argc < 4)
  {
    die("missing arguments.");
  }

  int dim = std::stoi(argv[1]);
  if (argc < 4 + dim - 1)
  {
    die("missing arguments.");
  }
  if (dim == 3 && argc != 7)
  {
    die("missing arguments for a 3D image.");
  }

  unsigned int ntests;
  auto *       size = new unsigned int[dim];
  float        sigma;
  try
  {
    sigma = std::stod(argv[2]);
    ntests = std::stoi(argv[3]);
    for (int i = 0; i < dim; ++i)
    {
      size[i] = std::stoi(argv[4 + i]);
    }
  }
  catch (...)
  {
    delete[] size;
    die("invalid size arguments.");
  }

  std::cout << ":::  Received: dim =" << dim << ", sigma= " << sigma << ", ntests= " << ntests << ", size= ";
  for (int i = 0; i < dim; ++i)
  {
    std::cout << size[i] << " ";
  }
  std::cout << ":::" << std::endl << std::endl;

  int result;

  itk::TimeProbesCollectorBase timeCollector;
  if (dim == 2)
  {
    using ImageType = itk::Image<PixelType, 2>;
    ImageType::SizeType size2D = { { size[0], size[1] } };
    result = testWhite<ImageType>(size2D, sigma, &timeCollector, ntests);
  }
  else if (dim == 3)
  {
    using ImageType = itk::Image<PixelType, 3>;
    ImageType::SizeType size3D = { { size[0], size[1], size[2] } };
    result = testWhite<ImageType>(size3D, sigma, &timeCollector, ntests);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    result = EXIT_FAILURE;
  }
  timeCollector.Report();

  delete[] size;

#ifndef GPU
  std::cout << "--      ITK GPU support was not detected and/or not configured, so no GPU filters were tested.     --"
            << std::endl;
#endif

  return result;
}
