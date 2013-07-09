#include <iostream>
#include <fstream>
#include "yvvFilter.hxx"

#ifdef GPU
#  include "itkGPUContextManager.h"
#endif

#ifdef WITH_DOUBLE
typedef double PixelType;
#else
typedef float PixelType;
#endif

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
    std::cerr << "Error: missing arguments." << std::endl;
    std::cerr << "Usage: " << argv[0] << " ndimension sigma num_runs width [height] [depth]" << std::endl;
    return EXIT_FAILURE;
  }

  int dim = atoi(argv[1]);
  if (argc < 4 + dim - 1)
  {
    std::cerr << "Error: missing arguments." << std::endl;
    std::cerr << "Usage: " << argv[0] << " ndimension sigma num_runs width [height] [depth]" << std::endl;
    return EXIT_FAILURE;
  }


  unsigned int ntests, size[dim];
  float        sigma;

  if (dim == 3 && argc != 7)
  {
    std::cerr << "Error: missing arguments for a 3D image." << std::endl;
    std::cerr << "Usage: " << argv[0] << " ndimension sigma num_runs width [height] [depth]" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    sigma = atof(argv[2]);
    ntests = atoi(argv[3]);
    for (int i = 0; i < dim; ++i)
    {
      size[i] = atoi(argv[4 + i]);
    }
  }
  catch (...)
  {
    std::cerr << "Error: invalid size arguments." << std::endl;
    std::cerr << "Usage: " << argv[0] << " ndimension sigma num_runs width height [depth]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << ":::  Received: dim =" << dim << ", sigma= " << sigma << ", ntests= " << ntests << ", size= ";
  for (int i = 0; i < dim; ++i)
  {
    std::cout << size[i] << " ";
  }

  std::cout << ":::\n\n";

  int                          res = EXIT_SUCCESS;
  itk::TimeProbesCollectorBase timeCollector;

  if (dim == 2)
  {
    typedef itk::Image<PixelType, 2> ImageType;
    ImageType::SizeType              size2D = { { size[0], size[1] } };
    res = testWhite<ImageType>(size2D, sigma, &timeCollector, ntests);
  }
  else if (dim == 3)
  {
    typedef itk::Image<PixelType, 3> ImageType;
    ImageType::SizeType              size3D = { { size[0], size[1], size[2] } };
    res = testWhite<ImageType>(size3D, sigma, &timeCollector, ntests);
  }
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dim << " selected." << std::endl;
    res = EXIT_FAILURE;
  }

  timeCollector.Report();
#ifndef GPU
  std::cout
    << "--      ITK GPU support was not detected and/or not configured, so no GPU filters were tested.     --\n";
#endif

  return res;
}
