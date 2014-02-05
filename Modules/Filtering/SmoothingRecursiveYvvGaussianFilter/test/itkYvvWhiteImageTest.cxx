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

  int dim = atoi(argv[1]);
  if (argc < 4 + dim - 1)
  {
    die("missing arguments.");
  }
  if (dim == 3 && argc != 7)
  {
    die("missing arguments for a 3D image.");
  }

  unsigned int   ntests;
  unsigned int * size = new unsigned int[dim];
  float          sigma;
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
    typedef itk::Image<PixelType, 2> ImageType;
    ImageType::SizeType              size2D = { { size[0], size[1] } };
    result = testWhite<ImageType>(size2D, sigma, &timeCollector, ntests);
  }
  else if (dim == 3)
  {
    typedef itk::Image<PixelType, 3> ImageType;
    ImageType::SizeType              size3D = { { size[0], size[1], size[2] } };
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
