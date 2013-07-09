#include <iostream>
#include <fstream>
#include "yvvFilter.hxx"


#ifdef WITH_DOUBLE
typedef double PixelType;
#else
typedef float PixelType;
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
  unsigned int dim = atoi(argv[2]);
  float        sigma = atof(argv[3]);
  unsigned int ntests = atoi(argv[4]);


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
