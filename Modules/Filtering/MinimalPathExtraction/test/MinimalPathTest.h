/*=========================================================================

  Filter:    MinimalPath
  Program:   Insight Segmentation & Registration Toolkit
  Module:    MinimalPathTests.cxx
  Language:  C++
  Date:      2008/03/01
  Version:   2.0

  Portions of this code are covered under the ITK and VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.
=========================================================================*/

#if defined(_MSC_VER)
// Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
#  pragma warning(disable : 4786)
#endif

// General includes
#include <string>
#include <iostream>
#include <iomanip>
#include "itksys/SystemTools.hxx"

// ITK includes
#include "itkNumericTraits.h"
#include "itkTimeProbe.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPolyLineParametricPath.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkArrivalFunctionToPathFilter.h"
#include "itkSpeedFunctionToPathFilter.h"
#include "itkSpeedFunctionPathInformation.h"
#include "itkPathIterator.h"
#include "itkGradientDescentOptimizer.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkIterateNeighborhoodOptimizer.h"
#include "itkTubeSpatialObject.h"
#include "itkTubeSpatialObjectPoint.h"
#include "itkSpatialObjectPoint.h"
#include "itkSpatialObjectWriter.h"

/////////////////////////////////////////////////////////////
// Reads a *.path file and adds the path info to the given filter
template <class PathFilterType, unsigned int VDimension>
int
ReadPathFile(const char * PathFilename, typename PathFilterType::Pointer pathFilter)
{
  // Path file example:
  // Path: [272.00, 128.00] [490.00, 148.00]
  // Path: [272.00, 128.00] [381.00, 001.00]
  // Path: [272.00, 128.00] [002.00, 130.00]
  // Path: [272.00, 128.00] [274.00, 268.00]
  typedef itk::Point<double, VDimension>               PointType;
  typedef itk::SpeedFunctionPathInformation<PointType> PathInfoType;
  typename PathInfoType::Pointer                       info = PathInfoType::New();
  // NOTE: No checking is done on the path file: the user must ensure it is valid!!!
  std::string filename = PathFilename;
  if (!itksys::SystemTools::FileIsFullPath(PathFilename))
  {
    std::string currentDirectory = itksys::SystemTools::GetCurrentWorkingDirectory();
    filename = currentDirectory + "/" + filename;
  }

  std::ifstream file(filename.c_str(), std::ios::in);
  if (!file)
  {
    std::cerr << "Unable to open file ";
    std::cerr << PathFilename;
    std::cerr << " for reading.";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  std::string line;
  bool        has_newline = false;
  while (itksys::SystemTools::GetLineFromStream(file, line, &has_newline))
  {
    if (has_newline)
    {
      itksys::SystemTools::ReplaceString(line, "Path: ", "");
      itksys::SystemTools::ReplaceString(line, " ", "");
      itksys::SystemTools::ReplaceString(line, "[", "");
      std::vector<itksys::String> parts;
      parts = itksys::SystemTools::SplitString(line.c_str(), ']');
      unsigned int numNonNullParts = 0;
      for (unsigned int i = 0; i < parts.size(); i++)
        if (parts[i].length() != 0)
          numNonNullParts++;
      for (unsigned int i = 0; i < numNonNullParts; i++)
      {
        if (parts[i].length() != 0)
        {
          typename PathFilterType::PointType point;
          std::vector<itksys::String>        partsPoint;
          partsPoint = itksys::SystemTools::SplitString(parts[i].c_str(), ',');
          for (unsigned int j = 0; j < partsPoint.size(); j++)
            point[j] = atof(partsPoint[j].c_str());
          if (i == 0)
            info->SetStartPoint(point);
          else if (i == numNonNullParts - 1)
            info->SetEndPoint(point);
          else
            info->AddWayPoint(point);
        }
      }
      pathFilter->AddPathInfo(info);
    }
  }

  return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////////
// Template for SpeedToPath with GradientDescentOptimizer
template <unsigned int VDimension>
int
Test_SpeedToPath_GradientDescent_ND(int argc, char * argv[])
{
  const unsigned int                                              Dimension = VDimension;
  typedef float                                                   PixelType;
  typedef unsigned char                                           OutputPixelType;
  typedef itk::Image<PixelType, Dimension>                        ImageType;
  typedef itk::Image<OutputPixelType, Dimension>                  OutputImageType;
  typedef itk::ImageFileReader<ImageType>                         ReaderType;
  typedef itk::ImageFileWriter<OutputImageType>                   WriterType;
  typedef itk::PolyLineParametricPath<Dimension>                  PathType;
  typedef itk::SpeedFunctionToPathFilter<ImageType, PathType>     PathFilterType;
  typedef typename PathFilterType::CostFunctionType::CoordRepType CoordRepType;
  typedef itk::PathIterator<OutputImageType, PathType>            PathIteratorType;

  try
  {
    // Print header info
    if (argc != 6)
    {
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0];
      std::cerr << " OutputFilename";
      std::cerr << " SpeedFilename";
      std::cerr << " PathFilename";
      std::cerr << " TerminationValue";   // Good default = 2.0
      std::cerr << " NumberOfIterations"; // Good default = 1000
      std::cerr << std::endl;
      return EXIT_FAILURE;
    }

    // Get arguments
    unsigned int argi = 1;
    char *       OutputFilename = argv[argi++];
    char *       SpeedFilename = argv[argi++];
    char *       PathFilename = argv[argi++];
    float        TerminationValue = atof(argv[argi++]);
    unsigned int NumberOfIterations = atoi(argv[argi++]);
    // NOTE: Points will be read from the command line later

    // Read speed function
    std::cout << "Speed: " << SpeedFilename << std::endl;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(SpeedFilename);
    reader->Update();
    typename ImageType::Pointer speed = reader->GetOutput();
    speed->DisconnectPipeline();

    // Create Interpolator
    typedef itk::LinearInterpolateImageFunction<ImageType, CoordRepType> InterpolatorType;
    typename InterpolatorType::Pointer                                   interp = InterpolatorType::New();

    // Create Cost Function
    typename PathFilterType::CostFunctionType::Pointer cost = PathFilterType::CostFunctionType::New();
    cost->SetInterpolator(interp);

    // Create GradientDescentOptimizer
    typedef itk::GradientDescentOptimizer OptimizerType;
    typename OptimizerType::Pointer       optimizer = OptimizerType::New();
    optimizer->SetNumberOfIterations(NumberOfIterations);

    // Create path filter
    typename PathFilterType::Pointer pathFilter = PathFilterType::New();
    pathFilter->SetInput(speed);
    pathFilter->SetCostFunction(cost);
    pathFilter->SetOptimizer(optimizer);
    pathFilter->SetTerminationValue(TerminationValue);

    // Read path file
    if (ReadPathFile<PathFilterType, Dimension>(PathFilename, pathFilter) == EXIT_FAILURE)
    {
      std::cerr << "Failed to read path file: " << PathFilename << std::endl;
      return EXIT_FAILURE;
    }

    // Compute the path
    std::cout << "Computing path..." << std::endl;
    itk::TimeProbe time;
    time.Start();
    pathFilter->Update();
    time.Stop();
    std::cout << std::setprecision(3) << "Path computed in: " << time.GetMean() << " seconds" << std::endl;

    // Allocate output image
    typename OutputImageType::Pointer output = OutputImageType::New();
    output->SetRegions(speed->GetLargestPossibleRegion());
    output->SetSpacing(speed->GetSpacing());
    output->SetOrigin(speed->GetOrigin());
    output->Allocate();
    output->FillBuffer(itk::NumericTraits<OutputPixelType>::Zero);

    // Rasterize path
    for (unsigned int i = 0; i < pathFilter->GetNumberOfOutputs(); i++)
    {
      // Get the path
      typename PathType::Pointer path = pathFilter->GetOutput(i);

      // Check path is valid
      if (path->GetVertexList()->Size() == 0)
      {
        std::cout << "WARNING: Path " << (i + 1) << " contains no points!" << std::endl;
        continue;
      }

      // Iterate path and convert to image
      std::cout << "Rasterizing path..." << std::endl;
      PathIteratorType it(output, path);
      for (it.GoToBegin(); !it.IsAtEnd(); ++it)
      {
        it.Set(itk::NumericTraits<OutputPixelType>::max());
      }
    }

    // Write output
    std::cout << "Output: " << OutputFilename << std::endl;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(OutputFilename);
    writer->SetInput(output);
    writer->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Return
  return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////////
// Template for SpeedToPath with RegularStepGradientDescentOptimizer
template <unsigned int VDimension>
int
Test_SpeedToPath_RegularStepGradientDescent_ND(int argc, char * argv[])
{
  const unsigned int                                              Dimension = VDimension;
  typedef float                                                   PixelType;
  typedef unsigned char                                           OutputPixelType;
  typedef itk::Image<PixelType, Dimension>                        ImageType;
  typedef itk::Image<OutputPixelType, Dimension>                  OutputImageType;
  typedef itk::ImageFileReader<ImageType>                         ReaderType;
  typedef itk::ImageFileWriter<OutputImageType>                   WriterType;
  typedef itk::PolyLineParametricPath<Dimension>                  PathType;
  typedef itk::SpeedFunctionToPathFilter<ImageType, PathType>     PathFilterType;
  typedef typename PathFilterType::CostFunctionType::CoordRepType CoordRepType;
  typedef itk::PathIterator<OutputImageType, PathType>            PathIteratorType;

  try
  {
    // Print header info
    if (argc != 8)
    {
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0];
      std::cerr << " OutputFilename";
      std::cerr << " SpeedFilename";
      std::cerr << " PathFilename";
      std::cerr << " TerminationValue";   // Good default = 2.0
      std::cerr << " NumberOfIterations"; // Good default = 1000
      std::cerr << " StepLengthFactor";   // Good default = 1.0
      std::cerr << " StepLengthRelax";    // Good default = 0.999
      std::cerr << std::endl;
      return EXIT_FAILURE;
    }

    // Get arguments
    unsigned int argi = 1;
    char *       OutputFilename = argv[argi++];
    char *       SpeedFilename = argv[argi++];
    char *       PathFilename = argv[argi++];
    float        TerminationValue = atof(argv[argi++]);
    unsigned int NumberOfIterations = atoi(argv[argi++]);
    float        StepLengthFactor = atof(argv[argi++]);
    float        StepLengthRelax = atof(argv[argi++]);
    // NOTE: Points will be read from the command line later

    // Read speed function
    std::cout << "Speed: " << SpeedFilename << std::endl;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(SpeedFilename);
    reader->Update();
    typename ImageType::Pointer speed = reader->GetOutput();
    speed->DisconnectPipeline();

    // Compute the minimum spacing
    typename ImageType::SpacingType spacing = speed->GetSpacing();
    double                          minspacing = spacing[0];
    for (unsigned int dim = 0; dim < Dimension; dim++)
      if (spacing[dim] < minspacing)
        minspacing = spacing[dim];

    // Create Interpolator
    typedef itk::LinearInterpolateImageFunction<ImageType, CoordRepType> InterpolatorType;
    typename InterpolatorType::Pointer                                   interp = InterpolatorType::New();

    // Create Cost Function
    typename PathFilterType::CostFunctionType::Pointer cost = PathFilterType::CostFunctionType::New();
    cost->SetInterpolator(interp);

    // Create RegularStepGradientDescentOptimizer
    typedef itk::RegularStepGradientDescentOptimizer OptimizerType;
    typename OptimizerType::Pointer                  optimizer = OptimizerType::New();
    optimizer->SetNumberOfIterations(NumberOfIterations);
    optimizer->SetMaximumStepLength(1.0 * StepLengthFactor * minspacing);
    optimizer->SetMinimumStepLength(0.5 * StepLengthFactor * minspacing);
    optimizer->SetRelaxationFactor(StepLengthRelax);

    // Create path filter
    typename PathFilterType::Pointer pathFilter = PathFilterType::New();
    pathFilter->SetInput(speed);
    pathFilter->SetCostFunction(cost);
    pathFilter->SetOptimizer(optimizer);
    pathFilter->SetTerminationValue(TerminationValue);

    // Read path file
    if (ReadPathFile<PathFilterType, Dimension>(PathFilename, pathFilter) == EXIT_FAILURE)
    {
      std::cerr << "Failed to read path file: " << PathFilename << std::endl;
      return EXIT_FAILURE;
    }

    // Compute the path
    std::cout << "Computing path..." << std::endl;
    itk::TimeProbe time;
    time.Start();
    pathFilter->Update();
    time.Stop();
    std::cout << std::setprecision(3) << "Path computed in: " << time.GetMean() << " seconds" << std::endl;

    // Allocate output image
    typename OutputImageType::Pointer output = OutputImageType::New();
    output->SetRegions(speed->GetLargestPossibleRegion());
    output->SetSpacing(speed->GetSpacing());
    output->SetOrigin(speed->GetOrigin());
    output->Allocate();
    output->FillBuffer(itk::NumericTraits<OutputPixelType>::Zero);

    // Rasterize path
    for (unsigned int i = 0; i < pathFilter->GetNumberOfOutputs(); i++)
    {
      // Get the path
      typename PathType::Pointer path = pathFilter->GetOutput(i);

      // Check path is valid
      if (path->GetVertexList()->Size() == 0)
      {
        std::cout << "WARNING: Path " << (i + 1) << " contains no points!" << std::endl;
        continue;
      }

      // Iterate path and convert to image
      std::cout << "Rasterizing path..." << std::endl;
      PathIteratorType it(output, path);
      for (it.GoToBegin(); !it.IsAtEnd(); ++it)
      {
        it.Set(itk::NumericTraits<OutputPixelType>::max());
      }
    }

    // Write output
    std::cout << "Output: " << OutputFilename << std::endl;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(OutputFilename);
    writer->SetInput(output);
    writer->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Return
  return EXIT_SUCCESS;
}

/////////////////////////////////////////////////////////////
// Template for SpeedToPath with IterateNeighborhoodOptimizer
template <unsigned int VDimension>
int
Test_SpeedToPath_IterateNeighborhood_ND(int argc, char * argv[])
{
  const unsigned int                                              Dimension = VDimension;
  typedef float                                                   PixelType;
  typedef unsigned char                                           OutputPixelType;
  typedef itk::Image<PixelType, Dimension>                        ImageType;
  typedef itk::Image<OutputPixelType, Dimension>                  OutputImageType;
  typedef itk::ImageFileReader<ImageType>                         ReaderType;
  typedef itk::ImageFileWriter<OutputImageType>                   WriterType;
  typedef itk::PolyLineParametricPath<Dimension>                  PathType;
  typedef itk::SpeedFunctionToPathFilter<ImageType, PathType>     PathFilterType;
  typedef typename PathFilterType::CostFunctionType::CoordRepType CoordRepType;
  typedef itk::PathIterator<OutputImageType, PathType>            PathIteratorType;

  try
  {
    // Print header info
    if (argc != 6)
    {
      std::cerr << "Usage: " << std::endl;
      std::cerr << argv[0];
      std::cerr << " OutputFilename";
      std::cerr << " SpeedFilename";
      std::cerr << " PathFilename";
      std::cerr << " TerminationValue"; // Good default = 2.0
      std::cerr << " StepLengthFactor"; // Good default = 1.0
      std::cerr << std::endl;
      return EXIT_FAILURE;
    }

    // Get arguments
    unsigned int argi = 1;
    char *       OutputFilename = argv[argi++];
    char *       SpeedFilename = argv[argi++];
    char *       PathFilename = argv[argi++];
    float        TerminationValue = atof(argv[argi++]);
    float        StepLengthFactor = atof(argv[argi++]);
    // NOTE: Points will be read from the command line later

    // Read speed function
    std::cout << "Speed: " << SpeedFilename << std::endl;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(SpeedFilename);
    reader->Update();
    typename ImageType::Pointer speed = reader->GetOutput();
    speed->DisconnectPipeline();

    // Create Interpolator
    typedef itk::LinearInterpolateImageFunction<ImageType, CoordRepType> InterpolatorType;
    typename InterpolatorType::Pointer                                   interp = InterpolatorType::New();

    // Create Cost Function
    typename PathFilterType::CostFunctionType::Pointer cost = PathFilterType::CostFunctionType::New();
    cost->SetInterpolator(interp);

    // Create IterateNeighborhoodOptimizer
    typedef itk::IterateNeighborhoodOptimizer OptimizerType;
    typename OptimizerType::Pointer           optimizer = OptimizerType::New();
    optimizer->MinimizeOn();
    optimizer->FullyConnectedOn();
    typename OptimizerType::NeighborhoodSizeType size(Dimension);
    for (unsigned int i = 0; i < Dimension; i++)
      size[i] = speed->GetSpacing()[i] * StepLengthFactor;
    optimizer->SetNeighborhoodSize(size);

    // Create path filter
    typename PathFilterType::Pointer pathFilter = PathFilterType::New();
    pathFilter->SetInput(speed);
    pathFilter->SetCostFunction(cost);
    pathFilter->SetOptimizer(optimizer);
    pathFilter->SetTerminationValue(TerminationValue);

    // Read path file
    if (ReadPathFile<PathFilterType, Dimension>(PathFilename, pathFilter) == EXIT_FAILURE)
    {
      std::cerr << "Failed to read path file: " << PathFilename << std::endl;
      return EXIT_FAILURE;
    }

    // Compute the path
    std::cout << "Computing path..." << std::endl;
    itk::TimeProbe time;
    time.Start();
    pathFilter->Update();
    time.Stop();
    std::cout << std::setprecision(3) << "Path computed in: " << time.GetMean() << " seconds" << std::endl;

    // Allocate output image
    typename OutputImageType::Pointer output = OutputImageType::New();
    output->SetRegions(speed->GetLargestPossibleRegion());
    output->SetSpacing(speed->GetSpacing());
    output->SetOrigin(speed->GetOrigin());
    output->Allocate();
    output->FillBuffer(itk::NumericTraits<OutputPixelType>::Zero);

    // Rasterize path
    for (unsigned int i = 0; i < pathFilter->GetNumberOfOutputs(); i++)
    {
      // Get the path
      typename PathType::Pointer path = pathFilter->GetOutput(i);

      // Check path is valid
      if (path->GetVertexList()->Size() == 0)
      {
        std::cout << "WARNING: Path " << (i + 1) << " contains no points!" << std::endl;
        continue;
      }

      // Iterate path and convert to image
      std::cout << "Rasterizing path..." << std::endl;
      PathIteratorType it(output, path);
      for (it.GoToBegin(); !it.IsAtEnd(); ++it)
      {
        it.Set(itk::NumericTraits<OutputPixelType>::max());
      }
    }

    // Write output
    std::cout << "Output: " << OutputFilename << std::endl;
    typename WriterType::Pointer writer = WriterType::New();
    writer->SetFileName(OutputFilename);
    writer->SetInput(output);
    writer->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Return
  return EXIT_SUCCESS;
}
