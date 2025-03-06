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

#define USE_BSPLINE_INTERPOLATOR 0
#define USE_MARCHING_CUBES 0
#define USE_DECIMATION 0

#include "itkTimeProbe.h"
#include "itkImage.h"
#include "itkMesh.h"
#include "itkQuadEdgeMesh.h"
#include "itkCuberilleImageToMeshFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVTKPolyDataWriter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkQuadricDecimationQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshDecimationCriteria.h"
#include "itkTestingMacros.h"

template <typename ImageType, typename MeshType>
int
CuberilleTest01Helper(int argc, char * argv[])
{

  if (argc < 6)
  {
    std::cout << "Usage: " << argv[0];
    std::cout << "InputImage OutputMesh IsoSurfaceValue ExpectedNumberOfPoints ExpectedNumberOfCells";
    std::cout << "[GenerateTriangleFaces] [ProjectToIsoSurface] ";
    std::cout << "[SurfaceDistanceThreshold] [StepLength] [StepLengthRelax] [MaximumNumberOfSteps]";
    std::cout << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = typename ImageType::PixelType;
  using ImageFileReaderType = itk::ImageFileReader<ImageType>;
  using MeshFileWriterType = itk::VTKPolyDataWriter<MeshType>;
#if USE_BSPLINE_INTERPOLATOR
  using InterpolatorType = itk::BSplineInterpolateImageFunction<ImageType, float, float>;
#else
  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType>;
#endif
  using CuberilleType = itk::CuberilleImageToMeshFilter<ImageType, MeshType, InterpolatorType>;


  // Read command-line parameters
  int          arg = 1;
  char *       filenameInputImage = argv[arg++];
  char *       filenameOutputMesh = argv[arg++];
  PixelType    isoSurfaceValue = atoi(argv[arg++]);
  unsigned int expectedNumberOfPoints = atoi(argv[arg++]);
  unsigned int expectedNumberOfCells = atoi(argv[arg++]);

  bool generateTriangleFaces = true;
  if (argc > arg)
  {
    generateTriangleFaces = atoi(argv[arg++]);
  }

  bool projectToIsoSurface = true;
  if (argc > arg)
  {
    projectToIsoSurface = atoi(argv[arg++]);
  }

  double surfaceDistanceThreshold = 0.5;
  if (argc > arg)
  {
    surfaceDistanceThreshold = atof(argv[arg++]);
  }

  double stepLength = 0.25;
  if (argc > arg)
  {
    stepLength = atof(argv[arg++]);
  }

  double stepLengthRelax = 0.95;
  if (argc > arg)
  {
    stepLengthRelax = atof(argv[arg++]);
  }

  unsigned int maximumNumberOfSteps = 50;
  if (argc > arg)
  {
    maximumNumberOfSteps = atoi(argv[arg++]);
  }

  // Read input image
  const auto reader = ImageFileReaderType::New();
  reader->SetFileName(filenameInputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->UpdateLargestPossibleRegion());

  typename ImageType::Pointer input = reader->GetOutput();
  input->DisconnectPipeline();

  // Create output mesh
  typename MeshType::Pointer outputMesh = nullptr;
  itk::TimeProbe             time;
#if USE_MARCHING_CUBES

  // Create marching cubes mesh
  const auto threshold = BinaryThresholdFilterType::New();
  threshold->SetInput(input);
  threshold->SetLowerThreshold(IsoSurfaceValue);
  threshold->SetUpperThreshold(itk::NumericTraits<PixelType>::max());
  threshold->SetInsideValue(itk::NumericTraits<PixelType>::One);
  threshold->SetOutsideValue(itk::NumericTraits<PixelType>::Zero);
  threshold->UpdateLargestPossibleRegion();

  const auto marching = MarchingCubesType::New();
  marching->SetInput(threshold->GetOutput());

  time.Start();

  ITK_TRY_EXPECT_NO_EXCEPTION(marching->Update());

  time.Stop();

  outputMesh = marching->GetOutput();
  outputMesh->DisconnectPipeline();
#else

  // Create cuberille mesh filter
  const auto cuberille = CuberilleType::New();

  // How long does it take to pre-calculate the array labels array?

  itk::TimeProbe labelsArrayTimer;

  labelsArrayTimer.Start();
  cuberille->CalculateLabelsArray();
  labelsArrayTimer.Stop();

  std::cout << "Time to calculate labels array: " << labelsArrayTimer.GetMean() << std::endl;

  cuberille->SetInput(input);

  cuberille->SetIsoSurfaceValue(isoSurfaceValue);
  ITK_TEST_SET_GET_VALUE(isoSurfaceValue, cuberille->GetIsoSurfaceValue());

  const auto interpolator = InterpolatorType::New();
#  if USE_BSPLINE_INTERPOLATOR
  unsigned int splineOrder = 3;
  interpolator->SetSplineOrder(splineOrder);
#  endif
  cuberille->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, cuberille->GetInterpolator());

  cuberille->SetGenerateTriangleFaces(generateTriangleFaces);
  ITK_TEST_SET_GET_BOOLEAN(cuberille, GenerateTriangleFaces, generateTriangleFaces);

  cuberille->SetProjectVerticesToIsoSurface(projectToIsoSurface);
  ITK_TEST_SET_GET_BOOLEAN(cuberille, ProjectVerticesToIsoSurface, projectToIsoSurface);

  cuberille->SetProjectVertexSurfaceDistanceThreshold(surfaceDistanceThreshold);
  ITK_TEST_SET_GET_VALUE(surfaceDistanceThreshold, cuberille->GetProjectVertexSurfaceDistanceThreshold());

  cuberille->SetProjectVertexStepLength(stepLength);
  ITK_TEST_SET_GET_VALUE(stepLength, cuberille->GetProjectVertexStepLength());

  cuberille->SetProjectVertexStepLengthRelaxationFactor(stepLengthRelax);
  ITK_TEST_SET_GET_VALUE(stepLengthRelax, cuberille->GetProjectVertexStepLengthRelaxationFactor());

  cuberille->SetProjectVertexMaximumNumberOfSteps(maximumNumberOfSteps);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfSteps, cuberille->GetProjectVertexMaximumNumberOfSteps());

  ITK_TEST_SET_GET_BOOLEAN(cuberille, SavePixelAsCellData, false);

  time.Start();

  ITK_TRY_EXPECT_NO_EXCEPTION(cuberille->Update());

  time.Stop();

  outputMesh = cuberille->GetOutput();

  outputMesh->DisconnectPipeline();

#endif

#if USE_DECIMATION
  // Decimation
  using DecimationCriterionType = itk::NumberOfFacesCriterion<MeshType>;
  const auto decimateCriterion = DecimationCriterionType::New();
  decimateCriterion->SetTopologicalChange(false);
  decimateCriterion->SetNumberOfElements(2000);

  using DecimationType = itk::QuadricDecimationQuadEdgeMeshFilter<MeshType, MeshType, DecimationCriterionType>;
  const auto decimate = DecimationType::New();
  decimate->SetCriterion(decimateCriterion);

  decimate->SetInput(outputMesh);

  ITK_TRY_EXPECT_NO_EXCEPTION(decimate->Update());
#endif

  // Write mesh
  const auto writer = MeshFileWriterType::New();
#if USE_DECIMATION
  writer->SetInput(decimate->GetOutput());
#else
  writer->SetInput(outputMesh);
#endif
  writer->SetFileName(filenameOutputMesh);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Assert number of points/cells
  std::cout << "Polygonization took " << time.GetMean() << " seconds" << std::endl;
  std::cout << "Mesh has " << outputMesh->GetNumberOfPoints() << " vertices ";
  std::cout << "and " << outputMesh->GetNumberOfCells() << " cells" << std::endl;
  if (expectedNumberOfPoints > 0 && outputMesh->GetNumberOfPoints() != expectedNumberOfPoints)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: Expected mesh with " << expectedNumberOfPoints << " points, but found "
              << outputMesh->GetNumberOfPoints() << std::endl;
    return EXIT_FAILURE;
  }
  if (expectedNumberOfCells > 0 && outputMesh->GetNumberOfCells() != expectedNumberOfCells)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: Expected mesh with " << expectedNumberOfCells << " cells, but found "
              << outputMesh->GetNumberOfCells() << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

int
CuberilleTest01(int argc, char * argv[])
{

  constexpr unsigned int Dimension = 3;
  using PixelType = unsigned char;
  using CoordinateType = double;
  using QEMeshType = itk::QuadEdgeMesh<CoordinateType, Dimension>;
  using MeshType = itk::Mesh<CoordinateType, Dimension>;
  using ImageType = itk::Image<PixelType, Dimension>;

  {
    using CuberilleType = itk::CuberilleImageToMeshFilter<ImageType, MeshType>;
    const auto cuberille = CuberilleType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(cuberille, CuberilleImageToMeshFilter, ImageToMeshFilter);
  }

  {
    using CuberilleType = itk::CuberilleImageToMeshFilter<ImageType, QEMeshType>;
    const auto cuberille = CuberilleType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(cuberille, CuberilleImageToMeshFilter, ImageToMeshFilter);
  }

  if (EXIT_FAILURE == CuberilleTest01Helper<ImageType, QEMeshType>(argc, argv))
  {
    return EXIT_FAILURE;
  }
  if (EXIT_FAILURE == CuberilleTest01Helper<ImageType, MeshType>(argc, argv))
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
