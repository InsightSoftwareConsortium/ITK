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

#define USE_BSPLINE_INTERPOLATOR 0
#define USE_MARCHING_CUBES 0
#define USE_QUAD_EDGE_MESH 1
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


int
CuberilleTest01(int argc, char * argv[])
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

  // Typedefs
  constexpr unsigned int Dimension = 3;
  using PixelType = unsigned char;
  // using PixelType = signed short;
  using ImageType = itk::Image<PixelType, Dimension>;

#if USE_QUAD_EDGE_MESH | USE_DECIMATION
  using MeshType = itk::QuadEdgeMesh<PixelType, Dimension>;
#else
  using MeshType = itk::Mesh<PixelType, Dimension>;
#endif
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
  ImageFileReaderType::Pointer reader = ImageFileReaderType::New();
  reader->SetFileName(filenameInputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->UpdateLargestPossibleRegion());

  ImageType::Pointer input = reader->GetOutput();
  input->DisconnectPipeline();

  // Create output mesh
  MeshType::Pointer outputMesh = nullptr;
  itk::TimeProbe    time;
#if USE_MARCHING_CUBES

  // Create marching cubes mesh
  BinaryThresholdFilterType::Pointer threshold = BinaryThresholdFilterType::New();
  threshold->SetInput(input);
  threshold->SetLowerThreshold(IsoSurfaceValue);
  threshold->SetUpperThreshold(itk::NumericTraits<PixelType>::max());
  threshold->SetInsideValue(itk::NumericTraits<PixelType>::One);
  threshold->SetOutsideValue(itk::NumericTraits<PixelType>::Zero);
  threshold->UpdateLargestPossibleRegion();

  MarchingCubesType::Pointer marching = MarchingCubesType::New();
  marching->SetInput(threshold->GetOutput());

  time.Start();

  ITK_TRY_EXPECT_NO_EXCEPTION(marching->Update());

  time.Stop();

  outputMesh = marching->GetOutput();
  outputMesh->DisconnectPipeline();
#else

  // Create cuberille mesh filter
  CuberilleType::Pointer cuberille = CuberilleType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(cuberille, CuberilleImageToMeshFilter, ImageToMeshFilter);

  // How long does it take to pre-calculate the array labels array?

  itk::TimeProbe labelsArrayTimer;

  labelsArrayTimer.Start();
  cuberille->CalculateLabelsArray();
  labelsArrayTimer.Stop();

  std::cout << "Time to calculate labels array: " << labelsArrayTimer.GetMean() << std::endl;

  cuberille->SetInput(input);

  cuberille->SetIsoSurfaceValue(isoSurfaceValue);
  ITK_TEST_SET_GET_VALUE(isoSurfaceValue, cuberille->GetIsoSurfaceValue());

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
#  if USE_BSPLINE_INTERPOLATOR
  unsigned int              splineOrder = 3;
  interpolator->SetSplineOrder(splineOrder);
#  endif
  cuberille->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, cuberille->GetInterpolator());

  cuberille->SetGenerateTriangleFaces(generateTriangleFaces);
  ITK_TEST_SET_GET_VALUE(generateTriangleFaces, cuberille->GetGenerateTriangleFaces());

  cuberille->SetProjectVerticesToIsoSurface(projectToIsoSurface);
  ITK_TEST_SET_GET_VALUE(projectToIsoSurface, cuberille->GetProjectVerticesToIsoSurface());

  cuberille->SetProjectVertexSurfaceDistanceThreshold(surfaceDistanceThreshold);
  ITK_TEST_SET_GET_VALUE(surfaceDistanceThreshold, cuberille->GetProjectVertexSurfaceDistanceThreshold());

  cuberille->SetProjectVertexStepLength(stepLength);
  ITK_TEST_SET_GET_VALUE(stepLength, cuberille->GetProjectVertexStepLength());

  cuberille->SetProjectVertexStepLengthRelaxationFactor(stepLengthRelax);
  ITK_TEST_SET_GET_VALUE(stepLengthRelax, cuberille->GetProjectVertexStepLengthRelaxationFactor());

  cuberille->SetProjectVertexMaximumNumberOfSteps(maximumNumberOfSteps);
  ITK_TEST_SET_GET_VALUE(maximumNumberOfSteps, cuberille->GetProjectVertexMaximumNumberOfSteps());

  time.Start();

  ITK_TRY_EXPECT_NO_EXCEPTION(cuberille->Update());

  time.Stop();

  outputMesh = cuberille->GetOutput();

  outputMesh->DisconnectPipeline();

#endif

#if USE_DECIMATION
  // Decimation
  using DecimationCriterionType = itk::NumberOfFacesCriterion<MeshType>;
  DecimationCriterionType::Pointer decimateCriterion = DecimationCriterionType::New();
  decimateCriterion->SetTopologicalChange(false);
  decimateCriterion->SetNumberOfElements(2000);

  using DecimationType = itk::QuadricDecimationQuadEdgeMeshFilter<MeshType, MeshType, DecimationCriterionType>;
  DecimationType::Pointer decimate = DecimationType::New();
  decimate->SetCriterion(decimateCriterion);

  decimate->SetInput(outputMesh);

  ITK_TRY_EXPECT_NO_EXCEPTION(decimate->Update());
#endif

  // Write mesh
  MeshFileWriterType::Pointer writer = MeshFileWriterType::New();
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
