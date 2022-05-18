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

#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElement3DC0LinearHexahedronMembrane.h"
#include "itkFEMFiniteDifferenceFunctionLoad.h"
#include "itkImageToRectilinearFEMObjectFilter.h"
#include "itkFEMSolver.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#include <fstream>


// Typedefs used for registration
constexpr unsigned int ImageDimension = 3;
constexpr unsigned int ImageWidth = 16;

using InputImagePixelType = unsigned char;
using DeformationFieldPixelType = float;

using InputImageType = itk::Image<InputImagePixelType, ImageDimension>;
using DeformationFieldVectorType = itk::Vector<DeformationFieldPixelType, ImageDimension>;
using DeformationFieldImageType = itk::Image<DeformationFieldVectorType, ImageDimension>;

constexpr unsigned int PixelsPerElement = 1;
using Element2DType = itk::fem::Element2DC0LinearQuadrilateralMembrane;
using Element3DType = itk::fem::Element3DC0LinearHexahedronMembrane;
using FEMObjectType = itk::fem::FEMObject<ImageDimension>;
using SolverType = itk::fem::Solver<ImageDimension>;
using ImageMetricLoadType = itk::fem::FiniteDifferenceFunctionLoad<InputImageType, InputImageType>;

using MeanSquareRegistrationMetricType =
  itk::MeanSquareRegistrationFunction<InputImageType, InputImageType, DeformationFieldImageType>;
using NCCRegistrationMetricType =
  itk::NCCRegistrationFunction<InputImageType, InputImageType, DeformationFieldImageType>;
using MIRegistrationMetricType = itk::MIRegistrationFunction<InputImageType, InputImageType, DeformationFieldImageType>;
using DemonsRegistrationMetricType =
  itk::DemonsRegistrationFunction<InputImageType, InputImageType, DeformationFieldImageType>;


// Template function to fill in an image with a value.
template <typename TImage>
void
FillImage(TImage * image, typename TImage::PixelType value)
{
  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator it(image, image->GetBufferedRegion());
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    it.Set(value);
  }
}

// Template function to fill in an image with a circle.
template <typename TImage>
void
FillWithCircle(TImage *                   image,
               double *                   center,
               double                     radius,
               typename TImage::PixelType foregnd,
               typename TImage::PixelType backgnd)
{
  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator it(image, image->GetBufferedRegion());

  typename TImage::IndexType index;
  double                     r2 = itk::Math::sqr(radius);
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    index = it.GetIndex();
    double distance = 0;
    for (unsigned int j = 0; j < TImage::ImageDimension; ++j)
    {
      distance += itk::Math::sqr(static_cast<double>(index[j]) - center[j]);
    }
    if (distance <= r2)
    {
      it.Set(foregnd);
    }
    else
    {
      it.Set(backgnd);
    }
  }
}

// Template function to copy image regions
template <typename TImage>
void
CopyImageBuffer(TImage * input, TImage * output)
{
  using Iterator = itk::ImageRegionIteratorWithIndex<TImage>;
  Iterator inIt(input, output->GetBufferedRegion());
  Iterator outIt(output, output->GetBufferedRegion());
  for (inIt.GoToBegin(); !inIt.IsAtEnd(); ++inIt, ++outIt)
  {
    outIt.Set(inIt.Get());
  }
}

FEMObjectType::Pointer
CreateMesh(InputImageType * image, unsigned int elementWidth = 1)
{
  using MaterialType = itk::fem::MaterialLinearElasticity;
  using MeshFilterType = itk::fem::ImageToRectilinearFEMObjectFilter<InputImageType>;

  vnl_vector<unsigned int> pixelsPerElement;
  pixelsPerElement.set_size(ImageDimension);
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    pixelsPerElement[i] = elementWidth;
  }

  // Set up image to mesh filter
  auto meshFilter = MeshFilterType::New();
  meshFilter->SetInput(image);
  meshFilter->SetPixelsPerElement(pixelsPerElement);
  auto material = MaterialType::New();
  if (ImageDimension == 2)
  {
    auto element = Element2DType::New();
    element->SetMaterial(material);
    meshFilter->SetElement(element);
  }
  else
  {
    auto element = Element3DType::New();
    element->SetMaterial(material);
    meshFilter->SetElement(element);
  }
  meshFilter->Update();

  return meshFilter->GetOutput();
}

int
RunTest(InputImageType *            fixedImage,
        InputImageType *            movingImage,
        DeformationFieldImageType * initField,
        DeformationFieldImageType * outField,
        SolverType *                solver,
        unsigned int                metricType,
        std::string                 filenamePrefix)
{
  InputImageType::SpacingType spacing = fixedImage->GetSpacing();
  InputImageType::PointType   origin = fixedImage->GetOrigin();

  auto load = ImageMetricLoadType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(load, FiniteDifferenceFunctionLoad, LoadElement);

  itk::PDEDeformableRegistrationFunction<InputImageType, InputImageType, DeformationFieldImageType>::Pointer metric;

  if (metricType == 0)
  {
    metric = dynamic_cast<MeanSquareRegistrationMetricType *>(MeanSquareRegistrationMetricType::New().GetPointer());
    load->SetDescentDirectionMinimize();
  }
  else if (metricType == 1)
  {
    metric = dynamic_cast<NCCRegistrationMetricType *>(NCCRegistrationMetricType::New().GetPointer());
    load->SetDescentDirectionMinimize();
  }
  else if (metricType == 2)
  {
    metric = dynamic_cast<MIRegistrationMetricType *>(MIRegistrationMetricType::New().GetPointer());
    load->SetDescentDirectionMaximize();
  }
  else if (metricType == 3)
  {
    metric = dynamic_cast<DemonsRegistrationMetricType *>(DemonsRegistrationMetricType::New().GetPointer());
    load->SetDescentDirectionMinimize();
  }
  else
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Unsupported metric type: " << metricType << std::endl;
    std::cout << "Only metrics: "
              << "0: MeanSquareRegistrationFunction" << std::endl
              << "1: NCCRegistrationFunction" << std::endl
              << "2: MIRegistrationFunction" << std::endl
              << "3: DemonsRegistrationFunction" << std::endl
              << "are supported." << std::endl;
    return EXIT_FAILURE;
  }

  if (metric.IsNull())
  {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Cannot create registration function." << std::endl;
    return EXIT_FAILURE;
  }

  // SetMetric() must to be called after SetDisplacementField()!!
  ITK_TRY_EXPECT_EXCEPTION(load->SetMetric(metric));

  ITK_TRY_EXPECT_EXCEPTION(load->InitializeMetric());


  load->SetMovingImage(movingImage);
  ITK_TEST_SET_GET_VALUE(movingImage, load->GetMovingImage());

  load->SetFixedImage(fixedImage);
  ITK_TEST_SET_GET_VALUE(fixedImage, load->GetFixedImage());

  load->SetDisplacementField(initField);
  ITK_TEST_SET_GET_VALUE(initField, load->GetDisplacementField());


  ITK_TRY_EXPECT_NO_EXCEPTION(load->SetMetric(metric));

  ITK_TRY_EXPECT_NO_EXCEPTION(load->InitializeMetric());


  ImageMetricLoadType::RadiusType radius;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    radius[i] = 2;
  }
  load->SetMetricRadius(radius);
  ITK_TEST_SET_GET_VALUE(radius, load->GetMetricRadius());

  ImageMetricLoadType::Float gamma = 1.;
  load->SetGamma(gamma);
  // ITK_TEST_SET_GET_VALUE( gamma, load->GetGamma() );

  unsigned int numberOfIntegrationPoints = 1;
  load->SetNumberOfIntegrationPoints(numberOfIntegrationPoints);
  ITK_TEST_SET_GET_VALUE(numberOfIntegrationPoints, load->GetNumberOfIntegrationPoints());

  load->PrintCurrentEnergy();


  // Test force computations Fe()
  Element2DType::VectorType position, solution;
  position.set_size(ImageDimension);
  solution.set_size(ImageDimension);
  using Iterator = itk::ImageRegionIteratorWithIndex<DeformationFieldImageType>;
  Iterator iter(outField, outField->GetLargestPossibleRegion());
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    // Query the image-based force (function Fe())
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      position[d] = iter.GetIndex()[d] * spacing[d] + origin[d];
      solution[d] = 0;
    }
    solution = load->Fe(position);

    // Write to output displacement field
    DeformationFieldImageType::PixelType pixelVal;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      pixelVal[d] = solution[d];
    }
    iter.Set(pixelVal);
  } // end of for (each pixel in displacement field)

  // Write to vector image
  using FieldWriterType = itk::ImageFileWriter<DeformationFieldImageType>;
  auto fieldWriter = FieldWriterType::New();

  std::ostringstream outFilenameStream;
  outFilenameStream << filenamePrefix << "ForcesWithMetric" << metricType << ".vtk";
  std::string outFilename = outFilenameStream.str();

  fieldWriter->SetFileName(outFilename.c_str());
  fieldWriter->SetInput(outField);
  fieldWriter->Update();


  // Test ApplyLoad() function
  DeformationFieldImageType::PixelType pixelVal;
  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    pixelVal = DeformationFieldImageType::PixelType(0.0);
  }
  outField->FillBuffer(pixelVal);
  const DeformationFieldImageType::RegionType & region = outField->GetLargestPossibleRegion();

  FEMObjectType::Pointer femObject = solver->GetInput();

  itk::fem::LinearSystemWrapper::Pointer femSolution = solver->GetLinearSystemWrapper();
  load->SetSolution(femSolution);
  ITK_TEST_SET_GET_VALUE(femSolution, load->GetSolution());


  Element2DType::VectorType force;
  force.set_size(ImageDimension);
  for (unsigned int i = 0; i < femObject->GetNumberOfElements(); ++i)
  {
    itk::fem::Element * element = femObject->GetElement(i);
    load->ApplyLoad(element, force);
    for (unsigned int n = 0; n < element->GetNumberOfNodes(); ++n)
    {
      // Accumulate to corresponding pixel in displacement field
      DeformationFieldImageType::PointType coords;
      DeformationFieldImageType::IndexType index;
      for (unsigned int d = 0; d < ImageDimension; ++d)
      {
        coords[d] = element->GetNodeCoordinates(n)[d];
      }
      fixedImage->TransformPhysicalPointToIndex(coords, index);
      if (!region.IsInside(index))
      {
        continue;
      }

      pixelVal = outField->GetPixel(index);
      for (unsigned int d = 0; d < ImageDimension; ++d)
      {
        pixelVal[d] += force(n * ImageDimension + d);
      }
      outField->SetPixel(index, pixelVal);

    } // end of for (each node in an element)
  }   // end of for(each element)

  // Write to vector image
  auto forceFieldWriter = FieldWriterType::New();

  std::ostringstream vectorOutFilenameStream;
  vectorOutFilenameStream << filenamePrefix << "NodalForcesWithMetric" << metricType << ".vtk";
  std::string vectorOutFilename = vectorOutFilenameStream.str();

  forceFieldWriter->SetFileName(vectorOutFilename.c_str());
  forceFieldWriter->SetInput(outField);
  forceFieldWriter->Update();

  return EXIT_SUCCESS;
}


int
itkFEMFiniteDifferenceFunctionLoadTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing Parameters" << std::endl;
    std::cerr << "Usage: " << argv[0] << " outputFilenamePrefix" << std::endl;
    return EXIT_FAILURE;
  }

  std::string filenamePrefix = argv[1];

  using IndexType = InputImageType::IndexType;
  using SizeType = InputImageType::SizeType;
  using RegionType = InputImageType::RegionType;
  using SpacingType = InputImageType::SpacingType;


  // Generate input images and initial deformation field

  SpacingType                   spacing;
  float                         origin[ImageDimension];
  InputImageType::SizeValueType sizeArray[ImageDimension];

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    spacing[i] = 0.84;
    origin[i] = i * 25.0 + 50.0; // add some randomness to test robustness
    sizeArray[i] = ImageWidth;
  }

  SizeType size;
  size.SetSize(sizeArray);

  IndexType index;
  index.Fill(0);

  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);

  auto movingImage = InputImageType::New();
  auto fixedImage = InputImageType::New();

  auto initField = DeformationFieldImageType::New();
  auto outField = DeformationFieldImageType::New();

  movingImage->SetLargestPossibleRegion(region);
  movingImage->SetBufferedRegion(region);
  movingImage->Allocate();
  movingImage->SetSpacing(spacing);
  movingImage->SetOrigin(origin);

  fixedImage->SetLargestPossibleRegion(region);
  fixedImage->SetBufferedRegion(region);
  fixedImage->Allocate();
  fixedImage->SetSpacing(spacing);
  fixedImage->SetOrigin(origin);

  initField->SetLargestPossibleRegion(region);
  initField->SetBufferedRegion(region);
  initField->Allocate();
  initField->SetSpacing(spacing);
  initField->SetOrigin(origin);

  outField->SetLargestPossibleRegion(region);
  outField->SetBufferedRegion(region);
  outField->Allocate();
  outField->SetSpacing(spacing);
  outField->SetOrigin(origin);

  double center[ImageDimension];
  double radius;
  for (double & i : center)
  {
    i = ImageWidth / 2.0;
  }

  InputImagePixelType fgnd = 250;
  InputImagePixelType bgnd = 15;

  // Fill fixed image with a circle
  radius = ImageWidth / 2.5;
  FillWithCircle<InputImageType>(fixedImage, center, radius, fgnd, bgnd);

  // Fill moving image with a circle
  radius = ImageWidth / 4.0;
  FillWithCircle<InputImageType>(movingImage, center, radius, fgnd, bgnd);

  // Fill initial deformation with zero vectors
  DeformationFieldVectorType zeroVec;
  zeroVec.Fill(0.0);
  FillImage<DeformationFieldImageType>(initField, zeroVec);

  using ImageWriterType = itk::ImageFileWriter<InputImageType>;
  auto        writer = ImageWriterType::New();
  std::string filename(filenamePrefix);

  writer->SetInput(movingImage);
  writer->SetFileName((filename + "MovingImage.mha").c_str());
  writer->Update();

  writer->SetInput(fixedImage);
  writer->SetFileName((filename + "FixedImage.mha").c_str());
  writer->Update();


  // Create mesh from image
  FEMObjectType::Pointer femObject = CreateMesh(fixedImage, PixelsPerElement);
  auto                   solver = SolverType::New();
  solver->SetInput(femObject);

  // Test FinitDifferenceFunctionLoad with four metric types
  for (unsigned int i = 0; i < 4; ++i)
  {
    if (RunTest(fixedImage, movingImage, initField, outField, solver, i, filename) != EXIT_SUCCESS)
    {
      std::cerr << "Test failed!" << std::endl;
      return EXIT_FAILURE;
    }
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
