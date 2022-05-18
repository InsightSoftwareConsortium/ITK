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
#include <fstream>
#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkTransformToDisplacementFieldFilter.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkTransformToDisplacementFieldFilterTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: ";
    std::cerr << itkNameOfTestExecutableMacro(argv)
              << " transformName displacementFieldFileName [bSplineParametersFile]" << std::endl;
    return EXIT_FAILURE;
  }

  std::string transformName = argv[1];
  std::string fileName = argv[2];
  std::string bSplineParametersFile;
  if (argc > 3)
  {
    bSplineParametersFile = argv[3];
  }

  // Typedefs.
  constexpr unsigned int Dimension = 2;
  using ScalarPixelType = float;
  using CoordRepresentationType = double;
  constexpr unsigned int SplineOrder = 3;

  using VectorPixelType = itk::Vector<ScalarPixelType, Dimension>;

  using DisplacementFieldImageType = itk::Image<VectorPixelType, Dimension>;

  using TransformType = itk::Transform<CoordRepresentationType, Dimension, Dimension>;

  using AffineTransformType = itk::AffineTransform<CoordRepresentationType, Dimension>;

  using BSplineTransformType = itk::BSplineTransform<CoordRepresentationType, Dimension, SplineOrder>;

  using ParametersType = TransformType::ParametersType;

  using DisplacementFieldGeneratorType =
    itk::TransformToDisplacementFieldFilter<DisplacementFieldImageType, CoordRepresentationType>;

  using SizeType = DisplacementFieldGeneratorType::SizeType;
  using SpacingType = DisplacementFieldGeneratorType::SpacingType;
  using OriginType = DisplacementFieldGeneratorType::OriginType;
  using IndexType = DisplacementFieldGeneratorType::IndexType;
  using WriterType = itk::ImageFileWriter<DisplacementFieldImageType>;

  // Create output information.
  SizeType size;
  size.Fill(20);
  IndexType index;
  index.Fill(0);
  SpacingType spacing;
  spacing.Fill(0.7);
  OriginType origin;
  origin.Fill(-10.0);

  // Create transforms.
  auto affineTransform = AffineTransformType::New();
  auto bSplineTransform = BSplineTransformType::New();
  if (transformName == "Affine")
  {
    // Set the options.
    OriginType centerOfRotation;
    centerOfRotation[0] = -3.0;
    centerOfRotation[1] = -3.0;
    affineTransform->SetCenter(centerOfRotation);

    // Create and set parameters.
    ParametersType parameters(affineTransform->GetNumberOfParameters());
    parameters[0] = 1.1;
    parameters[1] = 0.1;
    parameters[2] = -0.2;
    parameters[3] = 0.9;
    parameters[4] = 10.3;
    parameters[5] = -33.8;
    affineTransform->SetParameters(parameters);
  }
  else if (transformName == "BSpline")
  {
    // Set the options.

    BSplineTransformType::PhysicalDimensionsType dimensions;
    for (unsigned int d = 0; d < Dimension; ++d)
    {
      dimensions[d] = spacing[d] * (size[d] - 1.0);
    }
    BSplineTransformType::MeshSizeType  meshSize;
    BSplineTransformType::DirectionType direction;
    direction.SetIdentity();

    meshSize[0] = 7 - SplineOrder;
    meshSize[1] = 10 - SplineOrder;

    bSplineTransform->SetTransformDomainOrigin(origin);
    bSplineTransform->SetTransformDomainPhysicalDimensions(dimensions);
    bSplineTransform->SetTransformDomainMeshSize(meshSize);
    bSplineTransform->SetTransformDomainDirection(direction);

    // Create and set parameters.
    ParametersType parameters(bSplineTransform->GetNumberOfParameters());
    std::ifstream  input(bSplineParametersFile.c_str());
    if (input.is_open())
    {
      for (unsigned int i = 0; i < parameters.GetSize(); ++i)
      {
        input >> parameters[i];
      }
      input.close();
    }
    else
    {
      std::cerr << "ERROR: B-spline parameter file not found." << std::endl;
      return EXIT_FAILURE;
    }
    bSplineTransform->SetParametersByValue(parameters);
  }
  else
  {
    std::cerr << "ERROR: Not a valid transform." << std::endl;
    return EXIT_FAILURE;
  }

  // Create an setup displacement field generator.
  auto defGenerator = DisplacementFieldGeneratorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(defGenerator, TransformToDisplacementFieldFilter, ImageSource);


  defGenerator->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, defGenerator->GetSize());

  defGenerator->SetOutputSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, defGenerator->GetOutputSpacing());

  defGenerator->SetOutputOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, defGenerator->GetOutputOrigin());

  defGenerator->SetOutputStartIndex(index);
  ITK_TEST_SET_GET_VALUE(index, defGenerator->GetOutputStartIndex());

  DisplacementFieldGeneratorType::DirectionType direction;
  direction.SetIdentity();
  defGenerator->SetOutputDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, defGenerator->GetOutputDirection());

  if (transformName == "Affine")
  {
    defGenerator->SetTransform(affineTransform);
  }
  else if (transformName == "BSpline")
  {
    defGenerator->SetTransform(bSplineTransform);
  }
  std::cout << "Transform: " << defGenerator->GetTransform() << std::endl;

  // Write displacement field to disk.
  auto writer = WriterType::New();
  writer->SetInput(defGenerator->GetOutput());
  writer->SetFileName(fileName.c_str());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
