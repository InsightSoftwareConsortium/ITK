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
#include "itkAffineTransform.h"
#include "itkBSplineTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkTransformToStrainFilter.h"
#include "itkImageFileWriter.h"
#include "itkTransformToDisplacementFieldFilter.h"
#include "itkStrainImageFilter.h"
#include "itkTestingMacros.h"

int
itkTransformToStrainFilterTest(int argc, char * argv[])
{
  // Check command line arguments.
  if (argc < 6)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "strainForm"
              << " transformName"
              << " strainFieldFileName"
              << " displacementField"
              << " displacementFieldStrain"
              << " [bSplineParametersFile]" << std::endl;
    return EXIT_FAILURE;
  }


  // Typedefs.
  constexpr unsigned int Dimension = 2;
  using ScalarPixelType = float;
  using CoordRepresentationType = double;
  constexpr unsigned int SplineOrder = 3;

  using TransformType = itk::Transform<CoordRepresentationType, Dimension, Dimension>;
  using ParametersType = TransformType::ParametersType;

  using TransformToStrainFilterType = itk::TransformToStrainFilter<TransformType, ScalarPixelType, ScalarPixelType>;

  TransformToStrainFilterType::Pointer transformToStrainFilter = TransformToStrainFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(transformToStrainFilter, TransformToStrainFilter, GenerateImageSource);


  // Test the unknown strain form exception
  int strainForm = -1;

  transformToStrainFilter->SetStrainForm(static_cast<TransformToStrainFilterType::StrainFormType>(strainForm));

  ITK_TRY_EXPECT_EXCEPTION(transformToStrainFilter->Update());


  // Get the input strain form
  if (!strcmp(argv[1], "INFINITESIMAL"))
  {
    strainForm = 0;
  }
  else if (!strcmp(argv[1], "GREENLAGRANGIAN"))
  {
    strainForm = 1;
  }
  else if (!strcmp(argv[1], "EULERIANALMANSI"))
  {
    strainForm = 2;
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Unknown strain form: " << argv[1] << std::endl;
    return EXIT_FAILURE;
  }

  const std::string transformName = argv[2];
  const std::string strainFieldFileName = argv[3];
  const std::string displacementFieldFileName = argv[4];
  const std::string displacementFieldStrainFileName = argv[5];
  std::string       bSplineParametersFile;
  if (argc > 6)
  {
    bSplineParametersFile = argv[6];
  }

  transformToStrainFilter->SetStrainForm(static_cast<TransformToStrainFilterType::StrainFormType>(strainForm));
  ITK_TEST_SET_GET_VALUE(static_cast<TransformToStrainFilterType::StrainFormType>(strainForm),
                         transformToStrainFilter->GetStrainForm());


  // Create output information.
  using SizeType = TransformToStrainFilterType::SizeType;
  SizeType size;
  size.Fill(20);

  using SpacingType = TransformToStrainFilterType::SpacingType;
  SpacingType spacing;
  spacing.Fill(0.7);

  using OriginType = TransformToStrainFilterType::PointType;
  OriginType origin;
  origin.Fill(-10.0);

  // Create the equivalent strain field by first converting the transform to a
  // displacement field, then computing the strain from that displacement
  // field.
  using DisplacementVectorType = itk::Vector<ScalarPixelType, Dimension>;
  using DisplacementFieldType = itk::Image<DisplacementVectorType, Dimension>;

  using TransformToDisplacementFilterType =
    itk::TransformToDisplacementFieldFilter<DisplacementFieldType, CoordRepresentationType>;
  TransformToDisplacementFilterType::Pointer transformToDisplacement = TransformToDisplacementFilterType::New();

  // Create transforms.
  using AffineTransformType = itk::AffineTransform<CoordRepresentationType, Dimension>;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();

  using BSplineTransformType = itk::BSplineTransform<CoordRepresentationType, Dimension, SplineOrder>;
  BSplineTransformType::Pointer bSplineTransform = BSplineTransformType::New();

  using SimilarityTransformType = itk::Similarity2DTransform<CoordRepresentationType>;
  SimilarityTransformType::Pointer similarityTransform = SimilarityTransformType::New();

  if (transformName == "Similarity")
  {
    transformToStrainFilter->SetTransform(similarityTransform.GetPointer());
    ITK_TEST_SET_GET_VALUE(similarityTransform.GetPointer(), transformToStrainFilter->GetTransform());

    transformToDisplacement->SetTransform(similarityTransform.GetPointer());

    SimilarityTransformType::OffsetType translation;
    translation[0] = -3.0;
    translation[1] = -4.0;
    similarityTransform->Translate(translation);

    similarityTransform->SetAngle(0.2);
    similarityTransform->SetScale(1.025);
  }
  else if (transformName == "Affine")
  {
    transformToStrainFilter->SetTransform(affineTransform.GetPointer());
    ITK_TEST_SET_GET_VALUE(affineTransform.GetPointer(), transformToStrainFilter->GetTransform());

    transformToDisplacement->SetTransform(affineTransform.GetPointer());

    // Set the options.
    OriginType centerOfRotation;
    centerOfRotation[0] = -3.0;
    centerOfRotation[1] = -3.0;
    affineTransform->SetCenter(centerOfRotation);

    // Create and set parameters.
    ParametersType parameters(affineTransform->GetNumberOfParameters());
    // parameters[ 0 ] =   1.1;
    // parameters[ 1 ] =   0.1;
    // parameters[ 2 ] =  -0.2;
    // parameters[ 3 ] =   0.9;
    // parameters[ 4 ] =  10.3;
    // parameters[ 5 ] = -33.8;
    parameters[0] = 1.1;
    parameters[1] = 0.3;
    parameters[2] = 0.3;
    parameters[3] = 1.2;
    parameters[4] = 0.0;
    parameters[5] = 0.0;
    affineTransform->SetParameters(parameters);
  }
  else if (transformName == "BSpline")
  {
    transformToStrainFilter->SetTransform(bSplineTransform);
    ITK_TEST_SET_GET_VALUE(bSplineTransform, transformToStrainFilter->GetTransform());

    transformToDisplacement->SetTransform(bSplineTransform);

    // Set the options.
    BSplineTransformType::PhysicalDimensionsType dimensions;
    for (unsigned int dd = 0; dd < Dimension; ++dd)
    {
      dimensions[dd] = spacing[dd] * (size[dd] - 1.0);
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
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error: B-Spline parameter file not found." << std::endl;
      return EXIT_FAILURE;
    }
    bSplineTransform->SetParametersByValue(parameters);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: Not a valid transform name: " << transformName << std::endl;
    return EXIT_FAILURE;
  }

  // Create and setup strain field generator.
  transformToStrainFilter->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, transformToStrainFilter->GetSize());

  transformToStrainFilter->SetSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, transformToStrainFilter->GetSpacing());

  transformToStrainFilter->SetOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, transformToStrainFilter->GetOrigin());


  transformToDisplacement->SetSize(size);
  transformToDisplacement->SetOutputSpacing(spacing);
  transformToDisplacement->SetOutputOrigin(origin);

  // Write strain field to disk.
  using WriterType = itk::ImageFileWriter<TransformToStrainFilterType::OutputImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(transformToStrainFilter->GetOutput());
  writer->SetFileName(strainFieldFileName);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // Write strain computed from the displacement field.
  using StrainImageFilterType = itk::StrainImageFilter<DisplacementFieldType, CoordRepresentationType>;
  StrainImageFilterType::Pointer strainImageFilter = StrainImageFilterType::New();
  strainImageFilter->SetInput(transformToDisplacement->GetOutput());
  strainImageFilter->SetStrainForm(static_cast<StrainImageFilterType::StrainFormType>(strainForm));
  writer->SetInput(strainImageFilter->GetOutput());
  writer->SetFileName(displacementFieldStrainFileName);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  using DisplacementWriterType = itk::ImageFileWriter<DisplacementFieldType>;
  DisplacementWriterType::Pointer displacementWriter = DisplacementWriterType::New();
  displacementWriter->SetFileName(displacementFieldFileName);
  displacementWriter->SetInput(transformToDisplacement->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(displacementWriter->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
