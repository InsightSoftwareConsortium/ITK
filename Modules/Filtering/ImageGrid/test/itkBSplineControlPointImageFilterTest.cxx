/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkBSplineControlPointImageFilter.h"
#include "itkImageToImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


template <unsigned int ImageDimension>
int
BSpline(int argc, char * argv[])
{
  using RealType = float;
  using ScalarPixelType = itk::Vector<RealType, 1>;

  using ScalarFieldType = itk::Image<ScalarPixelType, ImageDimension>;

  using ReaderType = itk::ImageFileReader<ScalarFieldType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  if (argc > 2)
  {
    reader->SetFileName(argv[2]);
    reader->Update();
  }
  else
  {
    std::cerr << "No input image specified." << std::endl;
    return EXIT_FAILURE;
  }

  // Reconstruction of the scalar field from the control points

  typename ScalarFieldType::PointType origin;
  origin.Fill(0);
  typename ScalarFieldType::SizeType size;
  size.Fill(100);
  typename ScalarFieldType::SpacingType spacing;
  spacing.Fill(1.0);

  using BSplinerType = itk::BSplineControlPointImageFilter<ScalarFieldType, ScalarFieldType>;
  typename BSplinerType::Pointer bspliner = BSplinerType::New();

  bspliner->SetInput(reader->GetOutput());

  typename BSplinerType::ArrayType splineOrder = 3;
  bspliner->SetSplineOrder(3);
  ITK_TEST_SET_GET_VALUE(splineOrder, bspliner->GetSplineOrder());

  bspliner->SetSize(size);
  ITK_TEST_SET_GET_VALUE(size, bspliner->GetSize());

  bspliner->SetOrigin(origin);
  ITK_TEST_SET_GET_VALUE(origin, bspliner->GetOrigin());

  bspliner->SetSpacing(spacing);
  ITK_TEST_SET_GET_VALUE(spacing, bspliner->GetSpacing());

  typename BSplinerType::DirectionType direction = reader->GetOutput()->GetDirection();
  bspliner->SetDirection(direction);
  ITK_TEST_SET_GET_VALUE(direction, bspliner->GetDirection());

  try
  {
    bspliner->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  using WriterType = itk::ImageFileWriter<ScalarFieldType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[3]);
  writer->SetInput(bspliner->GetOutput());
  writer->Update();

  //
  // Test out additional functionality by refining the control point lattice
  // and seeing if the output is the same. In this example we double the
  // resolution twice as the refinement is doubled at every level.
  //
  typename BSplinerType::ArrayType numberOfRefinementLevels;
  numberOfRefinementLevels.Fill(3);

  typename BSplinerType::ControlPointLatticeType::Pointer refinedControlPointLattice =
    bspliner->RefineControlPointLattice(numberOfRefinementLevels);

  typename BSplinerType::Pointer bspliner2 = BSplinerType::New();
  bspliner2->SetInput(refinedControlPointLattice);
  bspliner2->SetSplineOrder(3);
  bspliner2->SetSize(size);
  bspliner2->SetOrigin(origin);
  bspliner2->SetSpacing(spacing);
  bspliner2->SetDirection(reader->GetOutput()->GetDirection());

  try
  {
    bspliner2->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  typename WriterType::Pointer writer2 = WriterType::New();
  writer2->SetFileName(argv[4]);
  writer2->SetInput(bspliner2->GetOutput());
  writer2->Update();

  return EXIT_SUCCESS;
}

int
itkBSplineControlPointImageFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " imageDimension inputControlPointImage"
              << " outputSampledBSplineObject outputSampledBSplineObjectRefined" << std::endl;
    exit(EXIT_FAILURE);
  }

  constexpr unsigned int Dimension = 2;
  using RealType = float;
  using ScalarPixelType = itk::Vector<RealType, 1>;

  using ScalarFieldType = itk::Image<ScalarPixelType, Dimension>;

  using BSplinerType = itk::BSplineControlPointImageFilter<ScalarFieldType, ScalarFieldType>;
  BSplinerType::Pointer bspliner = BSplinerType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(bspliner, BSplineControlPointImageFilter, ImageToImageFilter);

  int successOrFailure = EXIT_FAILURE;

  switch (std::stoi(argv[1]))
  {
    case 2:
      successOrFailure = BSpline<2>(argc, argv);
      break;
    case 3:
      successOrFailure = BSpline<3>(argc, argv);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit(EXIT_FAILURE);
  }
  return successOrFailure;
}
