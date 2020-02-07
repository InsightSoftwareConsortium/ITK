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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkTestingMacros.h"

/**
 * In this test, we approximate a 2-D scalar field.
 * The scattered data is derived from a segmented
 * image. We write the output to an image for
 * comparison.
 */
int
itkBSplineScatteredDataPointSetToImageFilterTest(int argc, char * argv[])
{

  if (argc != 3)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ParametricDimension = 2;
  constexpr unsigned int DataDimension = 1;

  using PixelType = int;
  using InputImageType = itk::Image<PixelType, ParametricDimension>;
  using RealType = float;
  using VectorType = itk::Vector<RealType, DataDimension>;
  using VectorImageType = itk::Image<VectorType, ParametricDimension>;
  using PointSetType = itk::PointSet<VectorImageType::PixelType, ParametricDimension>;

  PointSetType::Pointer pointSet = PointSetType::New();

  using ReaderType = itk::ImageFileReader<InputImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();

  itk::ImageRegionIteratorWithIndex<InputImageType> It(reader->GetOutput(),
                                                       reader->GetOutput()->GetLargestPossibleRegion());

  // Iterate through the input image which consists of multivalued
  // foreground pixels (=nonzero) and background values (=zero).
  // The foreground pixels comprise the input point set.

  for (It.GoToBegin(); !It.IsAtEnd(); ++It)
  {
    if (It.Get() != itk::NumericTraits<PixelType>::ZeroValue())
    {
      // We extract both the 2-D location of the point
      // and the pixel value of that point.

      PointSetType::PointType point;
      reader->GetOutput()->TransformIndexToPhysicalPoint(It.GetIndex(), point);

      unsigned long i = pointSet->GetNumberOfPoints();
      pointSet->SetPoint(i, point);

      PointSetType::PixelType V(DataDimension);
      V[0] = static_cast<RealType>(It.Get());
      pointSet->SetPointData(i, V);
    }
  }

  // Instantiate the B-spline filter and set the desired parameters.
  using FilterType = itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, VectorImageType>;

  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BSplineScatteredDataPointSetToImageFilter, PointSetToImageFilter);


  unsigned int splineOrder = 0u;
  ITK_TRY_EXPECT_EXCEPTION(filter->SetSplineOrder(splineOrder));

  FilterType::ArrayType splineOrderArray;
  splineOrderArray.Fill(4u);
  filter->SetSplineOrder(splineOrderArray);
  ITK_TEST_SET_GET_VALUE(splineOrderArray, filter->GetSplineOrder());

  splineOrder = 3u;
  filter->SetSplineOrder(splineOrder);
  splineOrderArray.Fill(splineOrder);
  ITK_TEST_SET_GET_VALUE(splineOrderArray, filter->GetSplineOrder());


  unsigned numberOfLevels = 0u;
  ITK_TRY_EXPECT_EXCEPTION(filter->SetNumberOfLevels(numberOfLevels));

  FilterType::ArrayType numberOfLevelsArray;
  numberOfLevelsArray.Fill(4u);
  filter->SetNumberOfLevels(numberOfLevelsArray);
  ITK_TEST_SET_GET_VALUE(numberOfLevelsArray, filter->GetNumberOfLevels());

  numberOfLevels = 3u;
  filter->SetNumberOfLevels(numberOfLevels);
  numberOfLevelsArray.Fill(numberOfLevels);
  ITK_TEST_SET_GET_VALUE(numberOfLevelsArray, filter->GetNumberOfLevels());


  FilterType::ArrayType ncps;
  ncps.Fill(4u);
  filter->SetNumberOfControlPoints(ncps);
  ITK_TEST_SET_GET_VALUE(ncps, filter->GetNumberOfControlPoints());


  FilterType::ArrayType close;
  close.Fill(0u);
  filter->SetCloseDimension(close);
  ITK_TEST_SET_GET_VALUE(close, filter->GetCloseDimension());


  // Define the parametric domain.
  filter->SetOrigin(reader->GetOutput()->GetOrigin());
  filter->SetSpacing(reader->GetOutput()->GetSpacing());
  filter->SetSize(reader->GetOutput()->GetLargestPossibleRegion().GetSize());
  filter->SetDirection(reader->GetOutput()->GetDirection());

  filter->SetInput(pointSet);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  // Get the current number of control points to increase coverage
  std::cout << "Current number of control points: " << filter->GetCurrentNumberOfControlPoints() << std::endl;

  // Get the control point lattice produced by the fitting process to increase
  // coverage
  std::cout << "Control point lattice produced by the fitting process: " << std::endl;
  std::cout << filter->GetPhiLattice() << std::endl;

  VectorImageType * outputImage = filter->GetOutput();

  // Write the output to an image.
  using RealImageType = itk::Image<RealType, ParametricDimension>;
  RealImageType::Pointer image = RealImageType::New();
  image->SetRegions(reader->GetOutput()->GetLargestPossibleRegion());
  image->Allocate();
  itk::ImageRegionIteratorWithIndex<RealImageType> Itt(image, image->GetLargestPossibleRegion());

  for (Itt.GoToBegin(); !Itt.IsAtEnd(); ++Itt)
  {
    Itt.Set(outputImage->GetPixel(Itt.GetIndex())[0]);
  }

  using WriterType = itk::ImageFileWriter<RealImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(image);
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
