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
/** This test applies a transform with the ResampleImageFilter, then creates
 * a deformation field from the transform, and uses the WarpImageFilter and
 * compares the two results to ensure that the same answer is found in both cases.
 * This test was written by Yong Quaing Zhao in order to address bug
 * 0008930http://public.kitware.com/Bug/view.php?id=8930
 */

#include <fstream>
#include "itkTransformToDisplacementFieldFilter.h"
#include "itkImageFileWriter.h"
#include "itkEuler3DTransform.h"
#include "itkResampleImageFilter.h"
#include "itkWarpImageFilter.h"

int
itkTransformToDisplacementFieldFilterTest1(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: ";
    std::cerr << argv[0] << "<resampledImageFileName> <displacementFieldFileName>" << std::endl;
    return EXIT_FAILURE;
  }
  const char * resampledImageFileName = argv[1];
  const char * displacementFieldFileName = argv[2];

  /** Typedefs. */
  constexpr unsigned int Dimension = 3;
  using ScalarPixelType = float;
  using CoordRepresentationType = double;

  using ImageType = itk::Image<ScalarPixelType, Dimension>;
  using VectorPixelType = itk::Vector<ScalarPixelType, Dimension>;

  using DisplacementFieldImageType = itk::Image<VectorPixelType, Dimension>;

  using TransformType = itk::Euler3DTransform<CoordRepresentationType>;

  using ParametersType = TransformType::ParametersType;

  using DisplacementFieldGeneratorType =
    itk::TransformToDisplacementFieldFilter<DisplacementFieldImageType, CoordRepresentationType>;
  using WarpImageType = itk::WarpImageFilter<ImageType, ImageType, DisplacementFieldImageType>;

  using SizeType = ImageType::SizeType;
  using SpacingType = ImageType::SpacingType;
  using PointType = ImageType::PointType;
  using IndexType = ImageType::IndexType;
  using RegionType = ImageType::RegionType;
  using DirectionType = ImageType::DirectionType;


  /** Create input image. */
  SizeType size;
  size.Fill(24);
  IndexType index;
  index.Fill(0);
  SpacingType spacing;
  spacing[0] = 1.1;
  spacing[1] = 2.2;
  spacing[2] = 3.3;
  PointType origin;
  origin[0] = 10;
  origin[1] = 20;
  origin[2] = -30;
  DirectionType inputDirection;
  inputDirection[0][0] = -1;
  inputDirection[0][1] = 0;
  inputDirection[0][2] = 0;

  inputDirection[1][0] = 0;
  inputDirection[1][1] = 0;
  inputDirection[1][2] = 1;

  inputDirection[2][0] = 0;
  inputDirection[2][1] = -1;
  inputDirection[2][2] = 0;

  RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  ImageType::Pointer image = ImageType::New();
  image->SetRegions(region);
  image->Allocate();
  image->SetSpacing(spacing);
  image->SetOrigin(origin);
  image->SetDirection(inputDirection);
  image->FillBuffer(itk::NumericTraits<ScalarPixelType>::ZeroValue());

  float     incrValue = 100.0;
  IndexType pixelIndex;
  for (unsigned int i = 0; i < size[0]; i++)
  {
    for (unsigned int j = 0; j < size[1]; j++)
    {
      for (unsigned int k = 0; k < size[2]; k++)
      {
        pixelIndex[0] = i;
        pixelIndex[1] = j;
        pixelIndex[2] = k;
        if ((i > 4) && (i < 20))
        {
          if (j > 6 && j < 18)
          {
            image->SetPixel(pixelIndex, incrValue);
            incrValue += 1;
          }
        }
        else
        {
          image->SetPixel(pixelIndex, 0.0);
        }
      }
    }
  }

  /** Set Output information. */
  IndexType outputIndex;
  outputIndex.Fill(0);
  SpacingType outputSpacing;
  SizeType    outputSize;
  outputSize.Fill(24);
  RegionType outputRegion;
  outputRegion.SetSize(outputSize);
  outputRegion.SetIndex(outputIndex);
  outputSpacing[0] = 1.0;
  outputSpacing[1] = 2.0;
  outputSpacing[2] = 3.0;
  PointType outputOrigin;
  outputOrigin[0] = 50;
  outputOrigin[1] = 30;
  outputOrigin[2] = -60;
  DirectionType outputDirection = inputDirection;

  /** Create transforms. */
  TransformType::Pointer eulerTransform = TransformType::New();
  {
    /** Set the options. */
    IndexType imageCenter;
    imageCenter.Fill(11);
    PointType centerPoint;
    image->TransformIndexToPhysicalPoint(imageCenter, centerPoint);
    eulerTransform->SetCenter(centerPoint);

    /** Create and set parameters. */
    ParametersType parameters(eulerTransform->GetNumberOfParameters());
    parameters[0] = 9.0 * (itk::Math::pi) / 180.0;
    parameters[1] = 6.0 * (itk::Math::pi) / 180.0;
    parameters[2] = 3.0 * (itk::Math::pi) / 180.0;
    parameters[3] = -40;
    parameters[4] = -15.0;
    parameters[5] = 35.0;
    eulerTransform->SetParameters(parameters);
  }

  /** Use ResampleImageFilter to get transformed image. */
  using ResampleImageFilter = itk::ResampleImageFilter<ImageType, ImageType>;
  ResampleImageFilter::Pointer resample = ResampleImageFilter::New();
  resample->SetInput(image);
  resample->SetTransform(eulerTransform);
  resample->SetSize(outputRegion.GetSize());
  resample->SetOutputStartIndex(outputRegion.GetIndex());
  resample->SetOutputSpacing(outputSpacing);
  resample->SetOutputOrigin(outputOrigin);
  resample->SetOutputDirection(outputDirection);
  resample->Update();

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer1 = WriterType::New();
  writer1->SetInput(resample->GetOutput());
  writer1->SetFileName(resampledImageFileName);
  try
  {
    writer1->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "Exception detected while writing image " << resampledImageFileName;
    std::cerr << " : " << err << std::endl;
    return EXIT_FAILURE;
  }

  /** Create an setup deformation field generator. */
  DisplacementFieldGeneratorType::Pointer defGenerator = DisplacementFieldGeneratorType::New();
  defGenerator->UseReferenceImageOn();
  defGenerator->SetReferenceImage(resample->GetOutput());
  defGenerator->SetTransform(eulerTransform);
  try
  {
    defGenerator->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "Exception detected while generating deformation field";
    std::cerr << " : " << err << std::endl;
    return EXIT_FAILURE;
  }
  /** Use WarpImageFilter with deformation field. */
  WarpImageType::Pointer warper = WarpImageType::New();
  warper->SetOutputSize(outputRegion.GetSize());
  warper->SetOutputStartIndex(outputRegion.GetIndex());
  warper->SetOutputSpacing(outputSpacing);
  warper->SetOutputOrigin(outputOrigin);
  warper->SetOutputDirection(outputDirection);
  warper->SetDisplacementField(defGenerator->GetOutput());
  warper->SetInput(image);
  try
  {
    warper->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "Exception detected while warping image";
    std::cerr << " : " << err << std::endl;
    return EXIT_FAILURE;
  }
  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput(warper->GetOutput());
  writer2->SetFileName(displacementFieldFileName);
  try
  {
    writer2->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "Exception detected while writing image " << displacementFieldFileName;
    std::cerr << " : " << err << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
