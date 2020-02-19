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
#include <iostream>
#include <iomanip>
#include <string>
#include <sstream>

#include <itkImage.h>
#include <itkLinearInterpolateImageFunction.h>
#include <itkNearestNeighborInterpolateImageFunction.h>
#include <itkLabelImageGaussianInterpolateImageFunction.h>
#include <itkBSplineInterpolateImageFunction.h>
#include <itkResampleImageFilter.h>
#include <itkVersorRigid3DTransform.h>
#include <itkImageFileWriter.h>
#include <itkImageFileReader.h>
#include <itkTestingComparisonImageFilter.h>
#include <itkTimeProbe.h>

#include "itkLabelImageGenericInterpolateImageFunction.h"
#include "itkLabelSelectionImageAdaptor.h"
#include "itkLabelSelectionPixelAccessor.h"

/* This demo is a torture test for interpolators: it takes an input label map,
 * rotates it one full turn in ten steps, and writes the result */

template <class ImageType>
void
RotateNTimes(typename ImageType::Pointer                                 input,
             typename itk::InterpolateImageFunction<ImageType, double> * interpolator,
             unsigned int                                                number_of_rotations,
             std::string                                                 filename_prefix,
             std::string                                                 output_directory)
{
  using TransformType = itk::VersorRigid3DTransform<double>;
  using ResampleFilterType = itk::ResampleImageFilter<ImageType, ImageType>;
  TransformType::AxisType axis;
  axis[0] = 0;
  axis[1] = 1;
  axis[2] = 0;
  TransformType::InputPointType center;
  center[0] = 144;
  center[1] = 86;
  center[2] = 101;
  TransformType::Pointer rot = TransformType::New();
  rot->SetCenter(center);
  rot->SetRotation(axis, 2. * itk::Math::pi / number_of_rotations);

  typename ResampleFilterType::Pointer rs = ResampleFilterType::New();
  rs->SetInput(input);
  rs->SetReferenceImage(input);
  rs->SetUseReferenceImage(true);
  rs->SetTransform(rot);
  rs->SetInterpolator(interpolator);
  typename ImageType::Pointer out;
  itk::TimeProbe              timer;
  timer.Start();
  for (unsigned i = 0; i < number_of_rotations; ++i)
  {
    rs->SetReferenceImage(input);
    rs->SetUseReferenceImage(true);
    rs->Update();
    out = rs->GetOutput();
    out->DisconnectPipeline();
    rs->SetInput(out);
    rs->SetTransform(rot);
  }
  timer.Stop();
  using ComparisonFilterType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  typename ComparisonFilterType::Pointer compare = ComparisonFilterType::New();
  compare->SetValidInput(input);
  compare->SetTestInput(out);
  compare->Update();
  std::cout << "Pixels with differences: " << std::setw(8) << compare->GetNumberOfPixelsWithDifferences() << " ( "
            << std::fixed << std::setprecision(2)
            << static_cast<double>(compare->GetNumberOfPixelsWithDifferences()) /
                 input->GetLargestPossibleRegion().GetNumberOfPixels() * 100.
            << "% ) in " << timer.GetTotal() << "s" << std::endl;
  using WriterType = itk::ImageFileWriter<ImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetUseCompression(true);
  writer->SetInput(out);
  std::ostringstream fname_stream;
  fname_stream << output_directory << "/" << filename_prefix << "_" << number_of_rotations << ".mha";
  writer->SetFileName(fname_stream.str());
  writer->Write();
}


// The BSpline interpolator has more arguments than other interpolators, so we set the additional parameter to the
// default value
template <class TImage, typename TCoordRep>
class BSplineInterpolator : public itk::BSplineInterpolateImageFunction<TImage, TCoordRep>
{};

template <class TImage, typename TCoordRep>
class FixedGaussianInterpolator : public itk::GaussianInterpolateImageFunction<TImage, TCoordRep>
{
public:
  using Self = FixedGaussianInterpolator;
  using Superclass = itk::GaussianInterpolateImageFunction<TImage, TCoordRep>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FixedGaussianInterpolator, GaussianInterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  FixedGaussianInterpolator()
  {
    this->SetAlpha(1.0);
    this->SetSigma(0.3);
  }
};

int
RotateLabels(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cout << "Usage: " << argv[0] << " inputImage outputDirectory numberOfRotation" << std::endl;
    return EXIT_FAILURE;
  }
  std::string        inputFileName = argv[1];
  std::string        outputDirectory = argv[2];
  std::istringstream stream(argv[3]);
  unsigned int       numberOfRotations = 0;
  stream >> numberOfRotations;
  if (numberOfRotations < 1)
  {
    std::cout << "numberOfRotation must be strictly positive. Given value: " << numberOfRotations << std::endl;
    return EXIT_FAILURE;
  }
  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 3;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer r = ReaderType::New();
  r->SetFileName(inputFileName);
  r->Update();

  std::cout << "Testing with " << numberOfRotations << " rotations..." << std::endl << std::endl;

  std::cout << "Nearest neighbor interpolator...                        " << std::flush;
  using NNInterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, double>;
  NNInterpolatorType::Pointer nn_interp = NNInterpolatorType::New();
  RotateNTimes<ImageType>(r->GetOutput(), nn_interp, numberOfRotations, "nearest", outputDirectory);

  std::cout << "Linear interpolator...                                  " << std::flush;
  using LInterpolatorType = itk::LinearInterpolateImageFunction<ImageType, double>;
  LInterpolatorType::Pointer l_interp = LInterpolatorType::New();
  RotateNTimes<ImageType>(r->GetOutput(), l_interp, numberOfRotations, "linear", outputDirectory);

  std::cout << "Label Gaussian interpolator type...                     " << std::flush;
  using LGInterpolatorType = itk::LabelImageGaussianInterpolateImageFunction<ImageType, double>;
  LGInterpolatorType::Pointer lg_interp = LGInterpolatorType::New();
  lg_interp->SetSigma(0.3);
  RotateNTimes<ImageType>(r->GetOutput(), lg_interp, numberOfRotations, "label_gaussian", outputDirectory);

  std::cout << "Generic label interpolator with nearest neighbor...     " << std::flush;
  using GNNInterpolatorType =
    itk::LabelImageGenericInterpolateImageFunction<ImageType, itk::NearestNeighborInterpolateImageFunction>;
  GNNInterpolatorType::Pointer gnn_interp = GNNInterpolatorType::New();
  RotateNTimes<ImageType>(r->GetOutput(), gnn_interp, numberOfRotations, "gl_nearest", outputDirectory);

  std::cout << "Generic label interpolator with linear interpolation... " << std::flush;
  using GLInterpolatorType =
    itk::LabelImageGenericInterpolateImageFunction<ImageType, itk::LinearInterpolateImageFunction>;
  GLInterpolatorType::Pointer gl_interp = GLInterpolatorType::New();
  RotateNTimes<ImageType>(r->GetOutput(), gl_interp, numberOfRotations, "gl_linear", outputDirectory);

  std::cout << "Generic label interpolator with B-Spline interpolation...             " << std::flush;
  using GBSInterpolatorType = itk::LabelImageGenericInterpolateImageFunction<ImageType, BSplineInterpolator>;
  GBSInterpolatorType::Pointer gbs_interp = GBSInterpolatorType::New();
  RotateNTimes<ImageType>(r->GetOutput(), gbs_interp, numberOfRotations, "gl_bspline", outputDirectory);

  std::cout << "Generic label interpolator with Gaussian interpolation...             " << std::flush;
  using GGInterpolatorType = itk::LabelImageGenericInterpolateImageFunction<ImageType, FixedGaussianInterpolator>;
  GGInterpolatorType::Pointer gg_interp = GGInterpolatorType::New();
  RotateNTimes<ImageType>(r->GetOutput(), gg_interp, numberOfRotations, "gl_gaussian", outputDirectory);

  std::cout << std::endl;


  return EXIT_SUCCESS;
}
