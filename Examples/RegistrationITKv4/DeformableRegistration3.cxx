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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Software Guide : BeginLatex
//
// This example demonstrates how to use a variant of the ``demons'' algorithm
// to deformably register two images. This variant uses a different
// formulation for computing the forces to be applied to the image in order to
// compute the deformation fields. The variant uses both the gradient of the
// fixed image and the gradient of the deformed moving image in order to
// compute the forces. This mechanism for computing the forces introduces a
// symmetry with respect to the choice of the fixed and moving images. This
// symmetry only holds during the computation of one iteration of the PDE
// updates. It is unlikely that total symmetry may be achieved by this
// mechanism for the entire registration process.
//
// The first step for using this filter is to include the following header
// files.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSymmetricForcesDemonsRegistrationFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkDisplacementFieldTransform.h"
#include "itkResampleImageFilter.h"
// Software Guide : EndCodeSnippet

//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<CommandIterationUpdate>;
  itkNewMacro(CommandIterationUpdate);

protected:
  CommandIterationUpdate() = default;

  using InternalImageType = itk::Image<float, 2>;
  using VectorPixelType = itk::Vector<float, 2>;
  using DisplacementFieldType = itk::Image<VectorPixelType, 2>;

  using RegistrationFilterType =
    itk::SymmetricForcesDemonsRegistrationFilter<InternalImageType,
                                                 InternalImageType,
                                                 DisplacementFieldType>;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * filter = static_cast<const RegistrationFilterType *>(object);
    if (!(itk::IterationEvent().CheckEvent(&event)))
    {
      return;
    }
    std::cout << filter->GetMetric() << std::endl;
  }
};

int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    return EXIT_FAILURE;
  }

  // Software Guide : BeginLatex
  //
  // Second, we declare the types of the images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned short;

  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;
  // Software Guide : EndCodeSnippet

  // Set up the file readers
  using FixedImageReaderType = itk::ImageFileReader<FixedImageType>;
  using MovingImageReaderType = itk::ImageFileReader<MovingImageType>;

  auto fixedImageReader = FixedImageReaderType::New();
  auto movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(argv[1]);
  movingImageReader->SetFileName(argv[2]);


  // Software Guide : BeginLatex
  //
  // Image file readers are set up in a similar fashion to previous examples.
  // To support the re-mapping of the moving image intensity, we declare an
  // internal image type with a floating point pixel type and cast the input
  // images to the internal image type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using InternalPixelType = float;
  using InternalImageType = itk::Image<InternalPixelType, Dimension>;
  using FixedImageCasterType =
    itk::CastImageFilter<FixedImageType, InternalImageType>;
  using MovingImageCasterType =
    itk::CastImageFilter<MovingImageType, InternalImageType>;

  auto fixedImageCaster = FixedImageCasterType::New();
  auto movingImageCaster = MovingImageCasterType::New();

  fixedImageCaster->SetInput(fixedImageReader->GetOutput());
  movingImageCaster->SetInput(movingImageReader->GetOutput());
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The demons algorithm relies on the assumption that pixels representing
  // the same homologous point on an object have the same intensity on both
  // the fixed and moving images to be registered. In this example, we will
  // preprocess the moving image to match the intensity between the images
  // using the \doxygen{HistogramMatchingImageFilter}.
  //
  // \index{itk::Histogram\-Matching\-Image\-Filter}
  //
  // The basic idea is to match the histograms of the two images at a
  // user-specified number of quantile values. For robustness, the histograms
  // are matched so that the background pixels are excluded from both
  // histograms. For MR images, a simple procedure is to exclude all gray
  // values that are smaller than the mean gray value of the image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using MatchingFilterType =
    itk::HistogramMatchingImageFilter<InternalImageType, InternalImageType>;
  auto matcher = MatchingFilterType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // For this example, we set the moving image as the source or input image
  // and the fixed image as the reference image.
  //
  // \index{itk::Histogram\-Matching\-Image\-Filter!SetInput()}
  // \index{itk::Histogram\-Matching\-Image\-Filter!SetSourceImage()}
  // \index{itk::Histogram\-Matching\-Image\-Filter!SetReferenceImage()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->SetInput(movingImageCaster->GetOutput());
  matcher->SetReferenceImage(fixedImageCaster->GetOutput());
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We then select the number of bins to represent the histograms and the
  // number of points or quantile values where the histogram is to be
  // matched.
  //
  // \index{itk::Histogram\-Matching\-Image\-Filter!Set\-Number\-Of\-Histogram\-Levels()}
  // \index{itk::Histogram\-Matching\-Image\-Filter!Set\-Number\-Of\-Match\-Points()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->SetNumberOfHistogramLevels(1024);
  matcher->SetNumberOfMatchPoints(7);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Simple background extraction is done by thresholding at the mean
  // intensity.
  //
  // \index{itk::Histogram\-Matching\-Image\-Filter!Threshold\-At\-Mean\-Intensity\-On()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->ThresholdAtMeanIntensityOn();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // In the \doxygen{SymmetricForcesDemonsRegistrationFilter}, the deformation
  // field is represented as an image whose pixels are floating point vectors.
  //
  // \index{itk::Symmetric\-Forces\-Demons\-Registration\-Filter}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using VectorPixelType = itk::Vector<float, Dimension>;
  using DisplacementFieldType = itk::Image<VectorPixelType, Dimension>;
  using RegistrationFilterType =
    itk::SymmetricForcesDemonsRegistrationFilter<InternalImageType,
                                                 InternalImageType,
                                                 DisplacementFieldType>;
  auto filter = RegistrationFilterType::New();
  // Software Guide : EndCodeSnippet

  // Create the Command observer and register it with the registration filter.
  //
  auto observer = CommandIterationUpdate::New();
  filter->AddObserver(itk::IterationEvent(), observer);

  // Software Guide : BeginLatex
  //
  // The input fixed image is simply the output of the fixed image casting
  // filter.  The input moving image is the output of the histogram matching
  // filter.
  //
  // \index{itk::Symmetric\-Forces\-Demons\-Registration\-Filter!SetFixedImage()}
  // \index{itk::Symmetric\-Forces\-Demons\-Registration\-Filter!SetMovingImage()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetFixedImage(fixedImageCaster->GetOutput());
  filter->SetMovingImage(matcher->GetOutput());
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The demons registration filter has two parameters: the number of
  // iterations to be performed and the standard deviation of the Gaussian
  // smoothing kernel to be applied to the deformation field after each
  // iteration.
  // \index{itk::Symmetric\-Forces\-Demons\-Registration\-Filter!SetNumberOfIterations()}
  // \index{itk::Symmetric\-Forces\-Demons\-Registration\-Filter!SetStandardDeviations()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations(50);
  filter->SetStandardDeviations(1.0);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The registration algorithm is triggered by updating the filter. The
  // filter output is the computed deformation field.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The \doxygen{ResampleImageFilter} can be used to warp the moving image
  // with the output deformation field. The \doxygen{ResampleImageFilter}
  // requires specifications for the input image to be resampled: an
  // input image interpolator, a transform and the output image's
  // meta-data can be set from a reference image.
  //
  // \index{itk::ResampleImageFilter}
  // \index{itk::ResampleImageFilter!SetInput()}
  // \index{itk::ResampleImageFilter!SetReferenceImage()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using InterpolatorPrecisionType = double;
  using TransformPrecisionType = float;
  using WarperType = itk::ResampleImageFilter<MovingImageType,
                                              MovingImageType,
                                              InterpolatorPrecisionType,
                                              TransformPrecisionType>;
  using InterpolatorType =
    itk::LinearInterpolateImageFunction<MovingImageType,
                                        InterpolatorPrecisionType>;
  auto                    warper = WarperType::New();
  auto                    interpolator = InterpolatorType::New();
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput(movingImageReader->GetOutput());
  warper->SetInterpolator(interpolator);
  warper->UseReferenceImageOn();
  warper->SetReferenceImage(fixedImage);
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The displacement field is not an itk::Transform type. The
  // ResampleImageFilter requires an itk::Transform as input, so a
  // DisplacementFieldTransform needs to be constructed. The
  // resulting warped or resampled  image is written to file as per
  // previous examples.
  //
  // \index{itk::ResampleImageFilter!SetTransform()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  using DisplacementFieldTransformType =
    itk::DisplacementFieldTransform<TransformPrecisionType, Dimension>;
  auto displacementTransform = DisplacementFieldTransformType::New();
  displacementTransform->SetDisplacementField(filter->GetOutput());
  warper->SetTransform(displacementTransform);
  // Software Guide : EndCodeSnippet


  // Write warped image out to file
  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using CastFilterType =
    itk::CastImageFilter<MovingImageType, OutputImageType>;
  using WriterType = itk::ImageFileWriter<OutputImageType>;

  auto writer = WriterType::New();
  auto caster = CastFilterType::New();

  writer->SetFileName(argv[3]);

  caster->SetInput(warper->GetOutput());
  writer->SetInput(caster->GetOutput());
  writer->Update();


  // Software Guide : BeginLatex
  //
  // Let's execute this example using the rat lung data from the previous
  // example. The associated data files can be found in \code{Examples/Data}:
  //
  // \begin{itemize}
  // \item \code{RatLungSlice1.mha}
  // \item \code{RatLungSlice2.mha}
  // \end{itemize}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{DeformableRegistration2CheckerboardBefore}
  // \includegraphics[width=0.44\textwidth]{DeformableRegistration2CheckerboardAfter}
  // \itkcaption[Demon's deformable registration output]{Checkerboard
  // comparisons before and after demons-based deformable registration.}
  // \label{fig:DeformableRegistration3Output}
  // \end{figure}
  //
  // The result of the demons-based deformable registration is presented in
  // Figure \ref{fig:DeformableRegistration3Output}. The checkerboard
  // comparison shows that the algorithm was able to recover the misalignment
  // due to expiration.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginLatex
  //
  // It may be also desirable to write the deformation field as an image of
  // vectors.  This can be done with the following code.
  //
  // Software Guide : EndLatex

  if (argc > 4) // if a fourth line argument has been provided...
  {

    // Software Guide : BeginCodeSnippet
    using FieldWriterType = itk::ImageFileWriter<DisplacementFieldType>;

    auto fieldWriter = FieldWriterType::New();
    fieldWriter->SetFileName(argv[4]);
    fieldWriter->SetInput(filter->GetOutput());

    fieldWriter->Update();
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // Note that the file format used for writing the deformation field must
    // be capable of representing multiple components per pixel. This is the
    // case for the MetaImage and VTK file formats for example.
    //
    // Software Guide : EndLatex
  }

  return EXIT_SUCCESS;
}
