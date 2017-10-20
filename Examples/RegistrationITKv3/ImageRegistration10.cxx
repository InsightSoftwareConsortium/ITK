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

// Software Guide : BeginLatex
//
// This example illustrates the use of the image registration framework in
// Insight to align two label maps. Common structures are assumed to
// use the same label.  The registration metric simply counts the
// number of corresponding pixels that have the same label.
//
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMatchCardinalityImageToImageMetric.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkAmoebaOptimizer.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"
#include "itkFileOutputWindow.h"

//
//  The following piece of code implements an observer
//  that will monitor the evolution of the registration process.
//
#include "itkCommand.h"
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef itk::SmartPointer<Self>   Pointer;
  itkNewMacro( Self );

protected:
  CommandIterationUpdate() {};

public:
  typedef itk::AmoebaOptimizer         OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }
    std::cout << optimizer->GetCachedValue() << "   ";
    std::cout << optimizer->GetCachedCurrentPosition() << std::endl;
    }
};


int main( int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile [differenceImage]" << std::endl;
    std::cerr << " [initialTx] [initialTy]" << std::endl;
    return EXIT_FAILURE;
    }

  itk::FileOutputWindow::Pointer fow = itk::FileOutputWindow::New();
  fow->SetInstance( fow );

  // The types of each one of the components in the registration methods should
  // be instantiated. First, we select the image dimension and the type for
  // representing image pixels.
  //
  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;


  //  The types of the input images are instantiated by the following lines.
  //
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  //  Software Guide : BeginLatex
  //  The transform that will map one image space into the other is defined
  //  below.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  An optimizer is required to explore the parameter space of the transform
  //  in search of optimal values of the metric. The metric selected
  //  does not require analytical derivatives of its cost function.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::AmoebaOptimizer       OptimizerType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  The metric will compare how well the two images match each
  //  other. Metric types are usually parameterized by the image types
  //  as can be seen in the following type declaration. The metric
  //  selected here is suitable for comparing two label maps where the
  //  labels are consistent between the two maps.  This metric
  //  measures the percentage of pixels that exactly match or
  //  mismatch.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::MatchCardinalityImageToImageMetric<
                                    FixedImageType,
                                    MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet

  //  Finally, the type of the interpolator is declared. The
  //  interpolator will evaluate the moving image at non-grid
  //  positions.
  //  Software Guide : BeginLatex
  //  Since we are registering label maps, we use a
  //  NearestNeighborInterpolateImageFunction to ensure subpixel
  //  values are not interpolated (to labels that do not exist).
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk:: NearestNeighborInterpolateImageFunction<
                                    MovingImageType,
                                    double          >    InterpolatorType;
  // Software Guide : EndCodeSnippet

  //  The registration method type is instantiated using the types of the
  //  fixed and moving images. This class is responsible for interconnecting
  //  all the components we have described so far.
  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType >    RegistrationType;

  //  Each one of the registration components is created using its
  //  \code{New()} method and is assigned to its respective
  //  \doxygen{SmartPointer}.
  //
  // Software Guide : BeginCodeSnippet
  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  // We are using a MatchCardinalityImageToImageMetric to compare two
  // label maps.  This metric simple counts the percentage of
  // corresponding pixels that have the same label.  This metric does
  // not provide analytical derivatives, so we will use an
  // AmoebaOptimizer to drive the registration.  The AmoebaOptimizer
  // can only minimize a cost function, so we set the metric to count
  // the percentages of mismatches.
  //  Software Guide : BeginLatex
  // Software Guide : BeginCodeSnippet
  metric->MeasureMatchesOff();
  // Software Guide : EndCodeSnippet


  //  Each component is now connected to the instance of the registration method.
  //  \index{itk::RegistrationMethod!SetMetric()}
  //  \index{itk::RegistrationMethod!SetOptimizer()}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //  \index{itk::RegistrationMethod!SetFixedImage()}
  //  \index{itk::RegistrationMethod!SetMovingImage()}
  //  \index{itk::RegistrationMethod!SetInterpolator()}
  //
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;
  FixedImageReaderType::Pointer
    fixedImageReader = FixedImageReaderType::New();
  MovingImageReaderType::Pointer
    movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );


  //  In this example, the fixed and moving images are read from files. This
  //  requires the \doxygen{ImageRegistrationMethod} to acquire its inputs to
  //  the output of the readers.
  //
  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  //  The registration can be restricted to consider only a particular region
  //  of the fixed image as input to the metric computation. This region is
  //  defined by the \code{SetFixedImageRegion()} method.  You could use this
  //  feature to reduce the computational time of the registration or to avoid
  //  unwanted objects present in the image affecting the registration outcome.
  //  In this example we use the full available content of the image. This
  //  region is identified by the \code{BufferedRegion} of the fixed image.
  //  Note that for this region to be valid the reader must first invoke its
  //  \code{Update()} method.
  //
  //  \index{itk::ImageRegistrationMethod!SetFixedImageRegion()}
  //  \index{itk::Image!GetBufferedRegion()}
  //
  fixedImageReader->Update();
  movingImageReader->Update();

  registration->SetFixedImageRegion(
     fixedImageReader->GetOutput()->GetBufferedRegion() );


  //  The parameters of the transform are initialized by passing them in an
  //  array. This can be used to setup an initial known correction of the
  //  misalignment. In this particular case, a translation transform is
  //  being used for the registration. The array of parameters for this
  //  transform is simply composed of the translation values along each
  //  dimension. Setting the values of the parameters to zero
  //  initializes the transform as an \emph{identity} transform. Note that the
  //  array constructor requires the number of elements as an argument.
  //
  //  \index{itk::TranslationTransform!GetNumberOfParameters()}
  //  \index{itk::RegistrationMethod!SetInitialTransformParameters()}
  //
  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  double tx = 0.0;
  double ty = 0.0;

  if( argc > 6 )
    {
    tx = atof( argv[5] );
    ty = atof( argv[6] );
    }

  initialParameters[0] = tx;  // Initial offset in mm along X
  initialParameters[1] = ty;  // Initial offset in mm along Y

  registration->SetInitialTransformParameters( initialParameters );

  //  At this point the registration method is ready for execution. The
  //  optimizer is the component that drives the execution of the
  //  registration.  However, the ImageRegistrationMethod class
  //  orchestrates the ensemble to make sure that everything is in place
  //  before control is passed to the optimizer.
  //

  //  Software Guide : BeginLatex
  //  It is usually desirable to fine tune the parameters of the optimizer.
  //  Each optimizer has particular parameters that must be interpreted in the
  //  context of the optimization strategy it implements.
  //
  //  The AmoebaOptimizer moves a simplex around the cost surface.
  //  Here we set the initial size of the simplex (5 units in each of
  //  the parameters)
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  OptimizerType::ParametersType
    simplexDelta( transform->GetNumberOfParameters() );
  simplexDelta.Fill( 5.0 );

  optimizer->AutomaticInitialSimplexOff();
  optimizer->SetInitialSimplexDelta( simplexDelta );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  We also adjust the tolerances on the optimizer to define
  //  convergence.  Here, we used a tolerance on the parameters of
  //  0.25 (which will be a quarter of image unit, in this case
  //  pixels). We also set the tolerance on the cost function value to
  //  define convergence.  The metric we are using returns the
  //  percentage of pixels that mismatch.  So we set the function
  //  convergence to be 0.1%
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  optimizer->SetParametersConvergenceTolerance( 0.25 ); // quarter pixel
  optimizer->SetFunctionConvergenceTolerance(0.001); // 0.1%
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  In the case where the optimizer never succeeds in reaching the desired
  //  precision tolerance, it is prudent to establish a limit on the number of
  //  iterations to be performed. This maximum number is defined with the
  //  method \code{SetMaximumNumberOfIterations()}.
  //
  //  \index{itk::Amoeba\-Optimizer!SetMaximumNumberOfIterations()}
  //
  //  Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet

  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  //  The registration process is triggered by an invocation of the
  //  \code{Update()} method. If something goes wrong during the
  //  initialization or execution of the registration an exception will be
  //  thrown. We should therefore place the \code{Update()} method
  //  in a \code{try/catch} block as illustrated in the following lines.
  //
  try
    {
    // print out the initial metric value.  need to initialize the
    // registration method to force all the connections to be established.
    registration->Initialize();
    std::cout << "Initial Metric value  = "
              << metric->GetValue( initialParameters )
              << std::endl;

    // run the registration
    registration->Update();
    std::cout << "Optimizer stop condition = "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  // In a real application, you may attempt to recover from the error in the
  // catch block. Here we are simply printing out a message and then
  // terminating the execution of the program.
  //

  //
  //  The result of the registration process is an array of parameters that
  //  defines the spatial transformation in an unique way. This final result is
  //  obtained using the \code{GetLastTransformParameters()} method.
  //
  //  \index{itk::RegistrationMethod!GetLastTransformParameters()}
  //
  ParametersType finalParameters = registration->GetLastTransformParameters();

  //  In the case of the \doxygen{TranslationTransform}, there is a
  //  straightforward interpretation of the parameters.  Each element of the
  //  array corresponds to a translation along one spatial dimension.
  //
  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];

  //  The optimizer can be queried for the actual number of iterations
  //  performed to reach convergence.
  //
  const unsigned int numberOfIterations
    = optimizer->GetOptimizer()->get_num_evaluations();

  //  The value of the image metric corresponding to the last set of parameters
  //  can be obtained with the \code{GetValue()} method of the
  //  optimizer. Since the AmoebaOptimizer does not yet support a call
  //  to GetValue(), we will simply re-evaluate the metric at the
  //  final parameters.
  //
  const double bestValue = metric->GetValue(finalParameters);

  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;

  //  It is common, as the last step of a registration task, to use the
  //  resulting transform to map the moving image into the fixed image space.
  //  This is easily done with the \doxygen{ResampleImageFilter}. Please
  //  refer to Section~\ref{sec:ResampleImageFilter} for details on the use
  //  of this filter.  First, a ResampleImageFilter type is instantiated
  //  using the image types. It is convenient to use the fixed image type as
  //  the output type since it is likely that the transformed moving image
  //  will be compared with the fixed image.
  //
  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  //  A transform of the same type used in the registration process should be
  //  created and initialized with the parameters resulting from the
  //  registration process.
  //
  //  \index{itk::ImageRegistrationMethod!Resampling image}
  //
  TransformType::Pointer finalTransform = TransformType::New();
  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( transform->GetFixedParameters() );

  //  Then a resampling filter is created and the corresponding transform and
  //  moving image connected as inputs.
  //
  ResampleFilterType::Pointer resample = ResampleFilterType::New();
  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  //  As described in Section \ref{sec:ResampleImageFilter}, the
  //  ResampleImageFilter requires additional parameters to be
  //  specified, in particular, the spacing, origin and size of the output
  //  image. The default pixel value is also set to the standard label
  //  for "unknown" or background. Finally, we need to set the
  //  interpolator to be the same type of interpolator as the
  //  registration method used (nearest neighbor).
  //
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();
  resample->SetSize( fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->SetInterpolator( interpolator );

  //  The output of the filter is passed to a writer that will store the
  //  image in a file. An \doxygen{CastImageFilter} is used to convert the
  //  pixel type of the resampled image to the final type used by the
  //  writer. The cast and writer filters are instantiated below.
  //
  typedef unsigned short                           OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType >          CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  //  The filters are created by invoking their \code{New()}
  //  method.
  //
  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  //  The \code{Update()} method of the writer is invoked in order to trigger
  //  the execution of the pipeline.
  //
  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();


  //
  //  The fixed image and the transformed moving image can easily be compared
  //  using the \code{SquaredDifferenceImageFilter}. This pixel-wise
  //  filter computes the squared value of the difference between homologous
  //  pixels of its input images.
  //
  typedef itk::SquaredDifferenceImageFilter<
                                  FixedImageType,
                                  FixedImageType,
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();
  difference->SetInput1( fixedImageReader->GetOutput() );
  difference->SetInput2( resample->GetOutput() );

  //  Its output can be passed to another writer.
  //
  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );

  if( argc > 4 )
    {
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }


  return EXIT_SUCCESS;
}

// SoftwareGuide : BeginLatex
// The example was run on two binary images. The first binary image was generated by running the
// confidence connected image filter (section \ref{sec:ConfidenceConnected}) on
// the MRI slice of the brain. The second was generated similarly after
// shifting the slice by 13 pixels horizontally and 17 pixels
// vertically. The Amoeba optimizer converged after 34 iterations
// and produced the following results:
//
// \begin{verbatim}
// Translation X = 12.5
// Translation Y = 16.77
// \end{verbatim}
// These results are a close match to the true misalignment.
// SoftwareGuide : EndLatex
