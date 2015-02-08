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

//  Software Guide : Begin TODO HACK FIXME CommandLineArgs
//  INPUTS:  {BrainT1SliceBorder20.png}
//  INPUTS:  {BrainProtonDensitySliceShifted13x17y.png}
//  ARGUMENTS:    RegisteredImage.png 0
//  OUTPUTS: {JointEntropyHistogramPriorToRegistration.png}
//  OUTPUTS: {JointEntropyHistogramAfterRegistration.png}
//  ARGUMENTS:    128
//  Software Guide : End TODO HACK FIXME CommandLineArgs

//  Software Guide : BeginLatex
//
//  When fine tuning the parameters of an image registration process it is not
//  always clear what factor are having a larger impact on the behavior of the
//  registration. Even plotting the values of the metric and the transform
//  parameters may not provide a clear indication on the best way to modify the
//  optimizer and metric parameters in order to improve the convergence rate
//  and stability. In such circumstances it is useful to take a closer look at
//  the internals of the components involved in computing the registration. One
//  of the critical components is, of course, the image metric. This section
//  illustrates a mechanism that can be used for monitoring the behavior of the
//  Mutual Information metric by continuously looking at the joint histogram at
//  regular intervals during the iterations of the optimizer.
//
//  This particular example shows how to use the
//  \doxygen{HistogramToEntropyImageFilter} class in order to get access to the
//  joint histogram that is internally computed by the metric. This class
//  represents the joint histogram as a $2D$ image and therefore can take
//  advantage of the IO functionalities described in chapter~\ref{sec:IO}.  The
//  example registers two images using the gradient descent optimizer.  The
//  transform used here is a simple translation transform. The metric is a
//  \doxygen{MutualInformationHistogramImageToImageMetric}.
//
//  In the code below we create a helper class called the
//  \code{HistogramWriter}.  Its purpose is to save the joint histogram into a
//  file using any of the file formats supported by ITK. This object is invoked
//  after every iteration of the optimizer.  The writer here saves the joint
//  histogram into files with names: \code{JointHistogramXXX.mhd} where
//  \code{XXX} is replaced with the iteration number. The output image contains
//  the joint entropy histogram given by
//  \begin{equation}
//  f_{ij} = -p_{ij} \log_2 ( p_{ij} )
//  \end{equation}
//
//  where the indices $i$ and $j$ identify the location of a bin in the Joint
//  Histogram of the two images and are in the ranges $i \in [0:N-1]$ and  $j
//  \in [0:M-1]$. The image $f$ representing the joint histogram has $N x M$
//  pixels because the intensities of the Fixed image are quantized into $N$
//  histogram bins and the intensities of the Moving image are quantized into
//  $M$ histogram bins. The probability value $p_{ij}$ is computed from the
//  frequency count of the histogram bins.
//  \begin{equation}
//  p_{ij} = \frac{q_{ij}}{\sum_{i=0}^{N-1} \sum_{j=0}^{M-1} q_{ij}}
//  \end{equation}
//  The value $q_{ij}$ is the frequency of a bin in the histogram and it is
//  computed as the number of pixels where the Fixed image has intensities in
//  the range of bin $i$ and the Moving image has intensities on the range of
//  bin $j$.  The value $p_{ij}$ is therefore the probability of the occurrence
//  of the measurement vector centered in the bin ${ij}$.  The filter produces
//  an output image of pixel type \code{double}. For details on the use of
//  Histograms in ITK please refer to section~\ref{sec:Histogram}.
//
//  Depending on whether you want to see the joint histogram frequencies
//  directly, or the joint probabilities, or log of joint probabilities, you
//  may want to instantiate respectively any of the following classes
//
//  \begin{itemize}
//  \item \doxygen{HistogramToIntensityImageFilter}
//  \item \doxygen{HistogramToProbabilityImageFilter}
//  \item \doxygen{HistogramToLogProbabilityImageFilter}
//  \end{itemize}
//
//  \index{Histogram\-To\-Log\-Probability\-ImageFilter}
//  \index{Histogram\-To\-Intensity\-Image\-Filter}
//  \index{Histogram\-To\-Probability\-Image\-Filter}
//
//  The use of all of these classes is very similar. Note that the log of the
//  probability is equivalent to units of information, also known as
//  \textbf{bits}, more details on this concept can be found in
//  section~\ref{sec:ComputingImageEntropy}
//
//   Software Guide : EndLatex


#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkNormalizeImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"

#include <iomanip>

// Software Guide : BeginLatex
//
// The header files of the classes featured in this example are included as a
// first step.
//
// \index{Histogram\-To\-Probability\-Image\-Filter!Header}
// \index{Mutual\-Information\-Histogram\-Image\-To\-Image\-Metric!Header}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkHistogramToEntropyImageFilter.h"
#include "itkMutualInformationHistogramImageToImageMetric.h"
// Software Guide : EndCodeSnippet

#include "itkCommand.h"

#include <stdio.h>

// Functor to rescale plot the histogram on a log scale and invert it.
template< class TInput >
class RescaleDynamicRangeFunctor
{
public:
  typedef unsigned char OutputPixelType;
  RescaleDynamicRangeFunctor() {};
  ~RescaleDynamicRangeFunctor() {};
  inline OutputPixelType operator()( const TInput &A )
    {
    if( (A > 0.0) )
      {
      if( -(30.0 * std::log(A)) > 255 )
        {
        return static_cast<OutputPixelType>( 255 );
        }
      else
        {
        return itk::Math::Round<OutputPixelType>( -(30.0 * std::log(A)) );
        }
      }
    else
      {
      return static_cast<OutputPixelType>(255);
      }
    }
};

// Class to write the joint histograms.
// Software : BeginLatex
//
// Here we will create a simple class to write the joint histograms. This
// class, that we arbitrarily name as \code{HistogramWriter}, uses internally
// the \doxygen{HistogramToEntropyImageFilter} class among others.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
namespace
{
class HistogramWriter
{
public:
  typedef float InternalPixelType;
  itkStaticConstMacro( Dimension, unsigned int, 2);

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;

  typedef itk::MutualInformationHistogramImageToImageMetric<
                                        InternalImageType,
                                        InternalImageType >    MetricType;
  // Software Guide : EndCodeSnippet

  typedef MetricType::Pointer   MetricPointer;

  // Software Guide : BeginCodeSnippet
  typedef MetricType::HistogramType   HistogramType;

  typedef itk::HistogramToEntropyImageFilter< HistogramType, InternalImageType>
                                HistogramToEntropyImageFilterType;

  typedef HistogramToEntropyImageFilterType::Pointer
                                HistogramToImageFilterPointer;

  typedef HistogramToEntropyImageFilterType::OutputImageType OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > HistogramFileWriterType;
  typedef HistogramFileWriterType::Pointer        HistogramFileWriterPointer;
  // Software Guide : EndCodeSnippet

  typedef HistogramToEntropyImageFilterType::OutputPixelType OutputPixelType;

  HistogramWriter():
    m_Metric(0)
    {

    // Software Guide : BeginLatex
    //
    // The \code{HistogramWriter} has a member variable \code{m\_Filter} of type
    // HistogramToEntropyImageFilter.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    this->m_Filter = HistogramToEntropyImageFilterType::New();
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // It also has an ImageFileWriter that has been instantiated using the image
    // type that is produced as output from the histogram to image filter. We
    // connect the output of the filter as input to the writer.
    //
    // Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    this->m_HistogramFileWriter = HistogramFileWriterType::New();
    this->m_HistogramFileWriter->SetInput( this->m_Filter->GetOutput() );
    // Software Guide : EndCodeSnippet

    }

  ~HistogramWriter() { };

  void SetMetric( MetricPointer metric )
    {
    this->m_Metric = metric;
    }

  MetricPointer GetMetric() const
    {
    return this->m_Metric;
    }

  void WriteHistogramFile( unsigned int iterationNumber )
  {
    std::string outputFileBase = "JointHistogram";
    std::ostringstream outputFilename;
    outputFilename << outputFileBase
                   << "."
                   << std::setfill('0') << std::setw(3) << iterationNumber
                   << "."
                   << "mhd";
    m_HistogramFileWriter->SetFileName( outputFilename.str() );
    this->m_Filter->SetInput( this->GetMetric()->GetHistogram() );
    this->m_Filter->Modified();

    try
      {
      m_Filter->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      }

    try
      {
      m_HistogramFileWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << excp << std::endl;
      }

    std::cout << "Joint Histogram file: ";
    std::cout << outputFilename.str() << " written" << std::endl;

    }

  // Software Guide : BeginLatex
  //
  // The method of this class that is most relevant to our discussion is the
  // one that writes the image into a file. In this method we assign the output
  // histogram of the metric to the input of the histogram to image filter. In
  // this way we construct an ITK $2D$ image where every pixel corresponds to
  // one of the Bins of the joint histogram computed by the Metric.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  void WriteHistogramFile( std::string &outputFilename  )
    {
    // Software Guide : EndCodeSnippet


    // Software Guide : BeginCodeSnippet
    this->m_Filter->SetInput( this->GetMetric()->GetHistogram() );
    this->m_Filter->Modified();

    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    //
    // The output of the filter is connected to a filter that will rescale the
    // intensities in order to improve the visualization of the values. This is
    // done because it is common to find histograms of medical images that have
    // a minority of bins that are largely dominant. Visualizing such histogram
    // in direct values is challenging because only the dominant bins tend to
    // become visible.
    //
    // Software Guide : EndLatex


    //Write the joint histogram as outputFilename. Also intensity window
    //the image by lower and upper thresholds and rescale the image to
    //8 bits.
    typedef itk::Image< unsigned char, Dimension > RescaledOutputImageType;

    typedef RescaleDynamicRangeFunctor<
                              OutputPixelType
                                      > RescaleDynamicRangeFunctorType;

    typedef itk::UnaryFunctorImageFilter<
                                OutputImageType,
                                RescaledOutputImageType,
                                RescaleDynamicRangeFunctorType
                                      > RescaleDynamicRangeFilterType;

    RescaleDynamicRangeFilterType::Pointer rescaler =
                                RescaleDynamicRangeFilterType::New();

    rescaler->SetInput( m_Filter->GetOutput() );

    typedef itk::ImageFileWriter< RescaledOutputImageType > RescaledWriterType;

    RescaledWriterType::Pointer rescaledWriter =
                                RescaledWriterType::New();

    rescaledWriter->SetInput( rescaler->GetOutput() );

    rescaledWriter->SetFileName( outputFilename );

    try
      {
      m_Filter->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      }

    try
      {
      rescaledWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << excp << std::endl;
      }

    std::cout << "Joint Histogram file: " << outputFilename <<
        " written" << std::endl;
    }

// Software Guide : BeginLatex
//
// The following are the member variables of our \code{HistogramWriter} class.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet

private:
  MetricPointer                   m_Metric;
  HistogramToImageFilterPointer   m_Filter;
  HistogramFileWriterPointer      m_HistogramFileWriter;
  // Software Guide : EndCodeSnippet
  std::string   m_OutputFile;
};

// Command - observer invoked after every iteration of the optimizer
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef  itk::SmartPointer<Self>  Pointer;
  itkSimpleNewMacro( Self );

protected:
  CommandIterationUpdate()
    {
    m_WriteHistogramsAfterEveryIteration = false;
    }

public:

  typedef   itk::RegularStepGradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType *                        OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
    if( ! itk::IterationEvent().CheckEvent( &event ) || optimizer == ITK_NULLPTR )
      {
      return;
      }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;

    // Write the joint histogram as a file JointHistogramXXX.mhd
    // where \code{XXX} is the iteration number


    //Write Joint Entropy Histogram prior to registration.
    if( optimizer->GetCurrentIteration() == 0 )
      {
      // Software Guide : BeginLatex
      //
      // We invoke the histogram writer within the Command/Observer of the
      // optimizer to write joint histograms after every iteration.
      //
      // Software Guide : EndLatex

      // Software Guide : BeginCodeSnippet
      m_JointHistogramWriter.WriteHistogramFile( m_InitialHistogramFile );
      // Software Guide : EndCodeSnippet
      }
    if( m_WriteHistogramsAfterEveryIteration )
      {
      m_JointHistogramWriter.WriteHistogramFile(
                                           optimizer->GetCurrentIteration() );
      }
    }

  void SetWriteHistogramsAfterEveryIteration( bool value )
    {
    m_WriteHistogramsAfterEveryIteration = value;
    }

  void SetInitialHistogramFile( const char * filename )
    {
    m_InitialHistogramFile = filename;
    }

  HistogramWriter m_JointHistogramWriter;

private:
  bool              m_WriteHistogramsAfterEveryIteration;
  std::string       m_InitialHistogramFile;

};

} // end anonymous namespace


int main( int argc, char *argv[] )
{
  if( argc < 8 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile WriteJointHistogramsAfterEveryIteration ";
    std::cerr << "JointHistogramPriorToRegistrationFile ";
    std::cerr << "JointHistogramAfterRegistrationFile ";
    std::cerr << "NumberOfHistogramBinsForWritingTheMutualInformationHistogramMetric";
    std::cerr <<  std::endl;
    return EXIT_FAILURE;
    }

  typedef  unsigned char  PixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< PixelType, Dimension >         FixedImageType;
  typedef itk::Image< PixelType, Dimension >         MovingImageType;
  typedef   float                                    InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;

  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction<
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::ImageRegistrationMethod<
                                    InternalImageType,
                                    InternalImageType >  RegistrationType;

  typedef itk::MutualInformationHistogramImageToImageMetric<
                                          InternalImageType,
                                          InternalImageType >    MetricType;

  // Software Guide : BeginLatex
  //
  // We instantiate an optimizer, interpolator and the registration method as
  // shown in previous examples.
  //
  // Software Guide : EndLatex

  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  MetricType::Pointer         metric        = MetricType::New();


  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );

  // Software Guide : BeginLatex
  //
  // The number of bins in the metric is set with the \code{SetHistogramSize()}
  // method. This will determine the number of pixels along each dimension of
  // the joint histogram. Note that in this case we arbitrarily decided to use
  // the same number of bins for the intensities of the Fixed image and those
  // of the Moving image. However, this does not have to be the case, we could
  // have selected different numbers of bins for each image.
  //
  // \index{Mutual\-Information\-Histogram\-Image\-To\-Image\-Metric!SetHistogramSize()}
  // \index{SetHistogramSize(),Mutual\-Information\-Histogram\-Image\-To\-Image\-Metric}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  unsigned int numberOfHistogramBins = atoi( argv[7] );
  MetricType::HistogramType::SizeType histogramSize;
  histogramSize.SetSize(2);
  histogramSize[0] = numberOfHistogramBins;
  histogramSize[1] = numberOfHistogramBins;
  metric->SetHistogramSize( histogramSize );
  // Software Guide : EndCodeSnippet

  const unsigned int numberOfParameters = transform->GetNumberOfParameters();
  typedef MetricType::ScalesType ScalesType;
  ScalesType scales( numberOfParameters );
  scales.Fill( 1.0 );
  metric->SetDerivativeStepLengthScales(scales);

  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();

  // Set the metric for the joint histogram writer
  observer->m_JointHistogramWriter.SetMetric( metric );

  registration->SetMetric( metric  );

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );


  typedef itk::NormalizeImageFilter<
                                FixedImageType,
                                InternalImageType
                                        > FixedNormalizeFilterType;

  typedef itk::NormalizeImageFilter<
                                MovingImageType,
                                InternalImageType
                                              > MovingNormalizeFilterType;

  FixedNormalizeFilterType::Pointer fixedNormalizer =
                                            FixedNormalizeFilterType::New();

  MovingNormalizeFilterType::Pointer movingNormalizer =
                                            MovingNormalizeFilterType::New();
  typedef itk::DiscreteGaussianImageFilter<
                                      InternalImageType,
                                      InternalImageType
                                                    > GaussianFilterType;

  GaussianFilterType::Pointer fixedSmoother  = GaussianFilterType::New();
  GaussianFilterType::Pointer movingSmoother = GaussianFilterType::New();

  fixedSmoother->SetVariance( 2.0 );
  movingSmoother->SetVariance( 2.0 );
  fixedNormalizer->SetInput(  fixedImageReader->GetOutput() );
  movingNormalizer->SetInput( movingImageReader->GetOutput() );

  fixedSmoother->SetInput( fixedNormalizer->GetOutput() );
  movingSmoother->SetInput( movingNormalizer->GetOutput() );

  registration->SetFixedImage(    fixedSmoother->GetOutput()    );
  registration->SetMovingImage(   movingSmoother->GetOutput()   );


  try
    {
    fixedNormalizer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  registration->SetFixedImageRegion(
       fixedNormalizer->GetOutput()->GetBufferedRegion() );

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  registration->SetInitialTransformParameters( initialParameters );


  optimizer->SetMaximumStepLength( 4.00 );
  optimizer->SetMinimumStepLength( 0.01 );
  optimizer->SetRelaxationFactor(  0.90 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->MaximizeOn();

  optimizer->AddObserver( itk::IterationEvent(), observer );


  observer->SetInitialHistogramFile( argv[5] );

  if( atoi(argv[4]) )
    {
    observer->SetWriteHistogramsAfterEveryIteration( true );
    }


  try
    {
    registration->Update();
    std::cout << "Optimizer stop condition: "
              << registration->GetOptimizer()->GetStopConditionDescription()
              << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  ParametersType finalParameters = registration->GetLastTransformParameters();

  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];

  unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  double bestValue = optimizer->GetValue();


  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;

  //Write Joint Entropy Histogram after registration.
  std::string histogramAfter(argv[6]);
  try
    {
    observer->m_JointHistogramWriter.WriteHistogramFile( histogramAfter );
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );
  finalTransform->SetFixedParameters( transform->GetFixedParameters() );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 100 );


  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;


  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();


  writer->SetFileName( argv[3] );


  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    }

  return EXIT_SUCCESS;

}

// Software Guide : BeginLatex
//
// Mutual information attempts to re-group the joint entropy histograms into a
// more ``meaningful'' formation. An optimizer that minimizes the joint entropy
// seeks a transform that produces a small number of high value bins and a
// large majority of almost zero bins. Multi-modality registration seeks such a
// transform while also attempting to maximize the information contribution by
// the fixed and the moving images in the overall region of the metric.
//
// A T1 MRI (fixed image) and a proton density MRI (moving image) as shown in Figure
// \ref{fig:FixedMovingImageRegistration2}
// are provided as input to this example.
//
// Figure \ref{fig:JointEntropyHistograms} shows the joint histograms before and
// after registration.
// \begin{figure}
// \center
// \includegraphics[width=0.44\textwidth]{JointEntropyHistogramPriorToRegistration}
// \includegraphics[width=0.44\textwidth]{JointEntropyHistogramAfterRegistration}
// \itkcaption[Multi-modality joint histograms]{Joint entropy histograms before and
// after registration. The final transform was within half a pixel of true misalignment.}
// \label{fig:JointEntropyHistograms}
// \end{figure}
//  Software Guide : EndLatex
