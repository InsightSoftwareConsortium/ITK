/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistrationHistogramPlotter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

      
//  Software Guide : BeginCommandLineArgs
//  INPUTS: {BrainT1SliceBorder20.png}, {BrainProtonDensitySliceShifted13x17y.png}
//  RegisteredImage.png 0
//  OUTPUTS: {JointEntropyHistogramPriorToRegistration.png}
//  OUTPUTS: {JointEntropyHistogramAfterRegistration.png}
//  128
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
// The example shows how to use the \doxygen{HistogramToEntropyImageFilter} class.
// The example registers two images using the gradient descent optimizer.
// The transform used here is a simple translation transform. The metric
// is a MutualInformationHistogramImageToImageMetric. 
//
// The jointHistogramWriter is invoked after every iteration of the optimizer.
// the writer here writes the joint histogram after every iteration as 
// JointHistogramXXX.mhd. The output image contains the joint entropy histogram
// given by 
// \begin{equation}
// f(I) = -p \log_2 p
// \end{equation}
// 
// where 
// \begin{equation}
// p = \frac{q_I}{\sum_{i \in I} q_I}
// \end{equation}
//  and $q_I$ is the frequency of measurement vector, I.
//  and p is the probability of the occurance of the measurement vector, $q_I$.
// The filter produces an output image is of type double. 
//
// You may similarly instantiate \doxygen{HistogramToLogProbabilityImageFilter} 
// or \doxygen{HistogramToIntensityImageFilter} or 
// \doxygen{HistogramToProbabilityImageFilter}. 
//  Software Guide : EndLatex 


#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMutualInformationHistogramImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkGradientDescentOptimizer.h"
#include "itkImage.h"
#include "itkNormalizeImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
// Software Guide : BeginCodeSnippet
#include "itkHistogramToEntropyImageFilter.h"
// Software Guide : EndCodeSnippet
#include "itkCommand.h"
#include "itkUnaryFunctorImageFilter.h"

#include <stdio.h>


const    unsigned int    Dimension = 2;
bool     writeHistogramsAfterEveryIteration = false;  
  //do not write joint histograms after every iteration.
char     *initialHistogramFile;      


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
      if( -(30.0 * log(A)) > 255 )
        {
        return static_cast<OutputPixelType>( 255 );
        }
      else
        {
        return static_cast<OutputPixelType>( -(30.0 * log(A)) );
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
// Here we will write a simple class to write the joint histograms. The class
// uses the \doxygen{HistogramToEntropyImageFilter}.
// Software Guide : BeginCodeSnippet
class HistogramWriter
{
public:
  typedef float InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  typedef itk::MutualInformationHistogramImageToImageMetric< 
                                          InternalImageType, 
                                          InternalImageType >    MetricType;
  // Software Guide : EndCodeSnippet
  
  typedef MetricType::Pointer   MetricPointer;
  
  // Software Guide : BeginCodeSnippet
  typedef MetricType::HistogramType   HistogramType;
  typedef itk::HistogramToEntropyImageFilter< HistogramType > 
                                HistogramToEntropyImageFilterType;
  typedef HistogramToEntropyImageFilterType::Pointer   HistogramToImageFilterPointer;
  typedef itk::ImageFileWriter< HistogramToEntropyImageFilterType::OutputImageType > 
                                                          HistogramWriterType;
  typedef HistogramWriterType::Pointer   HistogramWriterPointer;
  // Software Guide : EndCodeSnippet
  
  HistogramWriter():
    m_Metric(0)
    {
    this->m_Filter = HistogramToEntropyImageFilterType::New();
    this->histogramWriter = HistogramWriterType::New();
    this->histogramWriter->SetInput( this->m_Filter->GetOutput() );

    std::string outputFileBase = "JointHistogram"; 
            // Base of series filenames ( of the joint histogram )
    this->outputFile = outputFileBase + "%03d.";
    this->outputFile += "mhd";   // histogram filename extension
    }
    
  ~HistogramWriter() { };
  
  void SetMetric( MetricPointer metric )
    {
    this->m_Metric = metric;
    }

  MetricPointer GetMetric(  )
    {
    return this->m_Metric;
    }

  void WriteHistogramFile( unsigned int iterationNumber )
    {
    char outputFilename[1000];
    sprintf (outputFilename, this->outputFile.c_str(), iterationNumber ); 
    
    histogramWriter->SetFileName( outputFilename );
    this->m_Filter->SetInput( m_Metric->GetHistogram() );
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
      histogramWriter->Update(); 
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << excp << std::endl;
      }

    std::cout << "Joint Histogram file: " << outputFilename <<
        " written" << std::endl;
    }
  
  // Software Guide : BeginCodeSnippet
  void WriteHistogramFile( char *outputFilename  )
    {
    // Software Guide : EndCodeSnippet
    //Write the joint histogram as outputFilename. Also intensity window
    //the image by lower and upper thresholds and rescale the image to 
    //8 bits.
    typedef itk::Image< unsigned char > RescaledOutputImageType;
    typedef RescaleDynamicRangeFunctor< HistogramToEntropyImageFilterType::OutputPixelType > RescaleDynamicRangeFunctorType;
    typedef itk::UnaryFunctorImageFilter< HistogramToEntropyImageFilterType::OutputImageType,
        RescaledOutputImageType, RescaleDynamicRangeFunctorType> RescaleDynamicRangeFilterType;
    RescaleDynamicRangeFilterType::Pointer rescaler = RescaleDynamicRangeFilterType::New();
   
    rescaler->SetInput( m_Filter->GetOutput() ); 
    typedef itk::ImageFileWriter< RescaledOutputImageType > RescaledWriterType;
    RescaledWriterType::Pointer rescaledWriter = RescaledWriterType::New();
    rescaledWriter->SetInput( rescaler->GetOutput() );
      
    rescaledWriter->SetFileName( outputFilename );
    
    // Software Guide : BeginCodeSnippet
    this->m_Filter->SetInput( m_Metric->GetHistogram() );
    // Software Guide : EndCodeSnippet
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
  
// Software Guide : BeginCodeSnippet
private:
  MetricPointer m_Metric;
  HistogramToImageFilterPointer m_Filter;
  // Software Guide : EndCodeSnippet
  HistogramWriterPointer histogramWriter;
  std::string   outputFile;
} jointHistogramWriter;


// Command - observer invoked after every iteration of the optimizer    
class CommandIterationUpdate : public itk::Command 
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
  typedef  itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  
  typedef   itk::GradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *           OptimizerPointer;
  
  void Execute(itk::Object *caller, const itk::EventObject & event)
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event)
    {
      OptimizerPointer optimizer = 
        dynamic_cast< OptimizerPointer >( object );
      if( ! itk::IterationEvent().CheckEvent( &event ) )
        {
        return;
        }
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << optimizer->GetValue() << "   ";
      std::cout << optimizer->GetCurrentPosition() << std::endl;
      
      // Write the joint histogram as a file JointHistogramXXX.mhd 
      // where XXX is the iteration number
      

      //Write Joint Entropy Histogram prior to registration.
      if( optimizer->GetCurrentIteration() == 0 ) 
        {
        // Software Guide : BeginLatex
        // We may invoke the histogram writer within the command-observer of 
        // the optimizer to write joint histograms after every iteration.
        // Software Guide : EndLatex
        // Software Guide : BeginCodeSnippet
        jointHistogramWriter.WriteHistogramFile( initialHistogramFile );
        // Software Guide : EndCodeSnippet
        }
      if( writeHistogramsAfterEveryIteration )
        {
        jointHistogramWriter.WriteHistogramFile( optimizer->GetCurrentIteration() );  
        }
    }
};


int main( int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile JointHistogramPriorToRegistrationFile ";
    std::cerr << "JointHistogramAfterRegistrationFile WriteJointHistogramsAfterEveryIteration";
    std::cerr << "Number of histogram bins for writing the MutualInformationHistogramMetric";
    std::cerr <<  std::endl;
    return EXIT_FAILURE;
    }
 
  typedef  unsigned char  PixelType;
  
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  typedef   float     InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;

  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::GradientDescentOptimizer                  OptimizerType;
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
  // We instantiate an optimizer, interpolator and the registration method as 
  // before.
  // Software Guide : EndLatex

  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  MetricType::Pointer         metric        = MetricType::New();
 
  
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );


  if( atoi(argv[4]) ) 
    {
    writeHistogramsAfterEveryIteration = true;
    }
  
  // Software Guide : BeginLatex
  // The number of bins in the metric is set with the \code{SetHistogramSize()}
  // method. This will determine the number of pixels along each dimension of 
  // the joint histogram.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  unsigned int numberOfHistogramBins = atoi( argv[7] );
  MetricType::HistogramType::SizeType histogramSize;
  histogramSize[0] = numberOfHistogramBins;
  histogramSize[1] = numberOfHistogramBins;
  metric->SetHistogramSize( histogramSize );
  // Software Guide : EndCodeSnippet
  
  const unsigned int numberOfParameters = transform->GetNumberOfParameters();
  typedef MetricType::ScalesType ScalesType;
  ScalesType scales( numberOfParameters );
  scales.Fill( 1.0 );
  metric->SetDerivativeStepLengthScales(scales);

  // Set the metric for the joint histogram writer
  jointHistogramWriter.SetMetric( metric );
  
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


  fixedNormalizer->Update();
  registration->SetFixedImageRegion( 
       fixedNormalizer->GetOutput()->GetBufferedRegion() );

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y
  
  registration->SetInitialTransformParameters( initialParameters );


  optimizer->SetLearningRate( 10.0 );
  optimizer->SetNumberOfIterations( 200 );
  optimizer->MaximizeOn();


  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );
  
  initialHistogramFile = argv[5];


  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
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
  jointHistogramWriter.WriteHistogramFile( argv[6] );

  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;

  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( finalParameters );

  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
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
  writer->Update();

  return EXIT_SUCCESS;
}



// Software Guide : BeginLatex
// Mutual information attempts to re-group the joint entropy histograms into
// a more ``meaningful" formation. An optimizer that minimizes the joint entropy
// seeks a transform that produces a small number of high value bins and several
// zero bins. Multi-modality registration seeks such a transform while also 
// attempting to maximize the information contribution by the fixed and the moving
// images in the overalp region of the metric.
// 
// A T1 MRI (fixed image) and a proton density MRI (moving image) as shown in Figure
// \ref{fig:FixedMovingImageRegistration2}
// are provided as input to this example.
//
// Figure \ref{fig:JointEntropyHistograms} shows the joint histograms before and 
// after registration.
// \begin{figure}
// \center
// \includegraphics[width=0.44\textwidth]{JointEntropyHistogramPriorToRegistration.eps}
// \includegraphics[width=0.44\textwidth]{JointEntropyHistogramAfterRegistration.eps}
// \itkcaption[Multi-modality joint histograms]{Joint entropy histograms before and 
// after registration. The final transform was within half a pixel of true misalignment.}
// \label{fig:JointEntropyHistograms}
// \end{figure}
//  Software Guide : EndLatex 

