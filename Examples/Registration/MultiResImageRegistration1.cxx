/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MultiResImageRegistration1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{itk::ImageRegistrationMethod!Multi-Resolution|textbf}
// \index{itk::ImageRegistrationMethod!Multi-Modality|textbf}
// \index{itk::MultiResolutionImageRegistrationMethod|textbf}
//
// This is an example illustrating the use of 
// \code{MultiResolutionImageRegistrationMethod} to solve our simple
// multi-modality registration problem. In addition to the 
// two input Images, a Transform, a Metric, an Interpolator and
// an Optimizer, the multi-resolution frameworks also require
// two Image Pyramids for creating the sequence downsampled images. 
// The following headers declare the basic components of 
// the registration method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"


// Software Guide : BeginLatex
//
// \code{MultiResolutionImageRegistrationMethod} solves a registration
// problem in a coarse to fine manner as illustrated in Figure
// \ref{fig:MultiResRegistrationConcept}. The registration is first 
// performed at the coarsest level using the images for the first
// level of the fixed and moving image pyramids. The transform parameters
// determined by the registration is then used to initialize registration
// at the next finer level using images from the second level of the
// pyramids. This process is repeated as we work down to the last level
// of the pyramids.
//
// \begin{figure}
// \center
// \includegraphics[width=14cm]{MultiResRegistrationConcept.eps}
// \caption{Conceptual representation of Multi-Resolution registration.}
// \label{fig:MultiResRegistrationConcept}
// \end{figure}
//
// Software Guide : EndLatex 


// Software Guide : BeginLatex
//
// In a typical registration scenario, a user will typically want
// to tweak component settings or even swap out components between
// multi-resolution levels. For example, when optimizing at a 
// coarse, it may be possible to take more aggressive step sizes
// and have a more relaxed convergence criterion. Another scheme,
// may be use a simple translation transform for the initial
// coarse registration and upgrade to an affine transform
// at the finer levels.
//
// Tweaking of the components between resolution level can
// be done using ITK's implementation of the \emph{Observer/Command}
// pattern. Before beginning registration at each resolution level,
// \code{MultiResolutionImageRegistrationMethod} invoke a 
// \code{itk::IterationEvent}. The registration components can
// be changed by implementing an \code{itk::Command} which is
// registered to respond to the event. A brief description of 
// event and commands was previously presented in section 
// \ref{sec:MonitoringImageRegistration}.
//
// We will illustrate this mechanism in the example by changing
// the parameters of the optimizer between each resolution level
// by way of a simple interface command. First, the header file
// of the \code{Command} class needs to be included.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCommand.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Our new interface command class is called 
// \code{RegistrationIterfaceCommand}. It derives from the 
// \code{itk::Command} class and is templated over the 
// multi-resolution registration type.
// \code{RegistrationInterfaceCommand}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
template <typename TRegistration>
class RegistrationInterfaceCommand : public itk::Command 
{
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We then define \code{Self}, \code{Superclass}, \code{Pointer},
// \code{New()} and constructor in a similar fashion to
// \code{CommandIterationUpdate} class in section 
// \ref{sec:MonitoringImageRegistration}.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
public:
  typedef  RegistrationInterfaceCommand   Self;
  typedef  itk::Command                   Superclass;
  typedef  itk::SmartPointer<Self>        Pointer;
  itkNewMacro( Self );
protected:
  RegistrationInterfaceCommand() {};
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// For convenience, we declare types useful for converting pointers
// in the \code{Execute()} method.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
public:
  typedef   TRegistration                              RegistrationType;
  typedef   RegistrationType *                         RegistrationPointer;
  typedef   itk::RegularStepGradientDescentOptimizer   OptimizerType;
  typedef   OptimizerType *                            OptimizerPointer;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Two arguments are passed to the \code{Execute()} method: the first
// is the pointer to the object which invoked the event and the 
// second is the event that was invoked.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
  void Execute(itk::Object * object, const itk::EventObject & event)
  {
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// First we verify if that the event invoked is of the right type.
// If not, we return without any further action.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We then convert the input object pointer to a \code{RegistrationPointer}.
// Note that no error checking is done here to check if the 
// \code{dynamic\_cast} was successful since we know the actual object
// is a multi-resolution registration method. 
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
    RegistrationPointer registration =
                        dynamic_cast<RegistrationPointer>( object );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// If this the first resolution level we set the maximum step length
// (representing the first step size) and the minimum step length
// (representing the convergence criterion) to large values.
// At each subsequent resolution level, we will reduce these step
// length by a factor of 10.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
    OptimizerPointer optimizer = dynamic_cast< OptimizerPointer >( 
                       registration->GetOptimizer() );

    if ( registration->GetCurrentLevel() == 0 )
      {
      optimizer->SetMaximumStepLength( 16.00 );  
      optimizer->SetMinimumStepLength( 2.5 );
      }
    else
      {
      optimizer->SetMaximumStepLength( 
          0.1 * optimizer->GetMaximumStepLength() );

      optimizer->SetMinimumStepLength(
          0.1 * optimizer->GetMinimumStepLength() );
      }

  }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Another version of \code{Execute()} accepting a \code{const} input object
// is also require since is defined as a pure virtual in the base class.
// This version simply returns without taking any action.
//
// Software Guide : EndLatex
// Software Guide : BeginCodeSnippet
  void Execute(const itk::Object * object, const itk::EventObject & event)
    { return; }

};
// Software Guide : EndCodeSnippet


//
//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
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
  typedef   itk::RegularStepGradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *           OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event)
  {
    Execute( (const itk::Object *)caller, event);
  }

  void Execute(const itk::Object * object, const itk::EventObject & event)
  {
    OptimizerPointer optimizer = 
                      dynamic_cast< OptimizerPointer >( object );
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
      std::cout << optimizer->GetCurrentIteration() << "   ";
      std::cout << optimizer->GetValue() << "   ";
      std::cout << optimizer->GetCurrentPosition();
  }
};



int main( int argc, char **argv )
{


  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile [differenceImage]" << std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;
  
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  //  Software Guide : BeginLatex
  //  
  //  The fixed and moving image type are defined as with previous examples.
  //  Due to recursive nature of which the downsampled images are computed
  //  by the image pyramids, the output images are required to have
  //  real pixel types. We declare this internal image type to be:
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef   float     InternalPixelType;

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  The types for the registration components are then derived using
  //  the internal image type.
  // 
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    InternalImageType,
                                    double             > InterpolatorType;

  typedef itk::MattesMutualInformationImageToImageMetric< 
                                    InternalImageType, 
                                    InternalImageType >   MetricType;
  typedef itk::MultiResolutionPyramidImageFilter<
                                    InternalImageType,
                                    InternalImageType >   FixedImagePyramidType;
  // Software Guide: EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // In the multi-resolution framework, a \code{MultiResolutionPyramidImageFilter}
  // is used to create a pyramid of downsampled images. The size of each downsample image
  // is specified by a user in the form of schedule of shrink factors. A description of
  // the filter and multi-resolution schedules are described in detail in section
  // \ref{sec:ImagePyramids}. For this example, we will simply use the default schedule. 
  // 
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef itk::MultiResolutionPyramidImageFilter<
                                    InternalImageType,
                                    InternalImageType >   MovingImagePyramidType;
  typedef itk::MultiResolutionImageRegistrationMethod< 
                                    InternalImageType, 
                                    InternalImageType >   RegistrationType;
 
  // Software Guide: EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  All the components are instantiated using their \code{New()} method
  //  and connected to the registration object as in previous example.  
  //
  //  Software Guide : EndLatex 
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  MetricType::Pointer         metric        = MetricType::New();

  FixedImagePyramidType::Pointer fixedImagePyramid = 
      FixedImagePyramidType::New();
  MovingImagePyramidType::Pointer movingImagePyramid =
      MovingImagePyramidType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );
  registration->SetMetric( metric  );
  registration->SetFixedImagePyramid( fixedImagePyramid );
  registration->SetMovingImagePyramid( movingImagePyramid );
  

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  //  Software Guide : BeginLatex
  //  
  //  The fixed and moving images are read in from file. Before connecting
  //  these images to the registration we need to cast them to the internal
  //  image type using \code{CastImageFilter} objects.
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< 
                        FixedImageType, InternalImageType > FixedCastFilterType;
  
  typedef itk::CastImageFilter< 
                        MovingImageType, InternalImageType > MovingCastFilterType;

  FixedCastFilterType::Pointer fixedCaster   = FixedCastFilterType::New();
  MovingCastFilterType::Pointer movingCaster = MovingCastFilterType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  The output of the readers is connected as input to the cast
  //  filters. The inputs to the registration method are taken from the
  //  cast filters. 
  //
  //  Software Guide : EndLatex 
  // Software Guide : BeginCodeSnippet
  fixedCaster->SetInput(  fixedImageReader->GetOutput() );
  movingCaster->SetInput( movingImageReader->GetOutput() );

  registration->SetFixedImage(    fixedCaster->GetOutput()    );
  registration->SetMovingImage(   movingCaster->GetOutput()   );
  // Software Guide : EndCodeSnippet


  fixedCaster->Update();

  registration->SetFixedImageRegion( 
       fixedCaster->GetOutput()->GetBufferedRegion() );
   

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y
  
  registration->SetInitialTransformParameters( initialParameters );

  metric->SetNumberOfHistogramBins( 50 );
  metric->SetNumberOfSpatialSamples( 1000 );

  optimizer->SetNumberOfIterations( 200 );

  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );


  //  Software Guide : BeginLatex
  //  
  //  One all the registration components are in place we can create
  //  an instance of our interface command and connect it to the
  //  registration object using the \code{AddObserver()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef RegistrationInterfaceCommand<RegistrationType> CommandType;

  CommandType::Pointer command = CommandType::New();
  registration->AddObserver( itk::IterationEvent(), command );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We set the number of multi-resolution to 3 and trigger the registration
  //  process by calling \code{StartRegistration()}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetNumberOfLevels( 3 );

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
  // Software Guide : EndCodeSnippet

  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  double TranslationAlongX = finalParameters[0];
  double TranslationAlongY = finalParameters[1];
  
  unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  
  double bestValue = optimizer->GetValue();

  //
  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX  << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
  //  
  //  Let's execute this example using the same multi-modality images as
  //  before.  The registration converged at the first level 
  //  after 4 iterations with translation parameters of (13.8663, 18.9939).
  //  The second level converged after 5 iterations with result of
  //  (13.1035, 17.19). Registration converged after 1 iteration at the
  //  last level with the final result being:
  //
  //  \begin{verbatim}
  //  Translation X = 13.1035
  //  Translation Y = 17.19
  //  \end{verbatim}
  //
  //  These values are very close match to the true misaligment introduced in
  //  the moving image.
  //
  //  Software Guide : EndLatex 

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


  //  Software Guide : BeginLatex
  // 
  // \begin{figure}
  // \center
  // \includegraphics[width=5cm]{MultiResImageRegistration1Output.eps}
  // \includegraphics[width=5cm]{MultiResImageRegistration1CheckerboardBefore.eps}
  // \includegraphics[width=5cm]{MultiResImageRegistration1CheckerboardAfter.eps}
  // \caption{Mapped moving image (left) and composition of fixed and moving
  // images before (center) and after (right) registration.}
  // \label{fig:MultiResImageRegistration1Output}
  // \end{figure}
  //
  //  The result of the resampling the moving image is presented in the left
  //  side of Figure \ref{fig:MultiResImageRegistration1Output}. The center and right
  //  parts of the figure present a checkerboard composite of the fixed and
  //  moving images before and after registration.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[height=6cm]{MultiResImageRegistration1TraceTranslations.eps}
  // \includegraphics[height=6cm]{MultiResImageRegistration1TraceMetric.eps}
  // \caption{Sequence of translations and metric values at each iteration of the optimizer.}
  // \label{fig:MultiResImageRegistration1Trace}
  // \end{figure}
  //
  //  Figure \ref{fig:MultiResImageRegistration1Trace} (left) presents the
  //  sequence of translations followed by the optimizer as it searched the
  //  parameter space. The right side of the same figure shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //  From the trace, we can see that with the more aggressive optimization
  //  parameters we get quite close to the optimal value within 4 iterations
  //  with the remaining iterations just doing fine adjustments. This is compare
  //  to the single resolution example where 24 iterations were required as
  //  more conservative optimization parameters had to be used.
  //
  //  Software Guide : EndLatex 


  return 0;

}

