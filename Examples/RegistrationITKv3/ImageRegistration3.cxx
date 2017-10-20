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
// Given the numerous parameters involved in tuning a registration method for
// a particular application, it is not uncommon for a registration process to
// run for several minutes and still produce a useless result.  To avoid
// this situation it is quite helpful to track the evolution of the
// registration as it progresses. The following section illustrates the
// mechanisms provided in ITK for monitoring the activity of the
// ImageRegistrationMethod class.
//
// Insight implements the \emph{Observer/Command} design pattern
// \cite{Gamma1995}.
// The classes involved in this implementation are the \doxygen{Object},
// \doxygen{Command} and \doxygen{EventObject} classes. The Object
// is the base class of most ITK objects. This class maintains a linked
// list of pointers to event observers. The role of observers is played by
// the Command class.  Observers register themselves with an
// Object, declaring that they are interested in receiving
// notification when a particular event happens. A set of events is
// represented by the hierarchy of the Event class. Typical events
// are \code{Start}, \code{End}, \code{Progress} and \code{Iteration}.
//
// Registration is controlled by an \doxygen{Optimizer}, which generally
// executes an iterative process. Most Optimizer classes invoke an
// \doxygen{IterationEvent} at the end of each iteration. When an event is
// invoked by an object, this object goes through its list of registered
// observers (Commands) and checks whether any one of them has expressed
// interest in the current event type. Whenever such an observer is found,
// its corresponding \code{Execute()} method is invoked.  In this context,
// \code{Execute()} methods should be considered \emph{callbacks}.  As such,
// some of the common sense rules of callbacks should be respected.  For
// example, \code{Execute()} methods should not perform heavy computational
// tasks.  They are expected to execute rapidly, for example, printing out a
// message or updating a value in a GUI.
//
// \index{itk::ImageRegistrationMethod!Monitoring}
//
//
// Software Guide : EndLatex


#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkRegularStepGradientDescentOptimizer.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"


//  Software Guide : BeginLatex
//
//  The following code illustrates a simple way of creating a
//  Observer/Command to monitor a registration process. This new
//  class derives from the Command class and provides a specific
//  implementation of the \code{Execute()} method.  First, the header file of
//  the Command class must be included.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCommand.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  Our custom command class is called \code{CommandIterationUpdate}. It
//  derives from the Command class and declares for convenience the
//  types \code{Self} and \code{Superclass}. This facilitate the use of
//  standard macros later in the class implementation.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
class CommandIterationUpdate : public itk::Command
{
public:
  typedef  CommandIterationUpdate   Self;
  typedef  itk::Command             Superclass;
// Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The following typedef declares the type of the SmartPointer capable of
  //  holding a reference to this object.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::SmartPointer<Self>  Pointer;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The \code{itkNewMacro} takes care of defining all the necessary code for
  //  the \code{New()} method. Those with curious minds are invited to see the
  //  details of the macro in the file \code{itkMacro.h} in the
  //  \code{Insight/Code/Common} directory.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itkNewMacro( Self );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  In order to ensure that the \code{New()} method is used to instantiate
  //  the class (and not the C++ \code{new} operator), the constructor is
  //  declared \code{protected}.
  //
  //  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

protected:
  CommandIterationUpdate() {};
// Software Guide : EndCodeSnippet

public:
  //  Software Guide : BeginLatex
  //
  //  Since this Command object will be observing the optimizer,
  //  the following typedefs are useful for converting pointers when the
  //  \code{Execute()} method is invoked.  Note the use of \code{const} on
  //  the declaration of \code{OptimizerPointer}.  This is relevant since, in
  //  this case, the observer is not intending to modify the optimizer in any
  //  way. A \code{const} interface ensures that all operations invoked on the
  //  optimizer are read-only.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegularStepGradientDescentOptimizer     OptimizerType;
  typedef const OptimizerType *                        OptimizerPointer;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  ITK enforces const-correctness. There is hence a distinction between the
  //  \code{Execute()} method that can be invoked from a \code{const} object
  //  and the one that can be invoked from a non-\code{const} object. In this
  //  particular example the non-\code{const} version simply invoke the
  //  \code{const}  version. In a more elaborate situation the implementation
  //  of both \code{Execute()} methods could be quite different. For example,
  //  you could imagine a non-\code{const} interaction in which the observer
  //  decides to stop the optimizer in response to a divergent behavior. A
  //  similar case could happen when a user is controlling the registration
  //  process from a GUI.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // Finally we get to the heart of the observer, the \code{Execute()} method.
  // Two arguments are passed to this method. The first argument is the pointer
  // to the object that invoked the event. The second argument is the event that
  // was invoked.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    // Software Guide : EndCodeSnippet


    //  Software Guide : BeginLatex
    //
    //  Note that the first argument is a pointer to an Object even
    //  though the actual object invoking the event is probably a subclass of
    //  Object. In our case we know that the actual object is an
    //  optimizer. Thus we can perform a \code{dynamic\_cast} to the real type
    //  of the object.
    //
    //  Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
    // Software Guide : EndCodeSnippet


    //  Software Guide : BeginLatex
    //
    //  The next step is to verify that the event invoked is actually the one in
    //  which we are interested. This is checked using the RTTI\footnote{RTTI
    //  stands for: Run-Time Type Information} support. The \code{CheckEvent()}
    //  method allows us to compare the actual type of two events.  In this case
    //  we compare the type of the received event with an IterationEvent. The
    //  comparison will return true if \code{event} is of type
    //  \code{IterationEvent} or derives from \code{IterationEvent}.  If we find
    //  that the event is not of the expected type then the \code{Execute()}
    //  method of this command observer should return without any further action.
    //
    // \index{itk::EventObject!CheckEvent}
    //
    //  Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    if( ! itk::IterationEvent().CheckEvent( &event ) )
      {
      return;
      }
    // Software Guide : EndCodeSnippet


    //  Software Guide : BeginLatex
    //
    //  If the event matches the type we are looking for, we are ready to
    //  query data from the optimizer. Here, for example, we get the current
    //  number of iterations, the current value of the cost function and the
    //  current position on the parameter space. All of these values are printed
    //  to the standard output. You could imagine more elaborate actions like
    //  updating a GUI or refreshing a visualization pipeline.
    //
    //  Software Guide : EndLatex

    // Software Guide : BeginCodeSnippet
    std::cout << optimizer->GetCurrentIteration() << " = ";
    std::cout << optimizer->GetValue() << " : ";
    std::cout << optimizer->GetCurrentPosition() << std::endl;
    // Software Guide : EndCodeSnippet
    }

  //  Software Guide : BeginLatex
  //
  //  This concludes our implementation of a minimal Command class
  //  capable of observing our registration method.  We can now move on to
  //  configuring the registration process.
  //
  //  Software Guide : EndLatex
};


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << "outputImagefile " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::TranslationTransform< double, Dimension > TransformType;

  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  typedef itk::LinearInterpolateImageFunction<
                                    MovingImageType,
                                    double             > InterpolatorType;

  typedef itk::ImageRegistrationMethod<
                                    FixedImageType,
                                    MovingImageType   >  RegistrationType;

  typedef itk::MeanSquaresImageToImageMetric<
                                      FixedImageType,
                                      MovingImageType >  MetricType;

  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );

  MetricType::Pointer         metric        = MetricType::New();

  registration->SetMetric( metric  );

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );

  fixedImageReader->Update(); // This is needed to make the BufferedRegion below valid.

  registration->SetFixedImageRegion(
       fixedImageReader->GetOutput()->GetBufferedRegion() );

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y

  registration->SetInitialTransformParameters( initialParameters );

  optimizer->SetMaximumStepLength( 4.00 );
  optimizer->SetMinimumStepLength( 0.01 );
  optimizer->SetNumberOfIterations( 200 );

  optimizer->MaximizeOff();


  //  Software Guide : BeginLatex
  //
  //  Once all the registration components are in place we can create one
  //  instance of our observer. This is done with the standard \code{New()}
  //  method and assigned to a SmartPointer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{ImageRegistration3Observer}
  // \itkcaption[Command/Observer and the Registration Framework]{Interaction
  // between the Command/Observer and the Registration Method.}
  // \label{fig:ImageRegistration3Observer}
  // \end{figure}
  //
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  The newly created command is registered as observer on the
  //  optimizer, using the \code{AddObserver()} method. Note
  //  that the event type is provided as the first argument to this
  //  method. In order for the RTTI mechanism to work correctly, a newly
  //  created event of the desired type must be passed as the first
  //  argument. The second argument is simply the smart pointer to the
  //  optimizer. Figure \ref{fig:ImageRegistration3Observer} illustrates the
  //  interaction between the Command/Observer class and the registration
  //  method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  optimizer->AddObserver( itk::IterationEvent(), observer );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  At this point, we are ready to execute the registration. The
  //  typical call to \code{Update()} will do it. Note again the
  //  use of the \code{try/catch} block around the \code{Update()}
  //  method in case an exception is thrown.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The registration process is applied to the following images in \code{Examples/Data}:
  //
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png}
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  It produces the following output.
  //
  //  \begin{verbatim}
  //   0 = 4499.45 : [2.9287, 2.72447]
  //   1 = 3860.84 : [5.62751, 5.67683]
  //   2 = 3450.68 : [8.85516, 8.03952]
  //   3 = 3152.07 : [11.7997, 10.7469]
  //   4 = 2189.97 : [13.3628, 14.4288]
  //   5 = 1047.21 : [11.292, 17.851]
  //   6 = 900.189 : [13.1602, 17.1372]
  //   7 = 19.6301 : [12.3268, 16.5846]
  //   8 = 237.317 : [12.7824, 16.7906]
  //   9 = 38.1331 : [13.1833, 17.0894]
  //   10 = 18.9201 : [12.949, 17.002]
  //   11 = 1.15456 : [13.074, 16.9979]
  //   12 = 2.42488 : [13.0115, 16.9994]
  //   13 = 0.0590549 : [12.949, 17.002]
  //   14 = 1.15451 : [12.9803, 17.001]
  //   15 = 0.173731 : [13.0115, 16.9997]
  //   16 = 0.0586584 : [12.9959, 17.0001]
  //  \end{verbatim}
  //  You can verify from the code in the \code{Execute()} method that the first
  //  column is the iteration number, the second column is the metric value and
  //  the third and fourth columns are the parameters of the transform, which
  //  is a $2D$ translation transform in this case. By tracking these values as
  //  the registration progresses, you will be able to determine whether the
  //  optimizer is advancing in the right direction and whether the step-length
  //  is reasonable or not.  That will allow you to interrupt the registration
  //  process and fine-tune parameters without having to wait until the
  //  optimizer stops by itself.
  //
  //  Software Guide : EndLatex


  ParametersType finalParameters = registration->GetLastTransformParameters();

  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();

  std::cout << "Registration done !" << std::endl;
  std::cout << "Number of iterations = " << numberOfIterations << std::endl;
  std::cout << "Translation along X  = " << TranslationAlongX << std::endl;
  std::cout << "Translation along Y  = " << TranslationAlongY << std::endl;
  std::cout << "Optimal metric value = " << bestValue << std::endl;


  // Prepare the resampling filter in order to map the moving image.
  //
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


  // Prepare a writer and caster filters to send the resampled moving image to
  // a file
  //
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
