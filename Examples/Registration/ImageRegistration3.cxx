/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration3.cxx
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
// Given the numerous parameters involved in tunning a registration method for
// a particular application it is common to be faced with  a registration
// process that runs for several minutes and ends up with a usless result.  In
// order to avoid this situation it is quite helpful to track the evolution of
// the registration as it progress. The following section illustrates the
// mechanisms provided in ITK for monitoring the activity of the
// \code{ImageRegistrationMethod} class.
//
// Insight implements the \emph{Observer/Command} pattern design
// \cite{Gamma1995}. The classes involved in this implementation are the
// \code{itk::Object}, \code{itk::Command} and \code{itk::Event} classes. The
// \code{itk::Object} class is the base class of most ITK objects. This class
// holds a linked list of pointers to eventual observers. The role of Observers
// is played by the \code{itk::Command} class.  Observer register themselves
// after the \code{itk::Object} declaring that they are interested in receiving
// notice when a particular event happens. A set of events are represented by
// the hierarchy of the \code{itk::Event} class. Typical events are
// \code{Start}, \code{End}, \code{Progress} and \code{Iteration}.
//
// Registration is controled by an \code{itk::Optimizer} which in general
// executes an iterative process. Every \code{Optimizer} invokes an
// \code{itk::IterationEvent} at the end of each iteration. When an event is
// invoked by an object, this object goes through its list of registered
// observers (\code{itk::Command}s) and checks if any one of them declared to
// be interested in the current event type. Whenever such an observer is found,
// its corresponding \code{Execute()} method is invoked.  On this context
// \code{Execute()} methods should be considered as \emph{callbacks}. As such,
// some of the common-sense rules of callbacks should be respected. For
// example, \code{Execute()} methods should not perform heavy computational
// tasks.  They are supposed to execute rapid and short pieces of code like
// printing out a message or updating a value in a GUI.
//
//
//
// \index{itk::ImageRegistrationMethod!Monitoring|textbf}
//
//
// Software Guide : EndLatex 


#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"



#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"


//  Software Guide : BeginLatex
//
//  The following code illustrates a simple way of creating a new
//  Observer/Command capable of monitoring a registration process. This new
//  class derives from the \code{itk::Command} class and provides a specific
//  implementation of the \code{Execute()} method.  First, the header file of
//  the \code{Command} class must be included.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCommand.h"
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  Our custom comman class is called here \code{CommandIterationUpdate}. It
//  derives from the \code{itk::Command} class and declares for convenience the
//  types \code{Self} and \code{Superclass}. This facilitate to standarize some
//  further code in macros.
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
  //  the \code{New()} method. Those of curious minds are invited to see the
  //  details of the macro in the file \code{itkMacro.h} on the
  //  \code{Insight/Code/Common} directory. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  itkNewMacro( Self );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  In order to prevent the constructor of this class to be called by
  //  accident, the constructor is declared \code{protected} here. This helps
  //  to ensure that the object is always constructed using the \code{New()}
  //  method. 
  //
  //  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
protected:

  CommandIterationUpdate() {};

// Software Guide : EndCodeSnippet
 

public:

  //  Software Guide : BeginLatex
  //
  //  Since this object will be observing the Optimizer, the following typedefs
  //  are useful for converting pointers when the Execute method is invoked.
  //  Note the use of \code{const} on the declaration of
  //  \code{OptimizerPointer}.  This is relevant since the observer is not
  //  intended to modify the optimizer in any way. A \code{const} interface
  //  makes the compiler help to enforce that all actions invoked on the
  //  optimizer are read-only.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RegularStepGradientDescentOptimizer     OptimizerType;

  typedef   const OptimizerType   *    OptimizerPointer;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  ITK enforces const-correctness. There is hence a distinction between the
  //  \code{Execute()} method that can be invoked from a \code{const} object
  //  and the one that can be invoked from a \code{non-const} object. In this
  //  particular example the \code{non-const} version simply invoke the
  //  \code{const}  version. In a more elaborate situation the implementation
  //  of both \code{Execute()} methods could be quite different.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  void Execute(itk::Object *caller, const itk::EventObject & event)
  {
    Execute( (const itk::Object *)caller, event);
  }
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  // Finally we get to the heart of the observer, the \code{Execute()} method.
  // Two arguments are passed to this method. The first argument is the pointer
  // to the object who invoked the event. The second argument is the event that
  // was invoked.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  void Execute(const itk::Object * object, const itk::EventObject & event)
  {
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Note that the first argument is a pointer to \code{itk::Object} even
  //  though the actual object invoking the event is probably a subclass of
  //  \code{itk::Object}. In our case we know that the actual object is an
  //  optimizer. We have then good reasons to risk a \code{dynamic\_cast} to the
  //  real type of the object. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
    OptimizerPointer optimizer = 
                      dynamic_cast< OptimizerPointer >( object );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The next step is to verify that the event invoked is actually the one in
  //  which we are interested. This is checked using the RTTI\footnote{RTTI
  //  stands for: Run-Time Type Information} support. The \code{typeid}
  //  function allows to compare the actual type of two references or pointers.
  //  In this case we compare the type of the received event with the type of
  //  \code{itk::IterationEvent}. If they happen to be different the method
  //  return without any further action.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  But if the event type matches what we are looking for, we are ready for
  //  querying data from the optimizer. Here, for example, we got the current
  //  number of iterations, the current value of the cost function and the
  //  current position on the parameter space. All of these values are printed
  //  out to the standard output. You could imagine more elaborate actions like
  //  updating a GUI and probably refreshing a visualization pipeline.
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
  //  This concludes our revision of the minimal \code{Command} class capable
  //  of observing our registration method. We can now move on to configure a
  //  registration process.
  //
  //  Software Guide : EndLatex 
 
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

  optimizer->MaximizeOn();

  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();

  optimizer->AddObserver( itk::IterationEvent(), observer );

  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "Caught expected ExceptionObject" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 

  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  const double TranslationAlongX = finalParameters[0];
  const double TranslationAlongY = finalParameters[1];
  
  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();
  
  const double bestValue = optimizer->GetValue();




  //
  // Prepare the resampling filter in order to map the moving image.
  //
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



  //
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





  return 0;

}

