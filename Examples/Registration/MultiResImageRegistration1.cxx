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
// This is an example of using the multi-resolution registration framework.
//
// \index{itk::ImageRegistrationMethod!Multi-Resolution|textbf}
// \index{itk::ImageRegistrationMethod!Multi-Modality|textbf}
//
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
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"

//
//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
#include "itkCommand.h"
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

// Software Guide : BeginLatex
//
// A command for reducing maximum and minimum length parameter of
// optimizer
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
template <typename TRegistration>
class RegistrationInterfaceCommand : public itk::Command 
{
public:
  typedef  RegistrationInterfaceCommand   Self;
  typedef  itk::Command                   Superclass;
  typedef  itk::SmartPointer<Self>        Pointer;
  itkNewMacro( Self );
protected:
  RegistrationInterfaceCommand() {};
public:
  typedef   TRegistration                              RegistrationType;
  typedef   RegistrationType *                         RegistrationPointer;
  typedef   itk::RegularStepGradientDescentOptimizer   OptimizerType;
  typedef   OptimizerType *                            OptimizerPointer;

  void Execute(const itk::Object * object, const itk::EventObject & event)
  {
    std::cout << "in here" << std::endl;
  }

  void Execute(itk::Object * object, const itk::EventObject & event)
  {
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }

    RegistrationPointer registration =
                        dynamic_cast<RegistrationPointer>( object );

    if ( registration->GetCurrentLevel() == 0 )
      {
      return;
      }

    OptimizerPointer optimizer = dynamic_cast< OptimizerPointer >( 
                       registration->GetOptimizer() );

    optimizer->SetMaximumStepLength( 
      0.1 * optimizer->GetMaximumStepLength() );

    optimizer->SetMinimumStepLength(
      0.1 * optimizer->GetMinimumStepLength() );

  }
};
// Software Guide : EndCodeSnippet


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
  //  Floating point internal image type required for
  //  the image pyramids.
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef   float     InternalPixelType;

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::TranslationTransform< double, Dimension > TransformType;
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::MattesMutualInformationImageToImageMetric< 
                                          InternalImageType, 
                                          InternalImageType >    MetricType;


  //  Software Guide : BeginLatex
  //  
  //  Talk about MultiResolutionImageRegistrationMethod.
  //
  //  Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet
  typedef itk::MultiResolutionImageRegistrationMethod< 
                                    InternalImageType, 
                                    InternalImageType    > RegistrationType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Talk about the image pyramids
  //
  //  Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    InternalImageType,
                                    InternalImageType  >    FixedImagePyramidType;

 
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    InternalImageType,
                                    InternalImageType  >   MovingImagePyramidType;

  // Software Guide: EndCodeSnippet

  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  MetricType::Pointer         metric        = MetricType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );
  registration->SetMetric( metric  );
  

  //  Software Guide : BeginLatex
  //  
  //  Pyramid created and connected to multi-resolution registration
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FixedImagePyramidType::Pointer fixedImagePyramid = 
      FixedImagePyramidType::New();

  MovingImagePyramidType::Pointer movingImagePyramid =
      MovingImagePyramidType::New();
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  //  Software Guide : BeginLatex
  //  
  //  Need to cast input image to the internal image type.
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

  optimizer->SetMaximumStepLength( 16.00 );  
  optimizer->SetMinimumStepLength( 2.5 );
  optimizer->SetNumberOfIterations( 200 );

  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );

  //  Software Guide : BeginLatex
  //  
  //  Set the number of resolution levels - uses default schedule
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetNumberOfLevels( 3 );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  Set up an registration interface command and connect it
  //  to the registration method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef RegistrationInterfaceCommand<RegistrationType> CommandType;

  CommandType::Pointer command = CommandType::New();
  registration->AddObserver( itk::IterationEvent(), command );
  // Software Guide : EndCodeSnippet

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
  //  Let's execute this example using the same multi-modality images
  //  as before.
  //  The registration converged after 24 iterations and produce as result the
  //  parameters:
  //
  //  \begin{verbatim}
  //  Translation X = 13.1719
  //  Translation Y = 16.9006
  //  \end{verbatim}
  // 
  //  These values are very close match to 
  //  the true misaligment introduced in the moving image.
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
  // \includegraphics[width=5cm]{ImageRegistration4Output.eps}
  // \includegraphics[width=5cm]{ImageRegistration4CheckerboardBefore.eps}
  // \includegraphics[width=5cm]{ImageRegistration4CheckerboardAfter.eps}
  // \caption{Mapped moving image (left) and composition of fixed and moving
  // images before (center) and after (right) registration.}
  // \label{fig:ImageRegistration4Output}
  // \end{figure}
  //
  //  The result of the resampling the moving image is presented in the left
  //  side of Figure \ref{fig:ImageRegistration4Output}. The center and right
  //  parts of the figure present a checkerboard composite of the fixed and
  //  moving images before and after registration.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[height=6cm]{ImageRegistration4TraceTranslations.eps}
  // \includegraphics[height=6cm]{ImageRegistration4TraceMetric.eps}
  // \caption{Sequence of translations and metric values at each iteration of the optimizer.}
  // \label{fig:ImageRegistration4TraceTranslations}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration4TraceTranslations} (left) presents the
  //  sequence of translations followed by the optimizer as it searched the
  //  parameter space. The right side of the same figure shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //
  //
  //  Software Guide : EndLatex 


  return 0;

}

