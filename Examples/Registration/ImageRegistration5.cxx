/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration5.cxx
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
// This example illustrates the use of the \code{CenteredRigid2D} transform.for
// performing rigid registration in $2D$. The code of this example is for the
// most part identical to the one presented in \ref{sec:HelloWordRegistration}.
// The main difference is the use of the rigid $2D$ transform instead of the
// \code{Translation} transform.
//
// \index{itk::CenteredRigid2DTransform!textbf}
//
//
// Software Guide : EndLatex 

#include "itkImageRegistrationMethod.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"



//  Software Guide : BeginLatex
//  
//  In addition to the headers included in previous examples, here the header
//  of the \code{CenteredRigid2DTransform} must be included.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCenteredRigid2DTransform.h"
// Software Guide : EndCodeSnippet




#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"




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
  typedef itk::SmartPointer<Self>  Pointer;
  itkNewMacro( Self );
protected:
  CommandIterationUpdate() {};
public:
  typedef itk::RegularStepGradientDescentOptimizer     OptimizerType;
  typedef   const OptimizerType   *    OptimizerPointer;

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
    std::cerr << "outputImagefile" << std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //  
  //  The Transform is instantiated using the code below. The only template
  //  parameter of this class is the representation type of the space
  //  coordinates.
  //
  //  \index{itk::CenteredRigid2DTransform!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredRigid2DTransform< double > TransformType;
  // Software Guide : EndCodeSnippet



  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  typedef itk::MeanSquaresImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;

  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;

  MetricType::Pointer         metric        = MetricType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  

  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );




  //  Software Guide : BeginLatex
  //
  //  The transform is constructed and passed to the registration method.
  //
  //  \index{itk::CenteredRigid2DTransform!New()}
  //  \index{itk::CenteredRigid2DTransform!Pointer}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer      transform     = TransformType::New();

  registration->SetTransform(     transform     );
  // Software Guide : EndCodeSnippet




  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );




  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );
  fixedImageReader->Update();

  registration->SetFixedImageRegion( 
     fixedImageReader->GetOutput()->GetBufferedRegion() );




  //  Software Guide : BeginLatex
  //  
  //  In this example, the input images are taken from readers. Here the
  //  readers are updated in order to ensure that the parameters of size,
  //  spacing of the images are valid when used to initialize the transform.
  //  The intention is to use the center of the fixed image as the rotation
  //  center and then use the vector between the fixed image center and the
  //  moving image center as the translation to be applied after the rotation.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fixedImageReader->Update();
  movingImageReader->Update();
  // Software Guide : EndCodeSnippet



  
  //  Software Guide : BeginLatex
  //  
  //  The center of rotation is computed using the origin, size and spacing of
  //  the fixed image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  const double * fixedSpacing = fixedImage->GetSpacing();
  const double * fixedOrigin  = fixedImage->GetOrigin();
  
  FixedImageType::SizeType fixedSize  = 
          fixedImage->GetLargestPossibleRegion().GetSize();
  
  TransformType::InputPointType centerFixed;
  
  centerFixed[0] = fixedOrigin[0] + fixedSpacing[0] * fixedSize[0] / 2.0;
  centerFixed[1] = fixedOrigin[1] + fixedSpacing[1] * fixedSize[1] / 2.0;
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The center of the moving image is computed in a similar way.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  const double * movingSpacing = movingImage->GetSpacing();
  const double * movingOrigin  = movingImage->GetOrigin();
  
  MovingImageType::SizeType movingSize = 
            movingImage->GetLargestPossibleRegion().GetSize();
  
  TransformType::InputPointType centerMoving;
  
  centerMoving[0] = movingOrigin[0] + movingSpacing[0] * movingSize[0] / 2.0;
  centerMoving[1] = movingOrigin[1] + movingSpacing[1] * movingSize[1] / 2.0;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //   In order to initialize the transform parameters, the most
  //   straightforward method is to setup the configuration of a transform and
  //   then get its parameters with the method \code{GetParameters()}. Here we
  //   initialize first the transform by passing the center of the fixed image
  //   as the rotation center with the \code{SetRotationCenter()} method. Then
  //   the translation is set as the vector relating the center of moving image
  //   to the center of the fixed image. This last definition is passed with
  //   the method \code{SetTranslation()}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  transform->SetCenterOfRotation( centerFixed );

  transform->SetTranslation( centerMoving - centerFixed );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  Let's finally initialize the rotation with a zero angle.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  transform->SetAngle( 0.0 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  We pass now the parameter of the current transform as the initial
  //  parameters to be used when the registration process starts.
  //
  //  Software Guide : EndLatex 
 
  registration->SetInitialTransformParameters( 
                                 transform->GetParameters() );


  //  Software Guide : BeginLatex
  //  
  //  Keeping in mind that the scale of units in rotation and translation are
  //  quite different, we take advantage of the scaling functionality provided
  //  by the optimizers. We know that the first element of the parameters array
  //  corresponds to the angle. For this reason we use a small scale in this
  //  first parameter.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef OptimizerType::ScalesType       OptimizerScalesType;

  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  optimizerScales[0] = 0.0001;
  optimizerScales[0] = 1.0;
  optimizerScales[0] = 1.0;
  optimizerScales[0] = 1.0;
  optimizerScales[0] = 1.0;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  We set also the normal parameters of the optimization method. In this
  //  case we are using a \code{RegularStepGradientDescentOptimizer}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 4.00 );  
  optimizer->SetMinimumStepLength( 0.01 );
  optimizer->SetNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet




  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );



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
  
  OptimizerType::ParametersType finalParameters = 
                    registration->GetLastTransformParameters();


  const double finalAngle           = finalParameters[0];
  const double finalRotationCenterX = finalParameters[1];
  const double finalRotationCenterY = finalParameters[2];
  const double finalTranslationX    = finalParameters[3];
  const double finalTranslationY    = finalParameters[4];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();

  //
  // Print out results
  //
  const double finalAngleInDegrees = finalAngle * 45.0 / atan(1.0);

  std::cout << "Result = " << std::endl;
  std::cout << " Angle         = " << finalAngle  << std::endl;
  std::cout << " Center X      = " << finalRotationCenterX  << std::endl;
  std::cout << " Center Y      = " << finalRotationCenterY  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
  //  
  //  Let's execute this example over some of the images provided in
  //  \code{Insight/Examples/Data}, for example:
  //  
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png} 
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally translating the first
  //  image by $(13,17)$ millimeters. Both images have unit-spacing and
  //  are shown in Figure \ref{fig:FixedMovingImageRegistration1}. The
  //  registration takes 18 iterations and produce as result the parameters:
  //
  //  \begin{verbatim}
  //  Translation X = 12.9903
  //  Translation Y = 17.0001
  //  \end{verbatim}
  // 
  //  As expected, these values match pretty well the misalignment
  //  intentionally introduced in the moving image.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=6cm]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=6cm]{BrainProtonDensitySliceShifted13x17y.eps}
  // \caption{Fixed and Moving image provided as input to the registration method.}
  // \label{fig:FixedMovingImageRegistration1}
  // \end{figure}
  //
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

  typedef itk::SquaredDifferenceImageFilter< 
                                  FixedImageType, 
                                  FixedImageType, 
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  difference->SetInput1( fixedImageReader->GetOutput() );
  difference->SetInput2( resample->GetOutput() );

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );  
  


  if( argc >= 5 )
    {
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }

 
  return 0;

}

