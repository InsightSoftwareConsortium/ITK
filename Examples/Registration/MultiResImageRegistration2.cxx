/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MultiResImageRegistration2.cxx
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
//  This example illustrates the use of more complex components of the
//  registration framework. In particular, it introduces the use of the
//  \doxygen{AffineTransform} and the importance of fine tunning the scale
//  parameters of the optimizer.
//
// \index{itk::ImageRegistrationMethod!AffineTransform}
// \index{itk::ImageRegistrationMethod!Scaling parameter space}
// \index{itk::AffineTransform!Image Registration|textbf}
//
// The AffineTransform is a linear transformation that maps lines into lines. It
// can be used to represent translations, rotations, anisotropic scaling,
// shearing or any combination of them. Details about the affine transform can be
// seen in section~\ref{sec:AffineTransform}.
//
// In order to use the \doxygen{AffineTransform} class the following header should
// be included.
//
// \index{itk::AffineTransform!Header}
//
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkAffineTransform.h"
// Software Guide : EndCodeSnippet


#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkImage.h"


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




//
//  The following section of code implements a Command observer
//  that will control the modification of optimizer parameters
//  at every change of resolution level.
//
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
  void Execute(itk::Object * object, const itk::EventObject & event)
  {
    if( typeid( event ) != typeid( itk::IterationEvent ) )
      {
      return;
      }
    RegistrationPointer registration =
                        dynamic_cast<RegistrationPointer>( object );
    OptimizerPointer optimizer = dynamic_cast< OptimizerPointer >( 
                       registration->GetOptimizer() );

    if ( registration->GetCurrentLevel() == 0 )
      {
      optimizer->SetMaximumStepLength( 16.00 );  
      optimizer->SetMinimumStepLength(  2.50 );
      }
    else
      {
      optimizer->SetMaximumStepLength( 
                optimizer->GetCurrentStepLength() );

      optimizer->SetMinimumStepLength(
                optimizer->GetMinimumStepLength() / 10.0 );
      }

  }
  void Execute(const itk::Object * object, const itk::EventObject & event)
    { return; }

};






int main( int argc, char **argv )
{


  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile"     << std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 2;
  typedef  unsigned short  PixelType;
  
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef   float     InternalPixelType;

  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;



  //  Software Guide : BeginLatex
  //  
  //  The configuration of the registration method in this example follows
  //  closely the one of the previous section. The main changes involve the
  //  construction and initialization of the transform. The instantiation of the
  //  transform type requires only the dimension of the space and the type used
  //  for representing space coordinates.
  //  
  //  \index{itk::AffineTransform!Instantiation}
  //
  //  Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  typedef itk::AffineTransform< double, Dimension > TransformType;
  // Software Guide : EndCodeSnippet



  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  typedef itk::LinearInterpolateImageFunction< 
                                    InternalImageType,
                                    double             > InterpolatorType;
  typedef itk::MattesMutualInformationImageToImageMetric< 
                                          InternalImageType, 
                                          InternalImageType >    MetricType;

  typedef OptimizerType::ScalesType       OptimizerScalesType;


  typedef itk::MultiResolutionImageRegistrationMethod< 
                                    InternalImageType, 
                                    InternalImageType    > RegistrationType;

  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    InternalImageType,
                                    InternalImageType  >    FixedImagePyramidType;

 
  typedef itk::RecursiveMultiResolutionPyramidImageFilter<
                                    InternalImageType,
                                    InternalImageType  >   MovingImagePyramidType;


  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  MetricType::Pointer         metric        = MetricType::New();

  registration->SetOptimizer(     optimizer     );
  registration->SetInterpolator(  interpolator  );
  registration->SetMetric( metric  );
  



  //  Software Guide : BeginLatex
  //
  //  The transform is constructed using the standard \code{New()} method and
  //  assigning it to a \code{SmartPointer}.
  //
  //  \index{itk::AffineTransform!New()}
  //  \index{itk::AffineTransform!Pointer}
  //  \index{itk::MultiResolutionImageRegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer   transform  = TransformType::New();

  registration->SetTransform( transform );
  // Software Guide : EndCodeSnippet





  FixedImagePyramidType::Pointer fixedImagePyramid = 
      FixedImagePyramidType::New();

  MovingImagePyramidType::Pointer movingImagePyramid =
      MovingImagePyramidType::New();


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  typedef itk::CastImageFilter< 
                        FixedImageType, InternalImageType > FixedCastFilterType;
  
  typedef itk::CastImageFilter< 
                        MovingImageType, InternalImageType > MovingCastFilterType;

  FixedCastFilterType::Pointer fixedCaster   = FixedCastFilterType::New();

  MovingCastFilterType::Pointer movingCaster = MovingCastFilterType::New();

  fixedCaster->SetInput(  fixedImageReader->GetOutput() );
  movingCaster->SetInput( movingImageReader->GetOutput() );

  registration->SetFixedImage(    fixedCaster->GetOutput()    );
  registration->SetMovingImage(   movingCaster->GetOutput()   );


  fixedCaster->Update();

  registration->SetFixedImageRegion( 
       fixedCaster->GetOutput()->GetBufferedRegion() );
   



  //  Software Guide : BeginLatex
  //  
  //  One of the easiest ways of preparing a consistent set of parameters for
  //  the transform is to use the transform itself.  We can simplify the task
  //  of initialization by taking advantage of the additional convenience
  //  methods that most transforms have. In this case, we simply force the
  //  transform to be initialized as an identity transform. The method
  //  \code{SetIdentity()} is used to that end. Once the transform is
  //  initialized we can invoke its \code{GetParameters()} method to extract
  //  the array of parameters. Finally the array is passed to the registration
  //  method using its \code{SetInitialTransformParameters()} method.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  transform->SetIdentity();
  
  registration->SetInitialTransformParameters( 
                            transform->GetParameters() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The set of parameters in the AffineTransform have different dynamic
  //  ranges. Typically the parameters associated with the matrix have values
  //  around $[-1:1]$ although they are not restricted to this interval.
  //  Parameters associated with translations, on the other hand, tend to have
  //  much higher values, typically in the order of $10.0$ to $100.0$. This
  //  difference in dynamic range affects negatively the performance of
  //  gradient descent optimizers. ITK provides a mechanism to compensate for
  //  such differences in values among the parameters when they are passed to
  //  the optimizer. The mechanism consist of providing an array of scale
  //  factors to the optimizer. These factors renormalize the gradient
  //  components before they are used to compute the step of the optimizer at
  //  the current iteration. In our particular case, a common choice for the
  //  scale parameters is to set to $1.0$ all those associated with the matrix
  //  coefficients.  That is, the first $N \times N$ factors. Then, set the
  //  remaining scale factors to a small value. The following code sets up the
  //  scale coefficients.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  optimizerScales[0] = 1.0; // scale for M11
  optimizerScales[1] = 1.0; // scale for M12
  optimizerScales[2] = 1.0; // scale for M21
  optimizerScales[3] = 1.0; // scale for M22

  optimizerScales[4] = 1.0 / 1000000.0; // scale for translation on X
  optimizerScales[5] = 1.0 / 1000000.0; // scale for translation on Y
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  where the affine transform can be represented by the matrix $\bf{M}$ and
  //  the vector $\bf{T}$. The transformation of a point $\bf{P}$ into $\bf{P'}$ being expressed as
  //
  //  \begin{equation}
  //  \left[ 
  //  \begin{array}{c}
  //  {P'}_x  \\  
  //  {P'}_y  \\    
  //  \end{array} 
  //  \right]
  //  =
  //  \left[ 
  //  \begin{array}{cc}
  //  M_{11} & M_{12} \\ 
  //  M_{21} & M_{22} \\  
  //  \end{array}
  //  \right]
  //  \cdot 
  //  \left[ 
  //  \begin{array}{c}
  //  P_x  \\ 
  //  P_y  \\  
  //  \end{array} 
  //  \right]
  //  +
  //  \left[ 
  //  \begin{array}{c}
  //  T_x  \\ 
  //  T_y  \\  
  //  \end{array} 
  //  \right] 
  //  \end{equation}
  //
  //
  //  Software Guide : EndLatex 




  //  Software Guide : BeginLatex
  //  
  //  The array of scales is then passed to the optimizer using the
  //  \code{SetScales()} method.
  //
  //  \index{itk::Optimizer!SetScales()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->SetScales( optimizerScales );
  // Software Guide : EndCodeSnippet



  metric->SetNumberOfHistogramBins( 50 );
  metric->SetNumberOfSpatialSamples( 1000 );


  //  Software Guide : BeginLatex
  //  
  //  The step length has to be proportionate to the expected values of the
  //  parameters in the search space. Since the expected values of the matrix
  //  coefficients are around $1.0$ the initial step of the optimization should
  //  be a small number compared to $1.0$. As a guideline, it is useful to think
  //  of the matrix coefficients as combinations of $cos(\theta)$ and
  //  $sin(\theta)$, this leads to use values close to the expected rotation
  //  measured in radians. For example, a rotation of $1.0$ degree is about
  //  $0.017$ radians. Similar to the previous example, the maximum and minimum
  //  step length of the optimizer is set by the
  //  \code{RegistrationInterfaceCommand} when it is called at the beginning of
  //  registration at each multi-resolution level.   
  //
  //  Software Guide : EndLatex 


  optimizer->SetNumberOfIterations(    50   );

  //
  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  optimizer->AddObserver( itk::IterationEvent(), observer );



  //
  // Create the Command interface observer and register it with the optimizer.
  //
  typedef RegistrationInterfaceCommand<RegistrationType> CommandType;

  CommandType::Pointer command = CommandType::New();
  registration->AddObserver( itk::IterationEvent(), command );



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

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType finalParameters = registration->GetLastTransformParameters();
  
  double TranslationAlongX = finalParameters[4];
  double TranslationAlongY = finalParameters[5];
  
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
  //  before.  The registration converged after $5$ iterations in the first
  //  level, $7$ in the second level and $4$ in the third level. The final
  //  results when printed as an array of parameters appears as
  //
  //  \begin{verbatim}
  // [1.00164, 0.00147688, 0.00168372, 1.0027, 12.6296, 16.4768]
  //  \end{verbatim}
  //
  //  By reordering them as coefficient of matrix $M$ and vector $T$
  //  they can now be seen as
  //
  //  \begin{equation}
  //  M = 
  //  \left[ 
  //  \begin{array}{cc}
  //  1.00164 & 0.0014 \\ 
  //  0.00168 & 1.0027 \\  
  //  \end{array}
  //  \right]
  //  \mbox{ and }
  //  T =
  //  \left[ 
  //  \begin{array}{c}
  //  12.6296  \\ 
  //  16.4768  \\  
  //  \end{array} 
  //  \right] 
  //  \end{equation}
  //
  //  this last presentation of the  values makes easier to interpret the
  //  effect of the transform. The matrix $M$ is responsible for scaling,
  //  rotation and shearing while $T$ is responsible for translations.
  //  It can be seen that the translation values in this case match closely the
  //  true misaligment introduced in the moving image. 
  // 
  //  It is important to note that once the images are registered at a
  //  sub-pixel level, any further improvement of the registration relies
  //  heavily on the quality of the interpolator. It may be reasonable then to
  //  use a coarse and fast interpolator in the lower resolution levels and
  //  switch to a high quality but slow interpolator in the final resolution
  //  level.
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
  // \includegraphics[width=5cm]{MultiResImageRegistration2Output.eps}
  // \includegraphics[width=5cm]{MultiResImageRegistration2CheckerboardBefore.eps}
  // \includegraphics[width=5cm]{MultiResImageRegistration2CheckerboardAfter.eps}
  // \caption[Multi-Resolution Registration Input Images]{Mapped moving image
  // (left) and composition of fixed and moving images before (center) and
  // after (right) multi-resolution registration with AffineTransform.}
  // \label{fig:MultiResImageRegistration2Output}
  // \end{figure}
  //
  //  The result of resampling the moving image is presented in the left side
  //  of Figure \ref{fig:MultiResImageRegistration2Output}. The center and
  //  right parts of the figure present a checkerboard composite of the fixed
  //  and moving images before and after registration.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[height=6cm]{MultiResImageRegistration2TraceTranslations.eps}
  // \includegraphics[height=6cm]{MultiResImageRegistration2TraceMetric.eps}
  // \caption[Multi-Resolution Registration output plots]{Sequence of
  // translations and metric values at each iteration of the optimizer for
  // Multi-resolution with AffineTransform.}
  // \label{fig:MultiResImageRegistration2Trace}
  // \end{figure}
  //
  //  Figure \ref{fig:MultiResImageRegistration2Trace} (left) presents the
  //  sequence of translations followed by the optimizer as it searched the
  //  parameter space. The right side of the same figure shows the sequence of
  //  metric values computed as the optimizer searched the parameter space.
  //
  //
  //  Software Guide : EndLatex 


  return 0;

}

