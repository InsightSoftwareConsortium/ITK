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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainT1SliceBorder20.png}
//    INPUTS:  {BrainProtonDensitySliceR10X13Y17.png}
//    OUTPUTS: {MultiStageImageRegistration2Output.png}
//    ARGUMENTS:    100
//    OUTPUTS: {MultiStageImageRegistration2CheckerboardBefore.png}
//    OUTPUTS: {MultiStageImageRegistration2CheckerboardAfter.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
//  This examples shows how different stages can be cascaded together directly
//  in a multistage registration process. The example code is, for the most
//  part, identical to the previous multistage example. The main difference
//  is that no initial transform is used, and the output of the first stage
//  is directly linked to the second stage, and the whole registration process
//  is triggered only once by calling \code{Update()} after the last stage stage.
//
//  We will focus on the most relevent changes in current code and skip all the
//  similar parts already explained in the previous example.
//
// \index{itk::ImageRegistrationMethodv4!Multi-Stage}
//
// Software Guide : EndLatex

#include "itkImageRegistrationMethodv4.h"

#include "itkMattesMutualInformationImageToImageMetricv4.h"

#include "itkRegularStepGradientDescentOptimizerv4.h"
#include "itkConjugateGradientLineSearchOptimizerv4.h"

#include "itkTranslationTransform.h"
#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImageMomentsCalculator.h"
#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkCheckerBoardImageFilter.h"

#include "itkCommand.h"

//  The following section of code implements a Command observer
//  that will monitor the configurations of the registration process
//  at every change of stage and resolution level.
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
  typedef   TRegistration                          RegistrationType;

  // The Execute function simply calls another version of the \code{Execute()}
  // method accepting a \code{const} input object
  void Execute( itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *) object , event );
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    if( !(itk::MultiResolutionIterationEvent().CheckEvent( &event ) ) )
      {
      return;
      }

    std::cout << "\nObserving from class " << object->GetNameOfClass();
    if (!object->GetObjectName().empty())
      {
      std::cout << " \"" << object->GetObjectName() << "\"" << std::endl;
      }

    const RegistrationType * registration = static_cast<const RegistrationType *>( object );
    if(registration == 0)
      {
      itkExceptionMacro(<< "Dynamic cast failed, object of type " << object->GetNameOfClass());
      }

    unsigned int currentLevel = registration->GetCurrentLevel();
    typename RegistrationType::ShrinkFactorsPerDimensionContainerType shrinkFactors =
                                              registration->GetShrinkFactorsPerDimension( currentLevel );
    typename RegistrationType::SmoothingSigmasArrayType smoothingSigmas =
                                                            registration->GetSmoothingSigmasPerLevel();

    std::cout << "-------------------------------------" << std::endl;
    std::cout << " Current multi-resolution level = " << currentLevel << std::endl;
    std::cout << "    shrink factor = " << shrinkFactors << std::endl;
    std::cout << "    smoothing sigma = " << smoothingSigmas[currentLevel] << std::endl;
    std::cout << std::endl;
    }
};

//  The following section of code implements an observer
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
  CommandIterationUpdate(): m_CumulativeIterationIndex(0) {};

public:
  typedef   itk::GradientDescentOptimizerv4Template<double>  OptimizerType;
  typedef   const OptimizerType *                            OptimizerPointer;

  void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
    {
    OptimizerPointer optimizer =  static_cast< OptimizerPointer >( object );
    if( optimizer == ITK_NULLPTR)
      {
      return; // in this unlikely context, just do nothing.
      }
    if( !(itk::IterationEvent().CheckEvent( &event )) )
      {
      return;
      }
    std::cout << optimizer->GetCurrentIteration() << "   ";
    std::cout << optimizer->GetValue() << "   ";
    std::cout << optimizer->GetCurrentPosition() << "  " <<
      m_CumulativeIterationIndex++ << std::endl;
    }

private:
  unsigned int m_CumulativeIterationIndex;
};

int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile [backgroundGrayLevel]";
    std::cerr << " [checkerboardbefore] [CheckerBoardAfter]";
    std::cerr << " [numberOfBins] " << std::endl;
    return EXIT_FAILURE;
    }

  const    unsigned int    Dimension = 2;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  //  Software Guide : BeginLatex
  //
  //  Let's start by defining different types of the first stage.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension >     TTransformType;
  typedef itk::RegularStepGradientDescentOptimizerv4<double> TOptimizerType;
  typedef itk::MattesMutualInformationImageToImageMetricv4<
    FixedImageType,
    MovingImageType >  MetricType;
  typedef itk::ImageRegistrationMethodv4<
    FixedImageType,
    MovingImageType >  TRegistrationType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Type definitions are the same as previous example with an important subtle
  //  change: the transform type is not passed to the
  //  registration method as a template parameter anymore. In this case, the
  //  registration filter will consider the transform base class
  //  \doxygen{Transform} as the type of its output transform.
  //
  //  Software Guide : EndLatex

  //  All the components are instantiated using their \code{New()} method
  //  and connected to the registration object as in previous example.
  //
  TOptimizerType::Pointer      transOptimizer     = TOptimizerType::New();
  MetricType::Pointer         transMetric         = MetricType::New();
  TRegistrationType::Pointer   transRegistration  = TRegistrationType::New();

  transRegistration->SetOptimizer(     transOptimizer     );
  transRegistration->SetMetric( transMetric  );

  //  Software Guide : BeginLatex
  //
  //  Instead of passing the transform type, we create an explicit instantiation
  //  of the transform object outside of the registration filter, and connect
  //  that to the registration object using the \code{SetInitialTransform()} method.
  //  Also, by calling \code{InPlaceOn()} method, this transform object will be
  //  the output transform of the registration filter or will be grafted to the
  //  output.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TTransformType::Pointer translationTx = TTransformType::New();

  transRegistration->SetInitialTransform( translationTx );
  transRegistration->InPlaceOn();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Also, there is no initial transform defined for this example.
  //
  //  Software Guide : EndLatex

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );

  transRegistration->SetFixedImage(    fixedImageReader->GetOutput()    );
  transRegistration->SetMovingImage(   movingImageReader->GetOutput()   );
  transRegistration->SetObjectName("TranslationRegistration");

  //  Software Guide : BeginLatex
  //
  //  As in the previous example, the first stage is run using only one level of
  //  registration at a coarse resolution level. However, notice that we
  //  do not need to update the translation registration filter at this
  //  step since the output of this stage will be directly connected to the
  //  initial input of the next stage. Due to ITK's pipeline structure,
  //  when we call the \code{Update()} at the last stage, the first stage
  //  will be updated as well.
  //
  //  Software Guide : EndLatex

  const unsigned int numberOfLevels1 = 1;

  TRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel1;
  shrinkFactorsPerLevel1.SetSize( numberOfLevels1 );
  shrinkFactorsPerLevel1[0] = 3;

  TRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel1;
  smoothingSigmasPerLevel1.SetSize( numberOfLevels1 );
  smoothingSigmasPerLevel1[0] = 2;

  transRegistration->SetNumberOfLevels ( numberOfLevels1 );
  transRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel1 );
  transRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel1 );

  transMetric->SetNumberOfHistogramBins( 24 );

  if( argc > 7 )
    {
    // optionally, override the values with numbers taken from the command line arguments.
    transMetric->SetNumberOfHistogramBins( atoi( argv[7] ) );
    }

  transOptimizer->SetNumberOfIterations( 200 );
  transOptimizer->SetRelaxationFactor( 0.5 );
  transOptimizer->SetLearningRate( 16 );
  transOptimizer->SetMinimumStepLength( 1.5 );

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer1 = CommandIterationUpdate::New();
  transOptimizer->AddObserver( itk::IterationEvent(), observer1 );

  // Create the Command interface observer and register it with the optimizer.
  //
  typedef RegistrationInterfaceCommand<TRegistrationType> TranslationCommandType;
  TranslationCommandType::Pointer command1 = TranslationCommandType::New();
  transRegistration->AddObserver( itk::MultiResolutionIterationEvent(), command1 );

  //  Software Guide : BeginLatex
  //
  //  Now we upgrade to an Affine transform as the second stage of registration
  //  process,
  //  and as before, we initially define and instantiate different components of the
  //  current registration stage. We have used a new optimizer but the same
  //  metric in new configurations.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::AffineTransform< double, Dimension > ATransformType;
  typedef itk::ConjugateGradientLineSearchOptimizerv4Template<
    double >  AOptimizerType;
  typedef itk::ImageRegistrationMethodv4<
    FixedImageType,
    MovingImageType > ARegistrationType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Again notice that \emph{TransformType} is not passed to the type
  //  definition of the registration filter. It is important because when the
  //  registration filter considers transform base class \doxygen{Transform}
  //  as the type of its output transform, it prevents the type mismatch when
  //  the two stages are cascaded to each other.
  //
  //  Then, all components are instantiated using their \code{New()} method
  //  and connected to the registration object among the transform type.
  //  Despite the previous example, here we use the fixed image's center of mass
  //  to initialize the fixed parameters of the Affine transform.
  //  \doxygen{ImageMomentsCalculator} filter is used for this purpose.
  //
  //  Software Guide : EndLatex

  AOptimizerType::Pointer      affineOptimizer     = AOptimizerType::New();
  MetricType::Pointer          affineMetric        = MetricType::New();
  ARegistrationType::Pointer   affineRegistration  = ARegistrationType::New();

  affineRegistration->SetOptimizer(     affineOptimizer     );
  affineRegistration->SetMetric( affineMetric  );

  affineMetric->SetNumberOfHistogramBins( 24 );
  if( argc > 7 )
    {
     // optionally, override the values with numbers taken from the command line arguments.
    affineMetric->SetNumberOfHistogramBins( atoi( argv[7] ) );
    }

  fixedImageReader->Update();
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageMomentsCalculator<
    FixedImageType > FixedImageCalculatorType;

  FixedImageCalculatorType::Pointer fixedCalculator =
    FixedImageCalculatorType::New();
  fixedCalculator->SetImage( fixedImage );
  fixedCalculator->Compute();

  FixedImageCalculatorType::VectorType fixedCenter =
    fixedCalculator->GetCenterOfGravity();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // Then, we initialize the fixed parameters (center of rotation) in the Affine
  // transform and connect that to the registration object.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ATransformType::Pointer   affineTx  = ATransformType::New();

  const unsigned int numberOfFixedParameters =
                                      affineTx->GetFixedParameters().Size();
  ATransformType::ParametersType fixedParameters( numberOfFixedParameters );
  for (unsigned int i = 0; i < numberOfFixedParameters; ++i)
     {
     fixedParameters[i] = fixedCenter[i];
     }
  affineTx->SetFixedParameters( fixedParameters );

  affineRegistration->SetInitialTransform(  affineTx  );
  affineRegistration->InPlaceOn();
  // Software Guide : EndCodeSnippet

  affineRegistration->SetFixedImage( fixedImageReader->GetOutput() );
  affineRegistration->SetMovingImage( movingImageReader->GetOutput() );
  affineRegistration->SetObjectName("AffineRegistration");

  //  Software Guide : BeginLatex
  //
  //  Now, the output of the first stage is wrapped through a
  //  \doxygen{DataObjectDecorator} and is passed to the input
  //  of the second stage as the moving initial transform via
  //  \code{SetMovingInitialTransformInput()} method. Note that
  //  this API has an ``Input'' word attached to the name of another
  //  initialization method \code{SetMovingInitialTransform()}
  //  that already has been used in previous example.
  //  This extension means that the following API expects
  //  a data object decorator type.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  affineRegistration->SetMovingInitialTransformInput(
    transRegistration->GetTransformOutput() );
  // Software Guide : EndCodeSnippet


  typedef itk::RegistrationParameterScalesFromPhysicalShift<
    MetricType> ScalesEstimatorType;
  ScalesEstimatorType::Pointer scalesEstimator =
    ScalesEstimatorType::New();
  scalesEstimator->SetMetric( affineMetric );
  scalesEstimator->SetTransformForward( true );

  affineOptimizer->SetScalesEstimator( scalesEstimator );
  affineOptimizer->SetDoEstimateLearningRateOnce( true );
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration( false );
  affineOptimizer->SetLowerLimit( 0 );
  affineOptimizer->SetUpperLimit( 2 );
  affineOptimizer->SetEpsilon( 0.2 );
  affineOptimizer->SetNumberOfIterations( 200 );
  affineOptimizer->SetMinimumConvergenceValue( 1e-6 );
  affineOptimizer->SetConvergenceWindowSize( 10 );

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer2 = CommandIterationUpdate::New();
  affineOptimizer->AddObserver( itk::IterationEvent(), observer2 );

  //  Software Guide : BeginLatex
  //
  //  Second stage runs two levels of registration, where the second
  //  level is run in full resolution.
  //
  //  Software Guide : EndLatex

  const unsigned int numberOfLevels2 = 2;

  ARegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel2;
  shrinkFactorsPerLevel2.SetSize( numberOfLevels2 );
  shrinkFactorsPerLevel2[0] = 2;
  shrinkFactorsPerLevel2[1] = 1;

  ARegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel2;
  smoothingSigmasPerLevel2.SetSize( numberOfLevels2 );
  smoothingSigmasPerLevel2[0] = 1;
  smoothingSigmasPerLevel2[1] = 0;

  affineRegistration->SetNumberOfLevels ( numberOfLevels2 );
  affineRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel2 );
  affineRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel2 );

  // Create the Command interface observer and register it with the optimizer.
  //
  typedef RegistrationInterfaceCommand<ARegistrationType> AffineCommandType;
  AffineCommandType::Pointer command2 = AffineCommandType::New();
  affineRegistration->AddObserver( itk::MultiResolutionIterationEvent(), command2 );

  //  Software Guide : BeginLatex
  //
  //  Once all the registration components are in place,
  //  finally we trigger the whole registration process, including two cascaded
  //  registration stages, by calling \code{Update()} on the registration
  //  filter of the last stage, which causes both stages be updated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    affineRegistration->Update();
    std::cout << "Optimizer stop condition: "
              << affineRegistration->
                          GetOptimizer()->GetStopConditionDescription()
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
  //  Finally, a composite transform is used to concatenate the results of
  //  all stages together, which will be considered as the
  //  final output of this multistage process and will be passed to the
  //  resampler to resample the moving image into the virtual domain
  //  space (fixed image space if there is no fixed initial transform).
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CompositeTransform< double,
                                   Dimension >  CompositeTransformType;
  CompositeTransformType::Pointer   compositeTransform  =
                                        CompositeTransformType::New();
  compositeTransform->AddTransform( translationTx );
  compositeTransform->AddTransform( affineTx );
  // Software Guide : EndCodeSnippet

  std::cout << " Translation transform parameters after registration: " << std::endl
            << transOptimizer->GetCurrentPosition() << std::endl
            << " Last LearningRate: " << transOptimizer->GetCurrentStepLength() << std::endl;

  std::cout << " Affine transform parameters after registration: " << std::endl
            << affineOptimizer->GetCurrentPosition() << std::endl
            << " Last LearningRate: " << affineOptimizer->GetLearningRate() << std::endl;


  //  Software Guide : BeginLatex
  //
  //  Let's execute this example using the same multi-modality images as
  //  before. The registration converges after $6$ iterations in the first
  //  stage, also in $45$ and $11$ iterations corresponding to the first level
  //  and second level of the Affine stage.
  //  The final results when printed as an array of parameters are:
  //
  //  \begin{verbatim}
  //  Translation parameters after first registration stage:
  //  [11.600, 15.1814]
  //
  //  Affine parameters after second registration stage:
  //  [0.9860, -0.1742, 0.1751, 0.9862, 0.9219, 0.8023]
  //  \end{verbatim}
  //
  //  Let's reorder the Affine array of parameters again as coefficients of matrix
  //  $\bf{M}$ and vector $\bf{T}$. They can now be seen as
  //
  //  \begin{equation}
  //  M =
  //  \left[
  //  \begin{array}{cc}
  //  0.9860 & -0.1742 \\ 0.1751 & 0.9862 \\  \end{array}
  //  \right]
  //  \mbox{ and }
  //  T =
  //  \left[
  //  \begin{array}{c}
  //  0.9219  \\  0.8023  \\  \end{array}
  //  \right]
  //  \end{equation}
  //
  //  $10.02$ degrees is the rotation value computed from the affine matrix
  //  parameters, which approximately equals the intentional misalignment.
  //
  //  Also for the total translation value resulted from both transforms, we have:
  //
  //  In $X$ direction:
  //  \begin{equation}
  //  11.6004 + 0.9219 = 12.5223
  //  \end{equation}
  //  In $Y$ direction:
  //  \begin{equation}
  //  15.1814 + 0.8023 = 15.9837
  //  \end{equation}
  //
  //  These results closely match the true misalignment introduced in the moving image.
  //
  //  Software Guide : EndLatex

  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( compositeTransform );
  resample->SetInput( movingImageReader->GetOutput() );

  PixelType backgroundGrayLevel = 100;
  if( argc > 4 )
    {
    backgroundGrayLevel = atoi( argv[4] );
    }

  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( backgroundGrayLevel );

  typedef  unsigned char                           OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::CastImageFilter<
                        FixedImageType,
                        OutputImageType >          CastFilterType;
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
  // \includegraphics[width=0.32\textwidth]{MultiStageImageRegistration2Output}
  // \includegraphics[width=0.32\textwidth]{MultiStageImageRegistration2CheckerboardBefore}
  // \includegraphics[width=0.32\textwidth]{MultiStageImageRegistration2CheckerboardAfter}
  // \itkcaption[Multistage registration input images]{Mapped moving image
  // (left) and composition of fixed and moving images before (center) and
  // after (right) registration.}
  // \label{fig:MultiStageImageRegistration2Outputs}
  // \end{figure}
  //
  //  The result of resampling the moving image is presented in the left image
  //  of Figure \ref{fig:MultiStageImageRegistration2Outputs}. The center and
  //  right images of the figure depict a checkerboard composite of the fixed
  //  and moving images before and after registration.
  //
  //  Software Guide : EndLatex

  //
  // Generate checkerboards before and after registration
  //
  typedef itk::CheckerBoardImageFilter< FixedImageType > CheckerBoardFilterType;

  CheckerBoardFilterType::Pointer checker = CheckerBoardFilterType::New();

  checker->SetInput1( fixedImage );
  checker->SetInput2( resample->GetOutput() );

  caster->SetInput( checker->GetOutput() );
  writer->SetInput( caster->GetOutput()   );

  resample->SetDefaultPixelValue( 0 );

  // Write out checkerboard outputs
  // Before registration
  typedef itk::IdentityTransform< double, Dimension >   TransformType;
  TransformType::Pointer identityTransform;
  try
    {
    identityTransform = TransformType::New();
    }
  catch( itk::ExceptionObject & err )
    {
    err.Print(std::cerr);
    return EXIT_FAILURE;
    }
  identityTransform->SetIdentity();
  resample->SetTransform( identityTransform );

  if( argc > 5 )
    {
    writer->SetFileName( argv[5] );
    writer->Update();
    }

  // After registration
  resample->SetTransform( compositeTransform );
  if( argc > 6 )
    {
    writer->SetFileName( argv[6] );
    writer->Update();
    }

  return EXIT_SUCCESS;
}
