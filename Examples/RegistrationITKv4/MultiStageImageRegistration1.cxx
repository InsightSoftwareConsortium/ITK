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
//    OUTPUTS: {MultiStageImageRegistration1Output.png}
//    ARGUMENTS:    100
//    OUTPUTS: {MultiStageImageRegistration1CheckerboardBefore.png}
//    OUTPUTS: {MultiStageImageRegistration1CheckerboardAfter.png}
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
//  This example illustrates the use of more complex components of the
//  registration framework. In particular, it introduces a multistage,
//  multi-resolution approach to run a multi-modal registration process
//  using two linear \doxygen{TranslationTransform} and \doxygen{AffineTransform}.
//  Also, it shows the use of \emph{Scale Estimators}
//  for fine-tuning the scale parameters of the optimizer when an Affine
//  transform is used. The \doxygen{RegistrationParameterScalesFromPhysicalShift}
//  filter is used for automatic estimation of the parameters scales.
//
// \index{itk::ImageRegistrationMethodv4!AffineTransform}
// \index{itk::ImageRegistrationMethodv4!Scaling parameter space}
// \index{itk::AffineTransform!Image Registration}
// \index{itk::ImageRegistrationMethodv4!Multi-Stage}
// \index{itk::ImageRegistrationMethodv4!Multi-Resolution}
// \index{itk::ImageRegistrationMethodv4!Multi-Modality}
//
// To begin the example, we include the headers of the registration
// components we will use.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethodv4.h"

#include "itkMattesMutualInformationImageToImageMetricv4.h"

#include "itkRegularStepGradientDescentOptimizerv4.h"
#include "itkConjugateGradientLineSearchOptimizerv4.h"

#include "itkTranslationTransform.h"
#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

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
    OptimizerPointer optimizer = static_cast< OptimizerPointer >( object );
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
  //  In a multistage scenario, each stage needs an individual instantiation
  //  of the \doxygen{ImageRegistrationMethodv4}, so each stage can possibly
  //  have a different transform, a different optimizer, and a different image
  //  metric and can be performed in multiple levels.
  //  The configuration of the registration method at each stage closely
  //  follows the procedure in the previous section.
  //
  //  In early stages we can use simpler transforms and more aggressive optimization
  //  parameters to take big steps toward the optimal value. Then, at the final
  //  stage we can have a more complex transform to do fine adjustments of the final
  //  parameters.
  //
  //  A possible scheme is to use a simple translation transform for initial
  //  coarse registration levels and upgrade to an affine transform at the
  //  finer level.
  //  Since we have two different types of transforms, we can use a multistage
  //  registration approach as shown in the current example.
  //
  //  First we need to configure the registration components of the initial stage.
  //  The instantiation of the transform type requires only the
  //  dimension of the space and the type used for representing space coordinates.
  //
  //  \index{itk::TranslationTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension >      TTransformType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The types of other registration components are defined here.\newline
  //  \doxygen{RegularStepGradientDescentOptimizerv4} is used as the
  //  optimizer of the first stage. Also, we use
  //  \doxygen{MattesMutualInformationImageToImageMetricv4} as the metric
  //  since it is fitted for a multi-modal registration.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegularStepGradientDescentOptimizerv4< double > TOptimizerType;
  typedef itk::MattesMutualInformationImageToImageMetricv4<
    FixedImageType,
    MovingImageType > MetricType;
  typedef itk::ImageRegistrationMethodv4<
    FixedImageType,
    MovingImageType,
    TTransformType >  TRegistrationType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Then, all the components are instantiated using their \code{New()} method
  //  and connected to the registration object as in previous examples.
  //
  //  Software Guide : EndLatex

  TOptimizerType::Pointer      transOptimizer     = TOptimizerType::New();
  MetricType::Pointer          transMetric        = MetricType::New();
  TRegistrationType::Pointer   transRegistration  = TRegistrationType::New();

  transRegistration->SetOptimizer(     transOptimizer     );
  transRegistration->SetMetric( transMetric  );

  //  Software Guide : BeginLatex
  //
  //  The output transform of the registration process will be constructed
  //  internally in the registration filter since the related \emph{TransformType}
  //  is already passed to the registration method as a template parameter.
  //  However, we should provide an initial moving transform for the
  //  registration method if needed.
  //
  //  \index{itk::TranslationTransform!New()}
  //  \index{itk::TranslationTransform!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  TTransformType::Pointer   movingInitTx  = TTransformType::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  After setting the initial parameters, the initial transform can be
  //  passed to the registration filter by \code{SetMovingInitialTransform()} method.
  //
  //  \index{itk::Image\-Registration\-Methodv4!SetMovingInitialTransform()}
  //
  //  Software Guide : EndLatex

  typedef TOptimizerType::ParametersType          ParametersType;
  ParametersType initialParameters( movingInitTx->GetNumberOfParameters() );

  initialParameters[0] = 3.0;  // Initial offset in mm along X
  initialParameters[1] = 5.0;  // Initial offset in mm along Y

  movingInitTx->SetParameters( initialParameters );

  // Software Guide : BeginCodeSnippet
  transRegistration->SetMovingInitialTransform( movingInitTx );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We can use a \doxygen{CompositeTransform} to stack all the output
  //  transforms resulted from multiple stages. This composite
  //  transform should also hold the moving initial transform (if it exists)
  //  because as explained in section \ref{sec:RigidRegistrationIn2D},
  //  the output of each registration stage does not include the input
  //  initial transform to that stage.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CompositeTransform< double,
                                   Dimension >  CompositeTransformType;
  CompositeTransformType::Pointer  compositeTransform  =
                                          CompositeTransformType::New();
  compositeTransform->AddTransform( movingInitTx );
  // Software Guide : EndCodeSnippet

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
  //  In the case of this simple example, the first stage is run only
  //  in one level of registration at a coarse resolution.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet

  transMetric->SetNumberOfHistogramBins( 24 );

  if( argc > 7 )
    {
    // optionally, override the values with numbers taken from the command line arguments.
    transMetric->SetNumberOfHistogramBins( atoi( argv[7] ) );
    }

  transOptimizer->SetNumberOfIterations( 200 );
  transOptimizer->SetRelaxationFactor( 0.5 );

  //  Software Guide : BeginLatex
  //
  //  Also, for this initial stage we can use a more agressive parameter
  //  set for the optimizer by taking a big step size and relaxing
  //  stop criteria.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  transOptimizer->SetLearningRate( 16 );
  transOptimizer->SetMinimumStepLength( 1.5 );
  // Software Guide : EndCodeSnippet

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer1 = CommandIterationUpdate::New();
  transOptimizer->AddObserver( itk::IterationEvent(), observer1 );

  // Create the Registration interface observer and register it with the registration
  // method.
  //
  typedef RegistrationInterfaceCommand<TRegistrationType> TranslationCommandType;
  TranslationCommandType::Pointer command1 = TranslationCommandType::New();
  transRegistration->AddObserver( itk::MultiResolutionIterationEvent(), command1 );

  //  Software Guide : BeginLatex
  //
  //  Once all the registration components are in place, we trigger the registration
  //  process by calling \code{Update()} and add the result output transform to the final
  //  composite transform, so this composite transform can be used to initialize the next
  //  registration stage.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    transRegistration->Update();
    std::cout << "Optimizer stop condition: "
      << transRegistration->GetOptimizer()->GetStopConditionDescription()
      << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  compositeTransform->AddTransform(
    transRegistration->GetModifiableTransform() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now we can upgrade to an Affine transform as the second stage of registration
  //  process.
  //  The AffineTransform is a linear transformation that maps lines into
  //  lines. It can be used to represent translations, rotations, anisotropic
  //  scaling, shearing or any combination of them. Details about the affine
  //  transform can be seen in Section~\ref{sec:AffineTransform}.
  //  The instantiation of the transform type requires only the dimension of the
  //  space and the type used for representing space coordinates.
  //
  //  \index{itk::AffineTransform!Instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::AffineTransform< double, Dimension >       ATransformType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  We also use a different optimizer in configuration of the second stage while
  //  the metric is kept the same as before.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ConjugateGradientLineSearchOptimizerv4Template<
    double >         AOptimizerType;
  typedef itk::ImageRegistrationMethodv4<
    FixedImageType,
    MovingImageType,
    ATransformType > ARegistrationType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  //  Again all the components are instantiated using their \code{New()} method
  //  and connected to the registration object like in previous stages.
  //
  // Software Guide : EndLatex

  AOptimizerType::Pointer      affineOptimizer     = AOptimizerType::New();
  MetricType::Pointer          affineMetric        = MetricType::New();
  ARegistrationType::Pointer   affineRegistration  = ARegistrationType::New();

  affineRegistration->SetOptimizer(     affineOptimizer     );
  affineRegistration->SetMetric( affineMetric  );

  // Software Guide : BeginLatex
  //
  //  The current stage can be initialized using the initial transform of the
  //  registration and the result transform of the previous stage, so that both
  //  are concatenated into the composite transform.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  affineRegistration->SetMovingInitialTransform(  compositeTransform  );
  // Software Guide : EndCodeSnippet

  affineRegistration->SetFixedImage( fixedImageReader->GetOutput() );
  affineRegistration->SetMovingImage( movingImageReader->GetOutput() );
  affineRegistration->SetObjectName("AffineRegistration");

  affineMetric->SetNumberOfHistogramBins( 24 );

  if( argc > 7 )
    {
    // optionally, override the values with numbers taken from the command line arguments.
    affineMetric->SetNumberOfHistogramBins( atoi( argv[7] ) );
    }


  // Software Guide : BeginLatex
  //
  //  In Section \ref{sec:InitializingRegistrationWithMoments} we showed
  //  the importance of center of rotation in the registration process.
  //  In Affine transforms, the center of rotation is defined by the fixed
  //  parameters set, which are set by default to [0, 0].
  //  However, consider a situation where the
  //  origin of the virtual space, in which the registration is run, is far away
  //  from the zero origin. In such cases, leaving the center of rotation
  //  as the default value can make the optimization process unstable. Therefore,
  //  we are always interested to set the center of rotation to the center of virtual
  //  space which is usually the fixed image space.
  //
  //  Note that either center of gravity or geometrical center can be used
  //  as the center of rotation. In this example center of rotation is set
  //  to the geometrical center of the fixed image. We could also use
  //  \doxygen{ImageMomentsCalculator} filter to compute the center of mass.
  //
  //  Based on the above discussion, the user must set the fixed parameters of
  //  the registration transform outside of the registraton method, so first
  //  we instantiate an object of the output transform type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ATransformType::Pointer affineTx = ATransformType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  //  Then, we compute the physical center of the fixed image and set
  //  that as the center of the output Affine transform.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef FixedImageType::SpacingType    SpacingType;
  typedef FixedImageType::PointType      OriginType;
  typedef FixedImageType::RegionType     RegionType;
  typedef FixedImageType::SizeType       SizeType;

  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  const SpacingType fixedSpacing = fixedImage->GetSpacing();
  const OriginType  fixedOrigin  = fixedImage->GetOrigin();
  const RegionType  fixedRegion  = fixedImage->GetLargestPossibleRegion();
  const SizeType    fixedSize    = fixedRegion.GetSize();

  ATransformType::InputPointType centerFixed;
  centerFixed[0] =
    fixedOrigin[0] + fixedSpacing[0] * fixedSize[0] / 2.0;
  centerFixed[1] =
    fixedOrigin[1] + fixedSpacing[1] * fixedSize[1] / 2.0;

  const unsigned int numberOfFixedParameters =
    affineTx->GetFixedParameters().Size();
  ATransformType::ParametersType fixedParameters( numberOfFixedParameters );
  for (unsigned int i = 0; i < numberOfFixedParameters; ++i)
    {
    fixedParameters[i] = centerFixed[i];
    }
  affineTx->SetFixedParameters( fixedParameters );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  //  Then, the initialized output transform should be connected to
  //  the registration object by using \code{SetInitialTransform()} method.
  //
  //  It is important to distinguish between the \code{SetInitialTransform()}
  //  and \code{SetMovingInitialTransform()} that was used to initialize the
  //  registration stage based on the results of the previous stages.
  //  You can assume that the first one is used for direct manipulation of the
  //  optimizable transform in current registration process.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  affineRegistration->SetInitialTransform(  affineTx  );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The set of optimizable parameters in the Affine transform have different
  //  dynamic ranges. Typically the parameters associated with the matrix
  //  have values around $[-1:1]$, although they are not restricted to this
  //  interval.  Parameters associated with translations, on the other hand,
  //  tend to have much higher values, typically on the order of $10.0$ to
  //  $100.0$. This difference in dynamic range negatively affects the
  //  performance of gradient descent optimizers. ITK provides some mechanisms to
  //  compensate for such differences in values among the parameters when
  //  they are passed to the optimizer.
  //
  //  The first mechanism consists of providing an
  //  array of scale factors to the optimizer. These factors re-normalize the
  //  gradient components before they are used to compute the step of the
  //  optimizer at the current iteration.
  //  These scales are estimated by the user intuitively as shown in previous
  //  examples of this chapter. In our particular case, a common choice
  //  for the scale parameters is to set all those associated
  //  with the matrix coefficients to $1.0$, that is, the first $N \times N$
  //  factors. Then, we set the remaining scale factors to a small value.
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  //  Here the affine transform is represented by the matrix $\bf{M}$ and the
  //  vector $\bf{T}$. The transformation of a point $\bf{P}$ into $\bf{P'}$
  //  is expressed as
  //
  //  \begin{equation}
  //  \left[
  //  \begin{array}{c}
  //  {P'}_x  \\  {P'}_y  \\  \end{array}
  //  \right]
  //  =
  //  \left[
  //  \begin{array}{cc}
  //  M_{11} & M_{12} \\ M_{21} & M_{22} \\  \end{array}
  //  \right]
  //  \cdot
  //  \left[
  //  \begin{array}{c}
  //  P_x  \\ P_y  \\  \end{array}
  //  \right]
  //  +
  //  \left[
  //  \begin{array}{c}
  //  T_x  \\ T_y  \\  \end{array}
  //  \right]
  //  \end{equation}
  //
  //
  //  Software Guide : EndLatex

  //  Software Guide : BeginLatex
  //
  //  Based on the above discussion, we need much smaller scales for translation
  //  parameters of vector $\bf{T}$ ($T_x$, $T_y$) compared to the parameters
  //  of matrix $\bf{M}$ ($M_{11}$, $M_{12}$, $M_{21}$, $M_{22}$).
  //  However, it is not easy to have an intuitive estimation of all parameter
  //  scales when we have to deal with a large parameter space.
  //
  //  Fortunately, ITKv4 provides a framework for automated parameter scaling.
  //  \doxygen{RegistrationParameterScalesEstimator} vastly reduces the
  //  difficulty of tuning parameters for different transform/metric combinations.
  //  Parameter scales are estimated by analyzing the result of a small parameter
  //  update on the change in the magnitude of physical space deformation induced
  //  by the transformation.
  //
  //  The impact from a unit change of a parameter may be defined in multiple ways,
  //  such as the maximum shift of voxels in index or physical space, or the average
  //  norm of transform Jacobian.
  //  Filters \doxygen{RegistrationParameterScalesFromPhysicalShift}
  //  and \doxygen{RegistrationParameterScalesFromIndexShift} use the first definition
  //  to estimate the scales, while the \doxygen{RegistrationParameterScalesFromJacobian}
  //  filter estimates scales based on the later definition.
  //  In all methods, the goal is to rescale the transform parameters such that
  //  a unit change of each \emph{scaled parameter} will have the same impact on deformation.
  //
  //  In this example the first filter is chosen to estimate the parameter scales. The
  //  scales estimator will then be passed to optimizer.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::RegistrationParameterScalesFromPhysicalShift<
    MetricType> ScalesEstimatorType;
  ScalesEstimatorType::Pointer scalesEstimator =
    ScalesEstimatorType::New();
  scalesEstimator->SetMetric( affineMetric );
  scalesEstimator->SetTransformForward( true );

  affineOptimizer->SetScalesEstimator( scalesEstimator );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The step length has to be proportional to the expected values of the
  //  parameters in the search space. Since the expected values of the matrix
  //  coefficients are around $1.0$, the initial step of the optimization
  //  should be a small number compared to $1.0$. As a guideline, it is
  //  useful to think of the matrix coefficients as combinations of
  //  $cos(\theta)$ and $sin(\theta)$.  This leads to use values close to the
  //  expected rotation measured in radians. For example, a rotation of $1.0$
  //  degree is about $0.017$ radians.
  //
  //  However, we need not worry about the above considerations.
  //  Thanks to the \emph{ScalesEstimator}, the initial step size can also be
  //  estimated automatically, either at each iteration or only at the first
  //  iteration. In this example we choose to estimate learning rate
  //  once at the begining of the registration process.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  affineOptimizer->SetDoEstimateLearningRateOnce( true );
  affineOptimizer->SetDoEstimateLearningRateAtEachIteration( false );
  // Software Guide : EndCodeSnippet

  // Set the other parameters of optimizer
  //
  affineOptimizer->SetLowerLimit( 0 );
  affineOptimizer->SetUpperLimit( 2 );
  affineOptimizer->SetEpsilon( 0.2 );
  affineOptimizer->SetNumberOfIterations( 200 );
  affineOptimizer->SetMinimumConvergenceValue( 1e-6 );
  affineOptimizer->SetConvergenceWindowSize( 5 );

  // Create the Command observer and register it with the optimizer.
  //
  CommandIterationUpdate::Pointer observer2 = CommandIterationUpdate::New();
  affineOptimizer->AddObserver( itk::IterationEvent(), observer2 );

  //  Software Guide : BeginLatex
  //
  //  At the second stage, we run two levels of registration, where the second
  //  level is run in full resolution in which we do the final adjustments
  //  of the output parameters.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet

  // Create the Registration interface observer and register it with the registration
  // object.
  //
  typedef RegistrationInterfaceCommand<ARegistrationType> AffineCommandType;
  AffineCommandType::Pointer command2 = AffineCommandType::New();
  affineRegistration->AddObserver( itk::MultiResolutionIterationEvent(), command2 );

  //  Software Guide : BeginLatex
  //
  //  Finally we trigger the registration process by calling \code{Update()} and
  //  add the output transform of the last stage to the
  //  composite transform. This composite transform will be considered as
  //  the final transform of this multistage registration process and will be
  //  used by the resampler to resample the moving image in to the virtual domain
  //  space (fixed image space if there is no fixed initial transform).
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    affineRegistration->Update();
    std::cout << "Optimizer stop condition: "
      << affineRegistration->GetOptimizer()->GetStopConditionDescription()
      << std::endl;
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  compositeTransform->AddTransform(
    affineRegistration->GetModifiableTransform() );
  // Software Guide : EndCodeSnippet

  std::cout << "\nInitial parameters of the registration process: " << std::endl
            << movingInitTx->GetParameters() << std::endl;

  std::cout << "\nTranslation parameters after registration: " << std::endl
            << transOptimizer->GetCurrentPosition() << std::endl
            << " Last LearningRate: " << transOptimizer->GetCurrentStepLength() << std::endl;

  std::cout << "\nAffine parameters after registration: " << std::endl
            << affineOptimizer->GetCurrentPosition() << std::endl
            << " Last LearningRate: " << affineOptimizer->GetLearningRate() << std::endl;

  //  Software Guide : BeginLatex
  //
  //  Let's execute this example using the following multi-modality images:
  //
  //  \begin{itemize}
  //  \item BrainT1SliceBorder20.png
  //  \item BrainProtonDensitySliceR10X13Y17.png
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first
  //  image by $10$ degrees and then translating by $(-13,-17)$.  Both images
  //  have unit-spacing and are shown in Figure
  //  \ref{fig:FixedMovingMultiStageImageRegistration1}.
  //
  //  The registration converges after $5$ iterations in the translation stage.
  //  Also, in the second stage, the registration converges after $46$ iterations
  //  in the first level, and $6$ iterations in the second level.
  //  The final results when printed as an array of parameters are:
  //
  //  \begin{verbatim}
  //  Initial parameters of the registration process:
  //  [3, 5]
  //
  //  Translation parameters after first registration stage:
  //  [9.0346, 10.8303]
  //
  //  Affine parameters after second registration stage:
  //  [0.9864, -0.1733, 0.1738, 0.9863, 0.9693, 0.1482]
  //  \end{verbatim}
  //
  //  As it can be seen, the translation parameters after the first stage
  //  compensate most of the offset between the fixed and moving images.
  //  When the images are close to each other, the affine registration is
  //  run for the rotation and the final match.
  //  By reordering the Affine array of parameters as coefficients of matrix
  //  $\bf{M}$ and vector $\bf{T}$ they can now be seen as
  //
  //  \begin{equation}
  //  M =
  //  \left[
  //  \begin{array}{cc}
  //  0.9864 & -0.1733 \\ 0.1738 & 0.9863 \\  \end{array}
  //  \right]
  //  \mbox{ and }
  //  T =
  //  \left[
  //  \begin{array}{c}
  //  0.9693  \\  0.1482  \\  \end{array}
  //  \right]
  //  \end{equation}
  //
  //  In this form, it is easier to interpret the effect of the
  //  transform. The matrix $\bf{M}$ is responsible for scaling, rotation and
  //  shearing while $\bf{T}$ is responsible for translations.
  //
  //  The second component of the matrix values is usually associated with
  //  $\sin{\theta}$. We obtain the rotation through SVD of the affine
  //  matrix. The value is $9.975$ degrees, which is approximately the
  //  intentional misalignment of $10.0$ degrees.
  //
  //  Also, let's compute the total translation values resulting from initial transform,
  //  translation transform, and the Affine transform together.
  //
  //  In $X$ direction:
  //  \begin{equation}
  //  3 + 9.0346 + 0.9693 = 13.0036
  //  \end{equation}
  //  In $Y$ direction:
  //  \begin{equation}
  //  5 + 10.8303 + 0.1482 = 15.9785
  //  \end{equation}
  //
  //  It can be seen that the translation values closely match the true
  //  misalignment introduced in the moving image.
  //
  //  It is important to note that once the images are registered at a
  //  sub-pixel level, any further improvement of the registration relies
  //  heavily on the quality of the interpolator. It may then be reasonable to
  //  use a coarse and fast interpolator in the lower resolution levels and
  //  switch to a high-quality but slow interpolator in the final resolution
  //  level. However, in this example we used a linear interpolator for all
  //  stages and different registration levels since it is so fast.
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17}
  // \itkcaption[AffineTransform registration]{Fixed and moving images
  // provided as input to the registration method using the AffineTransform.}
  // \label{fig:FixedMovingMultiStageImageRegistration1}
  // \end{figure}
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
  // \includegraphics[width=0.32\textwidth]{MultiStageImageRegistration1Output}
  // \includegraphics[width=0.32\textwidth]{MultiStageImageRegistration1CheckerboardBefore}
  // \includegraphics[width=0.32\textwidth]{MultiStageImageRegistration1CheckerboardAfter}
  // \itkcaption[Multistage registration input images]{Mapped moving image
  // (left) and composition of fixed and moving images before (center) and
  // after (right) registration.}
  // \label{fig:MultiStageImageRegistration1Outputs}
  // \end{figure}
  //
  //  The result of resampling the moving image is presented in the left image
  //  of Figure \ref{fig:MultiStageImageRegistration1Outputs}. The center and
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
