/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration8.cxx
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
// This example illustrates the use of the \doxygen{VersorRigid3DTransform}
// for performing registration. The code of this example is for the most part
// identical to the one presented in section~\ref{sec:RigidRegistrationIn2D}.
//
// The major difference is that this current example is done in $3D$. The class
// \doxygen{CenteredTransformInitializer} is used again here to initialize the
// center and translation of the transform.
//
// \index{itk::VersorRigid3DTransform}
// \index{itk::CenteredTransformInitializer!In 3D}
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
//  The following are the most relevant headers in this example.
//
//  \index{itk::CenteredRigid2DTransform!header}
// 
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVersorRigid3DTransform.h"
#include "itkCenteredTransformInitializer.h"
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
      std::cout << optimizer->GetCurrentPosition() << std::endl;
  }
};







int main( int argc, char *argv[] )
{


  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile ";
    std::cerr << " outputImagefile  [differenceOutputfile] ";
    std::cerr << " [differenceBeforeRegistration] "<< std::endl;
    return 1;
    }
  
  const    unsigned int    Dimension = 3;
  typedef  float           PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;


  //  Software Guide : BeginLatex
  //  
  //  The Transform type is instantiated using the code below. The only
  //  template parameter of this class is the representation type of the space
  //  coordinates.
  //
  //  \index{itk::CenteredRigid2DTransform!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::VersorRigid3DTransform< double > TransformType;
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
  //  The transform object is constructed below and passed to the registration
  //  method.
  //
  //  \index{itk::CenteredRigid2DTransform!New()}
  //  \index{itk::CenteredRigid2DTransform!Pointer}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer  transform = TransformType::New();

  registration->SetTransform( transform );
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
  //  The input images are taken from readers. It is not necessary here to
  //  explicitly call \code{Update()} on the readers since the
  //  \doxygen{CenteredTransformInitializer} will do it as part of its
  //  computations. The following code instantiates the type of the
  //  initializer. This class is templated over the fixed and moving image type
  //  as well as the transform type. An initializer is then constructed by
  //  calling the \code{New()} method and assigning the result to a smart
  //  pointer.
  //
  // \index{itk::CenteredRigid2DTransform!Instantiation}
  // \index{itk::CenteredRigid2DTransform!New()}
  // \index{itk::CenteredRigid2DTransform!SmartPointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CenteredTransformInitializer< 
                                    TransformType, 
                                    FixedImageType, 
                                    MovingImageType >  TransformInitializerType;

  TransformInitializerType::Pointer initializer = TransformInitializerType::New();
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //
  //  The initializer is now connected to the transform and to the fixed and
  //  moving images.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  initializer->SetTransform(   transform );

  initializer->SetFixedImage(  fixedImageReader->GetOutput() );
  initializer->SetMovingImage( movingImageReader->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The use of the geometrical centers is selected by calling
  //  \code{GeometryOn()} while the use of center of mass is selected by
  //  calling \code{MomentsOn()}.  Below we select the center of mass mode.
  //
  //  \index{CenteredTransformInitializer!MomentsOn()}
  //  \index{CenteredTransformInitializer!GeometryOn()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  initializer->MomentsOn();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  Finally, the computation of the center and translation is triggered by the
  //  \code{InitializeTransform()} method. The resulting values will be passed
  //  directly to the transform.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  initializer->InitializeTransform();
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  The rotation part of the transform is initialized using a
  //  \doxygen{Versor} which is simply a unit quaternion.  The
  //  \code{VersorType} can be obtained from the transform traits. The versor
  //  itself defines the type of the vector used to indicate the rotation axis.
  //  This trait can be extracted as \code{VectorType}. The following lines
  //  create a versor object and initialize its parameters by passing a
  //  rotation axis and an angle.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef TransformType::VersorType  VersorType;

  typedef VersorType::VectorType     VectorType;
  
  VersorType     rotation;

  VectorType     axis;
  
  axis[0] = 0.0;
  axis[1] = 0.0;
  axis[2] = 1.0;

  const double angle = 0;

  rotation.Set(  axis, angle  );

  transform->SetRotation( rotation );
  // Software Guide : EndCodeSnippet





  //  Software Guide : BeginLatex
  //  
  //  We pass now the parameters of the current transform as the initial
  //  parameters to be used when the registration process starts. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetInitialTransformParameters( 
                                 transform->GetParameters() );
  // Software Guide : EndCodeSnippet





  typedef OptimizerType::ScalesType       OptimizerScalesType;

  OptimizerScalesType optimizerScales( transform->GetNumberOfParameters() );

  const double translationScale = 1.0 / 1000.0;

  optimizerScales[0] = 1.0;
  optimizerScales[1] = 1.0;
  optimizerScales[2] = 1.0;
  optimizerScales[3] = translationScale;
  optimizerScales[4] = translationScale;
  optimizerScales[5] = translationScale;
  optimizerScales[6] = translationScale;
  optimizerScales[7] = translationScale;
  optimizerScales[8] = translationScale;

  optimizer->SetScales( optimizerScales );

  optimizer->SetMaximumStepLength( 0.1    ); 
  optimizer->SetMinimumStepLength( 0.001 );

  optimizer->SetNumberOfIterations( 200 );





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
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    return -1;
    } 
  
  OptimizerType::ParametersType finalParameters = 
                    registration->GetLastTransformParameters();


  const double versorX              = finalParameters[0];
  const double versorY              = finalParameters[1];
  const double versorZ              = finalParameters[2];
  const double finalCenterX         = finalParameters[3];
  const double finalCenterY         = finalParameters[4];
  const double finalCenterZ         = finalParameters[5];
  const double finalTranslationX    = finalParameters[6];
  const double finalTranslationY    = finalParameters[7];
  const double finalTranslationZ    = finalParameters[8];

  const unsigned int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();

  //
  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " versor X      = " << versorX  << std::endl;
  std::cout << " versor Y      = " << versorY  << std::endl;
  std::cout << " versor Z      = " << versorZ  << std::endl;
  std::cout << " Center X      = " << finalCenterX  << std::endl;
  std::cout << " Center Y      = " << finalCenterY  << std::endl;
  std::cout << " Center Z      = " << finalCenterZ  << std::endl;
  std::cout << " Translation X = " << finalTranslationX  << std::endl;
  std::cout << " Translation Y = " << finalTranslationY  << std::endl;
  std::cout << " Translation Z = " << finalTranslationZ  << std::endl;
  std::cout << " Iterations    = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue          << std::endl;


  //  Software Guide : BeginLatex
  //  
  //  Let's execute this example over some of the images available in
  //  the ftp site, for example:
  //  
  //  \begin{itemize}
  //  \item \code{brainweb165a10f17.mha} 
  //  \item \code{brainweb165a10f17Rot10Tx15.mha}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally rotating the first image
  //  by $10$ degrees and shifting it $15mm$ in $Y$.  The registration takes
  //  $47$ iterations and produce as result the parameters:
  //
  //  \begin{center}
  //  \begin{verbatim}
  //  [0.174482, 123.182, 147.108, 9.57676, 17.922]
  //  \end{verbatim}
  //  \end{center}
  //
  //  That are interpreted as
  //
  //  \begin{itemize}
  //  \item Angle         =                     $0.174482$   radians
  //  \item Center        = $( 123.182    , 147.108      )$ millimeters
  //  \item Translation   = $(   9.57676  ,  17.922      )$ millimeters
  //  \end{itemize}
  //  
  // 
  //  Note that the reported translation is not the translation of $(13,17)$
  //  that we may naively expecting. The reason is that the $5$ parameters of
  //  the \doxygen{CenteredRigid2DTransform} are redundant. The actual movement in
  //  space is described by only $3$ parameters. This means that there are
  //  infinite combinations of rotation center and translations that will
  //  represent the same actual movement in space. It it more illustrative in
  //  this case to take a look at the actual rotation matrix and offset
  //  resulting form the $5$ parameters.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  transform->SetParameters( finalParameters );

  TransformType::MatrixType matrix = transform->GetRotationMatrix();
  TransformType::OffsetType offset = transform->GetOffset();

  std::cout << "Matrix = " << std::endl << matrix << std::endl;
  std::cout << "Offset = " << std::endl << offset << std::endl;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Which produces the following output.
  //
  //
  //  \begin{verbatim}
  //  Matrix =
  //     0.984817 -0.173598
  //     0.173598 0.984817
  //
  //  Offset =
  //     36.9848  -1.22857
  //  \end{verbatim}
  //
  //  This output illustrates how counter intuitive could be the mix of center
  //  of rotation and translations. Figure
  //  \ref{fig:TranslationAndRotationCenter} will clarify this situation. The
  //  figures shows at left an original image. A rotation of $10^{\circ}$ around
  //  the center of the image is shown in the middle. The same rotation
  //  performed around the origin of coordinates is shown at right. It can be
  //  seen here that changing the center of rotation introduced additional
  //  translations.
  //
  //  Let's analyze what happens to the center of the image that we just
  //  registered. Under the point of view of rotating $10^{\circ}$ around the
  //  center and then applying a translation of $(13mm,17mm)$. The image has a
  //  size of $(221 \times 257)$ pixels and unit spacing. Hence its center has
  //  coordinates $(110.5,128.5)$. Since the rotation is done around this
  //  point, the center behaves as the fixed point of the transformation and
  //  passed unchanged. Then with the  $(13mm,17mm)$ translation it is mapped
  //  to $(123.5,145.5)$ which becomes its final position.
  //
  //  The matrix and offset that we obtained at the end of the registration say
  //  that this should be equivalent to a rotation of $10^{\circ}$ around the
  //  origin, followed by a translations of $(36.98,-1.22)$. Let's compute this
  //  in detail. First the rotation of the image center by $10^{\circ}$ around the
  //  origin will move the point to $(86.52,147.97)$. Now, applying a
  //  translation of $(36.98,-1.22)$ maps this point to $(123.5,146.75)$. Which
  //  is pretty close to the result of our previous computation.
  //
  //  It is unlikely that we could have choosen such tranlation as an initial
  //  guess, since we tend to think about image in a coordinate system whose
  //  origin is in the center of the image.
  // 
  // \begin{figure}
  // \center
  // \includegraphics[width=\textwidth]{TranslationAndRotationCenter.eps}
  // \itkcaption{Effect of changing the center of rotation.}
  // \label{fig:TranslationAndRotationCenter}
  // \end{figure}
  //
  //  Software Guide : EndLatex 


  //  Software Guide : BeginLatex
  //
  //  You may be wondering why if the actual movement is represented by three
  //  parameters we take the long way of using five. In particular when that
  //  results in having to deal with a $5$-dimensional space for the optimizer
  //  instead of a $3$-dimensional one. The answer is that by using 5
  //  parameters we have a much simpler way of initializing the transform with
  //  an appropriate rotation matrix and offset. Using the minimum three
  //  parameters is not obvious how to find what the rotation and translations
  //  should be.
  //
  //  Software Guide : EndLatex 


  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceBorder20.eps}
  // \includegraphics[width=0.44\textwidth]{BrainProtonDensitySliceR10X13Y17.eps}
  // \itkcaption[CenteredTransformInitializer input images]{Fixed and Moving image
  // provided as input to the registration method using
  // CenteredTransformInitializer.}
  // \label{fig:FixedMovingImageRegistration6}
  // \end{figure}
  //
  //
  // \begin{figure}
  // \center
  // \includegraphics[width=0.32\textwidth]{ImageRegistration6Output.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration6DifferenceBefore.eps}
  // \includegraphics[width=0.32\textwidth]{ImageRegistration6DifferenceAfter.eps} 
  // \itkcaption[CenteredTransformInitializer output images]{Resampled moving
  // image (left). Differences between fixed and moving images, before (center)
  // and after (right) registration with the
  // CenteredTransformInitializer.}
  // \label{fig:ImageRegistration6Outputs}
  // \end{figure}
  //
  // Figure \ref{fig:ImageRegistration6Outputs} shows the output of the
  // registration. The right most image of this Figure shows the squared
  // magnitude of pixel differences between the fixed image and the resampled
  // moving image. 
  //
  // \begin{figure}
  // \center
  // \includegraphics[height=0.32\textwidth]{ImageRegistration6TraceMetric.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration6TraceAngle.eps}
  // \includegraphics[height=0.32\textwidth]{ImageRegistration6TraceTranslations.eps} 
  // \itkcaption[CenteredTransformInitializer output plots]{Plots of the Metric,
  // rotation angle, center of rotation and translations during the
  // registration using CenteredTransformInitializer.}
  // \label{fig:ImageRegistration6Plots}
  // \end{figure}
  //
  //  Figure \ref{fig:ImageRegistration6Plots} shows the plots of the main
  //  output parameters of the registration process. The metric values at every
  //  iteration are shown on the top. The angle values are shown in the plot at
  //  left while the translation components of the registration are presented
  //  in the plot at right. Note that this is the complementary translation as
  //  used in the transform, not the atctual total translation that gets used
  //  in the offset of the transform. We could modify the Observer in order to
  //  print the total offset instead of printing the array of parameters. Let's
  //  call that an exercise for the reader !.
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


  typedef itk::SquaredDifferenceImageFilter< 
                                  FixedImageType, 
                                  FixedImageType, 
                                  OutputImageType > DifferenceFilterType;

  DifferenceFilterType::Pointer difference = DifferenceFilterType::New();

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( difference->GetOutput() );  
  

  // Compute the difference image between the 
  // fixed and resampled moving image.
  if( argc >= 5 )
    {
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( resample->GetOutput() );
    writer2->SetFileName( argv[4] );
    writer2->Update();
    }


  // Compute the difference image between the 
  // fixed and moving image before registration.
  if( argc >= 6 )
    {
    writer2->SetFileName( argv[5] );
    difference->SetInput1( fixedImageReader->GetOutput() );
    difference->SetInput2( movingImageReader->GetOutput() );
    writer2->Update();
    }



  return 0;

}

