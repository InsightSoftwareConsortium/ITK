/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageRegistration1.cxx
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
// This example illustrates the use of the image registration framework in
// Insight.  It should be readed as a \emph{Hello World} in registration. Which
// means that by now you don't ask: \emph{why ?}. Instead, just use it as an
// overview of the typical elements involved in solving an image registration
// problem.
//
// \index{itk::Image!Instantiation|textbf}
// \index{itk::Image!Header|textbf}
//
// A registration method requires the following set of components: two input
// Images, a Transform, a Metric, an Interpolator and an Optimizer. Some of
// these components are parametrized by the image type for which the
// registration is intended.  The following header files provide declarations
// for common types
// of these components.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageRegistrationMethod.h"
#include "itkTranslationTransform.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"



int main( int argc, char **argv )
{


  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile  movingImageFile outputImagefile " << std::endl;
    return 1;
    }
  
  // Software Guide : BeginLatex
  // 
  // The types of each one of the components in the registration methods should
  // be instantiated. First we select the image dimension and the type for
  // representing image pixels.
  //
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  const unsigned int Dimension = 2;
  typedef float PixelType;
  // Software Guide : EndCodeSnippet

  
  //  Software Guide : BeginLatex
  //  
  //  The types of the input images are instantiated by the following lines.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The Transform that will map one image space into the other is defined
  //  below.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension > TransformType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  An optimizer if required to explore the parameter space of the transform
  //  in search of optimal values of the metric..
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The metric will compare how well the two images match. Metric types are
  //  usually parametrized by the image types as can be seen in the following
  //  type declaration.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::MeanSquaresImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  Finally, the type of the interpolator is declared. This interpolator will
  //  evaluate the moving image at non-grid positions.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;
  // Software Guide : EndCodeSnippet 


  //  Software Guide : BeginLatex
  //
  //  The RegistrationMethod type is instantiated using the types of the fixed
  //  and moving images. This class is just putting together all the components 
  //  we have described so far.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Each one of the registration components are created using their
  //  \code{New()} method and are assigned to their respective 
  //  \code{SmartPointer}.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  // Software Guide : EndCodeSnippet
  


  //  Software Guide : BeginLatex
  //
  //  The components are connected to the instance of the registration method.
  //  \index{itk::RegistrationMethod!SetMetric()}
  //  \index{itk::RegistrationMethod!SetOptimizer()}
  //  \index{itk::RegistrationMethod!SetTransform()}
  //  \index{itk::RegistrationMethod!SetFixedImage()}
  //  \index{itk::RegistrationMethod!SetMovingImage()}
  //  \index{itk::RegistrationMethod!SetInterpolator()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetInterpolator(  interpolator  );
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer  fixedImageReader  = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName(  argv[1] );
  movingImageReader->SetFileName( argv[2] );




  //  Software Guide : BeginLatex
  //  
  //  In this example, the fixed and moving images are read from files. This
  //  requires the \code{RegistrationMethod} to connect its inputs to the
  //  output of the respective readers.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  registration->SetFixedImage(    fixedImageReader->GetOutput()    );
  registration->SetMovingImage(   movingImageReader->GetOutput()   );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The registration can be restricted to consider only a particular region
  //  of the fixed image as input to the metric computation. This region is
  //  defined by the \code{SetFixedImageRegion()} method.  In this example we
  //  use the full available content of the image. This region is identified by
  //  the \code{BufferedRegion} of the fixed image. Note that for this region
  //  to be valid the reader must first invoke its \code{Update()} method.
  //
  //  \index{itk::ImageRegistrationMethod!SetFixedImageRegion()}
  //  \index{itk::Image!GetBufferedRegion()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  fixedImageReader->Update();

  registration->SetFixedImageRegion( 
     fixedImageReader->GetOutput()->GetBufferedRegion() );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  The parameters of the transform are initialized by passing them in an
  //  array. This can be used to setup an initial known correction to the
  //  miss-registration. In this particular case, a translation transform is
  //  being used for the registration. The array of parameters for this
  //  transform is simply composed by the values of translation along each
  //  dimension. Setting the values of the parameters to zero leads to
  //  initialize the transform as an \emph{identity} transform. Note that the
  //  array constructor requires the number of elements as argument.
  //
  //  \index{itk::TranslationTransform!GetNumberOfParameters()}
  //  \index{itk::RegistrationMethod!SetInitialTransformParameters()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  initialParameters[0] = 0.0;  // Initial offset in mm along X
  initialParameters[1] = 0.0;  // Initial offset in mm along Y
  
  registration->SetInitialTransformParameters( initialParameters );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  At this point the registration method is ready to be executed. The
  //  optimizer is the component that drives the execution of the registration.
  //  However, the \code{RegistrationMethod} class orchestrates the ensemble in
  //  order to make sure that everything is in place before the control is
  //  passed to the optimizer.
  //
  //  It is usually desirable to fine tune the parameters of the optimizer.
  //  Each optimizer have particular parameters that must be interpreted in the
  //  context of the optimization strategy it implements. The optimizer used in
  //  this example is a variant of gradient descent that attempts to prevent too
  //  large steps to be taken.  At each iteration this optimizer will make an
  //  step along the direction of the \code{ImageMetric} derivative. The
  //  initial lenght of the step is defined by the user. Each time that the
  //  direction of the derivative changes abruptly the optimizer assumes that a
  //  local extrema has been passed and reacts by reducing the step lenght by a
  //  half. After several reduction of the step lenght the optimizer may be
  //  moving in a very restricted area of the transform parameters space . The
  //  user can define how small the step length should be to consider that the
  //  method has converged. This is equivalent to define the precision with
  //  which the final transform is to be known.
  //
  //  The initial step length is defined with the method
  //  \code{SetMaximumStepLength()} while the tolerance for convergence is
  //  defined with the method \code{SetMinimumStepLength()}.
  //
  //  \index{itk::RegularStepGradientDescentOptimizer!SetMaximumStepLength()}
  //  \index{itk::RegularStepGradientDescentOptimizer!SetMinimumStepLength()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->SetMaximumStepLength( 4.00 );  
  optimizer->SetMinimumStepLength( 0.01 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  In case the optimizer never succed in reaching the desired precision
  //  tolerance it is prudent to establish a limit to the number of iterations
  //  to be performed. This maximum number is defined with the method
  //  \code{SetNumberOfIterations()}.
  //
  //  \index{itk::RegularStepGradientDescentOptimizer!SetNumberOfIterations()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  optimizer->SetNumberOfIterations( 200 );
  // Software Guide : EndCodeSnippet




  //  Software Guide : BeginLatex
  //  
  //  The registration process is triggered by an invokation of the
  //  \code{StartRegistration()} method. If something goes wrong during the
  //  initialization or execution of the registration an exception will be
  //  thrown. We should henceforth place the \code{StartRegistration()} method
  //  in a \code{try/catch} block as illustrated in the following lines.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet


  
  //  Software Guide : BeginLatex
  //  
  // In a real application you may attempt to recover from the error in the
  // catch block. Here we are simply printing a message and cowardly refusing
  // to continue with the execution of the program.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginLatex
  //  
  //  The result of the registration process is an array of parameters that
  //  defines the spatial transformation in an unique way. This final result is
  //  obtained using the \code{GetLastTransformParameters()} method.
  //
  //  \index{itk::RegistrationMethod!GetLastTransformParameters()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ParametersType result = registration->GetLastTransformParameters();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  In the case of the \code{itk::TranslationTransform} the parameters can be
  //  interpreted very straighforward. Each element of the array corresponds to
  //  a translation along one of the dimensions of space.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const double TranslationAlongX = result[0];
  const double TranslationAlongY = result[1];
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //  
  //  The optimizer cat be queried for the actual number of iterations.  The
  //  \code{GetCurrentIteration()} method returns this value. A large value of
  //  iterations may be an indication that the maximum step length has been set
  //  too small, which is undesirable since it results in long computational
  //  times.
  //
  //  \index{itk::RegularStepGradientDescentOptimizer!GetCurrentIteration()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  const int numberOfIterations = optimizer->GetCurrentIteration();
  // Software Guide : EndCodeSnippet


  //
  // Print out results
  //
  std::cout << "Result = " << std::endl;
  std::cout << " Translation X = " << TranslationAlongX <<  std::endl;
  std::cout << " Translation Y = " << TranslationAlongY <<  std::endl;
  
  std::cout << " Iterations    = " << numberOfIterations << std::endl;


  //  Software Guide : BeginLatex
  //  
  //  Let's execute this example over the some of the images provided in
  //  \code{Insight/Examples/Data}, for example:
  //  
  //  \begin{itemize}
  //  \item \code{BrainProtonDensitySliceBorder20.png} 
  //  \item \code{BrainProtonDensitySliceShifted13x17y.png}
  //  \end{itemize}
  //
  //  The second image is the result of intentionally tranlating the first
  //  image by $(13,17)$ millimeters assuming unit-spacing. Both images are
  //  shown in Figure \ref{fig:FixedMovingImageRegistration1}. The registration
  //  takes 18 iterations and produce as result the parameters:
  //
  //  \code{Translation X = 12.9903}
  //  \code{Translation Y = 17.0001}
  // 
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



  //  Software Guide : BeginLatex
  //  
  //  It is common, as a last step of a registration task, to use the resulting
  //  transform to map the moving image into the fixed image space.  This is
  //  easily done with the \code{itk::ResampleImageFilter}. Please refer to
  //  section \ref{sec:ResampleImageFilter} for details on the use of this
  //  filter.  First a \code{ResampleImageFilter} type is instantiated using
  //  the image types. It is convenient to use the fixed image type as the
  //  output type since probably the transformed moving image will be compared
  //  with the fixed image.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter< 
                            MovingImageType, 
                            FixedImageType >    ResampleFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //  
  //  A transform of the same type used in the registration process should be
  //  created and initialized with the parameters resulting from the registration 
  //  process. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  TransformType::Pointer finalTransform = TransformType::New();

  finalTransform->SetParameters( result );
  // Software Guide : EndCodeSnippet

  



  //  Software Guide : BeginLatex
  //  
  //  Then a resampling filter is created and the corresponding transform and
  //  moving image connected as inputs.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  resample->SetTransform( finalTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  // Software Guide : EndCodeSnippet
  




  //  Software Guide : BeginLatex
  //  
  //  The output of the filter is passed to a writer that will store the image
  //  in a file and the \code{Update()} method of the writer is invoked in
  //  order to trigger the execution of the pipeline.
  //
  //  Software Guide : EndLatex 
  
  typedef itk::ImageFileWriter< FixedImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[3] );
  
  // Software Guide : BeginCodeSnippet
  writer->SetInput( resample->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet



  return 0;

}

