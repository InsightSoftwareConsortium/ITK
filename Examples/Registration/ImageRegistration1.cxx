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
  //  evaluate the fixed image at non-grid positions.
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
  //  respective \code{New()} method and are assigned to their
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
  //  The parameters of the transform are initialized by passing them in an
  //  array. This can be used to setup an initial known correction to the
  //  miss-registration. In this particular case, a translation transform is
  //  being used for the registration. The array of parameters for this
  //  transform is simply composed by the values of translation along each
  //  dimension. Setting the values of the parameters to zero lead to
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

  initialParameters.Fill( 0.0 );  // Initialize to zero
  
  registration->SetInitialTransformParameters( initialParameters );
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //
  //  At this point the registration method is ready to be executed. The
  //  optimizer is the component that drives the execution of the registration.
  //  However, the registration methods orchestrates the ensemble in order to
  //  make sure that everything is in place before the control is passed to the
  //  optimizer.
  //
  //  The registration process is triggered by an invokation of the
  //  \code{StartRegistration()} method. If something goes wrong during the
  //  initialization or execution of the registration an exception will be
  //  thrown. We should hencforth place the \code{StartRegistration()} method
  //  in a try/catch block as illustrated in the following lines.
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


  return 0;

}

