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
// means that you don't ask \emph{why ?}. Instead, just use it as an overview of
// the typical elements involved in solving an image registration problem.
//
// \index{itk::Image!Instantiation|textbf}
// \index{itk::Image!Header|textbf}
//
// First, the header file of the Image class must be included.
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



int main()
{

  
  // Software Guide : BeginLatex
  // 
  // The types of each one of the components in the registration methods should
  // be instantiated.
  //
  // Software Guide : EndLatex 
  //
  // Software Guide : BeginCodeSnippet 
  const unsigned int Dimension = 2;
  typedef float PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::TranslationTransform< double, Dimension > TransformType;

  typedef itk::RegularStepGradientDescentOptimizer       OptimizerType;

  typedef itk::MeanSquaresImageToImageMetric< 
                                    FixedImageType, 
                                    MovingImageType >    MetricType;

  typedef itk:: LinearInterpolateImageFunction< 
                                    MovingImageType,
                                    double          >    InterpolatorType;
  // Software Guide : EndCodeSnippet 


  //  Software Guide : BeginLatex
  //
  //  The RegistrationMethod type is instantiated using the types of the fixed
  //  and moving images.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageRegistrationMethod< 
                                    FixedImageType, 
                                    MovingImageType >    RegistrationType;
  // Software Guide : EndCodeSnippet


  MetricType::Pointer         metric        = MetricType::New();
  TransformType::Pointer      transform     = TransformType::New();
  OptimizerType::Pointer      optimizer     = OptimizerType::New();
  FixedImageType::Pointer     fixedImage    = FixedImageType::New();  
  MovingImageType::Pointer    movingImage   = MovingImageType::New();  
  InterpolatorType::Pointer   interpolator  = InterpolatorType::New();
  RegistrationType::Pointer   registration  = RegistrationType::New();
  
  registration->SetMetric(        metric        );
  registration->SetOptimizer(     optimizer     );
  registration->SetTransform(     transform     );
  registration->SetFixedImage(    fixedImage    );
  registration->SetMovingImage(   movingImage   );
  registration->SetInterpolator(  interpolator  );

  typedef RegistrationType::ParametersType ParametersType;
  ParametersType initialParameters( transform->GetNumberOfParameters() );

  registration->SetInitialTransformParameters( initialParameters );

  try 
    { 
    registration->StartRegistration(); 
    } 
  catch( itk::ExceptionObject& err ) 
    { 
    std::cout << "Caught expected ExceptionObject" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
    } 

  return 0;

}

