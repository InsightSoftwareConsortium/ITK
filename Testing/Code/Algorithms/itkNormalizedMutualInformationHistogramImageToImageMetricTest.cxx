/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedMutualInformationHistogramImageToImageMetricTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGaussianImageSource.h"
#include "itkImage.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkNormalizedMutualInformationHistogramImageToImageMetric.h"
#include "itkTranslationTransform.h"

/** This test uses two 2D-Gaussians (standard deviation RegionSize/2).
    This test computes the normalized mutual information between the two
    images.
*/
int itkNormalizedMutualInformationHistogramImageToImageMetricTest(int argc,
  char* argv[])
{
  try {
    // Create two simple images.
    const unsigned int ImageDimension = 2;
    typedef double PixelType;
    typedef double CoordinateRepresentationType;
    
    //Allocate Images
    typedef itk::Image<PixelType,ImageDimension> MovingImageType;
    typedef itk::Image<PixelType,ImageDimension> FixedImageType;
    
    // Declare Gaussian Sources
    typedef itk::GaussianImageSource<MovingImageType> MovingImageSourceType;
    typedef itk::GaussianImageSource<FixedImageType> FixedImageSourceType;
    typedef MovingImageSourceType::Pointer MovingImageSourcePointer;
    typedef FixedImageSourceType::Pointer FixedImageSourcePointer;
    
    // Note: the following declarations are classical arrays
    unsigned long fixedImageSize[] = {100,  100};
    unsigned long movingImageSize[] = {100,  100}; 
    
    float fixedImageSpacing[]  = {1.0f, 1.0f}; 
    float movingImageSpacing[] = {1.0f, 1.0f}; 
    
    float fixedImageOrigin[] = {0.0f, 0.0f}; 
    float movingImageOrigin[] = {0.0f, 0.0f}; 
    
    MovingImageSourceType::Pointer movingImageSource =
      MovingImageSourceType::New();
    FixedImageSourceType::Pointer  fixedImageSource  =
      FixedImageSourceType::New();
    
    movingImageSource->SetSize(movingImageSize);
    movingImageSource->SetOrigin(movingImageOrigin);
    movingImageSource->SetSpacing(movingImageSpacing);
    movingImageSource->SetNormalized(false);
    movingImageSource->SetScale(250.0f);
    
    fixedImageSource->SetSize(fixedImageSize);
    fixedImageSource->SetOrigin(fixedImageOrigin);
    fixedImageSource->SetSpacing(fixedImageSpacing);
    fixedImageSource->SetNormalized(false);
    fixedImageSource->SetScale(250.0f);
    
    movingImageSource->Update(); // Force the filter to run
    fixedImageSource->Update();  // Force the filter to run
    
    MovingImageType::Pointer movingImage = movingImageSource->GetOutput();
    FixedImageType::Pointer  fixedImage  = fixedImageSource->GetOutput();
    
    // Set up the metric.
    typedef itk::NormalizedMutualInformationHistogramImageToImageMetric<
      FixedImageType, MovingImageType> MetricType;
    typedef MetricType::TransformType TransformBaseType;
    typedef MetricType::ScalesType ScalesType;
    typedef TransformBaseType::ParametersType ParametersType;
    
    MetricType::Pointer metric = MetricType::New();
    
    unsigned int nBins = 256;
    MetricType::HistogramType::SizeType histSize;
    histSize[0] = nBins;
    histSize[1] = nBins;
    metric->SetHistogramSize(histSize);
    
    // Plug the images into the metric.
    metric->SetFixedImage(fixedImage);
    metric->SetMovingImage(movingImage);

    // Set up a transform.
    typedef itk::TranslationTransform<CoordinateRepresentationType,
      ImageDimension> TransformType;
    
    TransformType::Pointer transform = TransformType::New();
    metric->SetTransform(transform.GetPointer());
    
    // Set up an interpolator.
    typedef itk::LinearInterpolateImageFunction<MovingImageType,
      double> InterpolatorType;
    
    InterpolatorType::Pointer interpolator = InterpolatorType::New();
    interpolator->SetInputImage(movingImage.GetPointer());
    metric->SetInterpolator(interpolator.GetPointer());
    
    // Define the region over which the metric will be computed.
    metric->SetFixedImageRegion(fixedImage->GetBufferedRegion());

    // Set up transform parameters.
    ParametersType parameters(transform->GetNumberOfParameters());

    for (unsigned int k = 0; k < ImageDimension; k++)
      parameters[k] = 0.0f;

    // Set scales for derivative calculation.
    ScalesType scales(transform->GetNumberOfParameters());

    for (unsigned int k = 0; k < ImageDimension; k++)
      scales[k] = 1;

    metric->SetDerivativeStepLengthScales(scales);

    // Initialize the metric.
    metric->Initialize();

    // Print out metric value and derivative.
    MetricType::MeasureType measure = metric->GetValue(parameters);
    MetricType::DerivativeType derivative;
    metric->GetDerivative(parameters, derivative);

    std::cout << "Metric value = " << measure << std::endl
              << "Derivative = " << derivative << std::endl;
 
    // Exercise Print() method.
    metric->Print(std::cout);

    std::cout << "Test passed." << std::endl;
  }
  catch (itk::ExceptionObject& ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
