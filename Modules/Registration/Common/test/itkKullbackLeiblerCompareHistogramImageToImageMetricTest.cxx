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

#include "itkKullbackLeiblerCompareHistogramImageToImageMetric.h"

#include "itkLinearInterpolateImageFunction.h"
#include "itkTimeProbesCollectorBase.h"
#include "vnl/vnl_sample.h"

#include <iostream>

/**
 *  This test uses two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 5 pixels from the other.
 *
 *  This test computes the KullbackLeibler information value and derivatives
 *  for various shift values in (-10,10).
 *
 */

int itkKullbackLeiblerCompareHistogramImageToImageMetricTest(int, char* [] )
{

//------------------------------------------------------------
// Create four simple images
//------------------------------------------------------------

  //Allocate Images
  typedef itk::Image<unsigned char,2>           MovingImageType;
  typedef itk::Image<unsigned char,2>           FixedImageType;
  typedef itk::Image<unsigned char,2>           TrainingMovingImageType;
  typedef itk::Image<unsigned char,2>           TrainingFixedImageType;

  enum { ImageDimension = MovingImageType::ImageDimension };

  MovingImageType::SizeType size = {{100,100}};
  MovingImageType::IndexType index = {{0,0}};
  MovingImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );

  MovingImageType::Pointer imgMoving = MovingImageType::New();
  imgMoving->SetLargestPossibleRegion( region );
  imgMoving->SetBufferedRegion( region );
  imgMoving->SetRequestedRegion( region );
  imgMoving->Allocate();

  FixedImageType::Pointer imgFixed = FixedImageType::New();
  imgFixed->SetLargestPossibleRegion( region );
  imgFixed->SetBufferedRegion( region );
  imgFixed->SetRequestedRegion( region );
  imgFixed->Allocate();

  MovingImageType::Pointer imgTrainingMoving = MovingImageType::New();
  imgTrainingMoving->SetLargestPossibleRegion( region );
  imgTrainingMoving->SetBufferedRegion( region );
  imgTrainingMoving->SetRequestedRegion( region );
  imgTrainingMoving->Allocate();

  FixedImageType::Pointer imgTrainingFixed = FixedImageType::New();
  imgTrainingFixed->SetLargestPossibleRegion( region );
  imgTrainingFixed->SetBufferedRegion( region );
  imgTrainingFixed->SetRequestedRegion( region );
  imgTrainingFixed->Allocate();

  // Fill images with a 2D gaussian
  typedef  itk::ImageRegionIterator<MovingImageType>
    ReferenceIteratorType;
  typedef  itk::ImageRegionIterator<FixedImageType>
    TargetIteratorType;
  typedef  itk::ImageRegionIterator<TrainingMovingImageType>
    TrainingReferenceIteratorType;
  typedef  itk::ImageRegionIterator<TrainingFixedImageType>
    TrainingTargetIteratorType;

  itk::Point<double,2> center;
  center[0] = (double)region.GetSize()[0]/2.0;
  center[1] = (double)region.GetSize()[1]/2.0;

  const double s = (double)region.GetSize()[0]/2.0;
  const double mag = (double)200.0;
  const double noisemag = (double)0.0; // ended up yielding best results

  itk::Point<double,2>  p;
  itk::Vector<double,2> d;

  // Set the displacement
  itk::Vector<double,2> displacement;
  displacement[0] = 5;
  displacement[1] = 0;

  ReferenceIteratorType ri(imgMoving,region);
  TargetIteratorType ti(imgFixed,region);
  TrainingReferenceIteratorType gri(imgTrainingMoving,region);
  TrainingTargetIteratorType gti(imgTrainingFixed,region);

  ri.GoToBegin();
  while(!ri.IsAtEnd())
    {
    p[0] = ri.GetIndex()[0];
    p[1] = ri.GetIndex()[1];
    d = p-center;
    d += displacement;
    const double x = d[0];
    const double y = d[1];
    ri.Set( (unsigned char) ( mag * std::exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ri;
    }

  ti.GoToBegin();
  while(!ti.IsAtEnd())
    {
    p[0] = ti.GetIndex()[0];
    p[1] = ti.GetIndex()[1];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    ti.Set( (unsigned char) ( mag * std::exp( - ( x*x + y*y )/(s*s) ) ) );
    ++ti;
    }

  vnl_sample_reseed(2334237);

  gri.GoToBegin();
  while(!gri.IsAtEnd())
    {
    p[0] = gri.GetIndex()[0];
    p[1] = gri.GetIndex()[1];
    d = p-center;
    //    d += displacement;
    const double x = d[0];
    const double y = d[1];
    gri.Set( (unsigned char) (( mag * std::exp( - ( x*x + y*y )/(s*s) ) )  +
      vnl_sample_normal(0.0, noisemag)));
    ++gri;
    }

  gti.GoToBegin();
  while(!gti.IsAtEnd())
    {
    p[0] = gti.GetIndex()[0];
    p[1] = gti.GetIndex()[1];
    d = p-center;
    const double x = d[0];
    const double y = d[1];
    gti.Set( (unsigned char) (( mag * std::exp( - ( x*x + y*y )/(s*s) ) )  +
      vnl_sample_normal(0.0, noisemag)));
    ++gti;
    }

//-----------------------------------------------------------
// Set up a transformer
//-----------------------------------------------------------
  typedef itk::AffineTransform< double, ImageDimension > TransformType;
  typedef TransformType::ParametersType                  ParametersType;

  TransformType::Pointer transformer    = TransformType::New();
  TransformType::Pointer TrainingTransform = TransformType::New();
  transformer->SetIdentity();
  TrainingTransform->SetIdentity();

//------------------------------------------------------------
// Set up a interpolator
//------------------------------------------------------------
  typedef itk::LinearInterpolateImageFunction< MovingImageType, double >
    InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  InterpolatorType::Pointer TrainingInterpolator = InterpolatorType::New();

//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------
  typedef itk::KullbackLeiblerCompareHistogramImageToImageMetric<
    FixedImageType, MovingImageType > MetricType;

  MetricType::Pointer metric = MetricType::New();

  // connect the interpolator
  metric->SetInterpolator( interpolator );

  // connect the transform
  metric->SetTransform( transformer );

  // connect the images to the metric
  metric->SetFixedImage( imgFixed );
  metric->SetMovingImage( imgMoving );

  // set the standard deviations
  //metric->SetFixedImageStandardDeviation( 5.0 );
  //metric->SetMovingImageStandardDeviation( 5.0 );

  // set the number of samples to use
  // metric->SetNumberOfSpatialSamples( 100 );

  unsigned int nBins = 64;
  MetricType::HistogramType::SizeType histSize;
  histSize.SetSize(2);
  histSize[0] = nBins;
  histSize[1] = nBins;
  metric->SetHistogramSize(histSize);

  // Set scales for derivative calculation.
  typedef MetricType::ScalesType ScalesType;
  ScalesType scales(transformer->GetNumberOfParameters());

  for (unsigned int k = 0; k < transformer ->GetNumberOfParameters(); k++)
    scales[k] = 1;

  metric->SetDerivativeStepLengthScales(scales);

  // set the region over which to compute metric
  metric->SetFixedImageRegion( imgFixed->GetBufferedRegion() );

//------------------------------------------------------------
// Set up the metric
//------------------------------------------------------------

  metric->SetTrainingInterpolator( TrainingInterpolator );
  metric->SetTrainingFixedImage( imgTrainingFixed );
  metric->SetTrainingMovingImage( imgTrainingMoving );
  metric->SetTrainingFixedImageRegion( imgTrainingFixed->GetBufferedRegion() );
  metric->SetTrainingTransform( TrainingTransform );

  // initialize the metric before use
  metric->Initialize();

//------------------------------------------------------------
// Set up a affine transform parameters
//------------------------------------------------------------
  unsigned int numberOfParameters = transformer->GetNumberOfParameters();
  ParametersType parameters( numberOfParameters );

  // set the parameters to the identity
  unsigned long count = 0;

     // initialize the linear/matrix part
  for( unsigned int row = 0; row < ImageDimension; row++ )
    {
    for( unsigned int col = 0; col < ImageDimension; col++ )
      {
      parameters[count] = 0;
      if( row == col )
        {
        parameters[count] = 1;
        }
      ++count;
      }
    }

     // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
    {
    parameters[count] = 0;
    ++count;
    }

//---------------------------------------------------------
// Print out KullbackLeibler values
// for parameters[4] = {-10,10}
//---------------------------------------------------------

  MetricType::MeasureType measure;
  MetricType::DerivativeType derivative( numberOfParameters );

  itk::TimeProbesCollectorBase   collector;
  collector.Start("Loop");

  std::cout << "param[4]\tKullbackLeibler\tdKullbackLeibler/dparam[4]" << std::endl;

  for( double trans = -10; trans <= 4; trans += 0.5 )
    {
    parameters[4] = trans;
    metric->GetValueAndDerivative( parameters, measure, derivative );

    std::cout << trans << "\t" << measure << "\t" << derivative[4] <<std::endl;

    // exercise the other functions
    metric->GetValue( parameters );
    metric->GetDerivative( parameters, derivative );

    }
  collector.Stop("Loop");
  collector.Report();

//-------------------------------------------------------
// exercise misc member functions
//-------------------------------------------------------
  std::cout << "Name of class: " <<
    metric->GetNameOfClass() << std::endl;
//  std::cout << "No. of samples used = " <<
//    metric->GetNumberOfSpatialSamples() << std::endl;
//  std::cout << "Fixed image std dev = " <<
//    metric->GetFixedImageStandardDeviation() << std::endl;
//  std::cout << "Moving image std dev = " <<
//    metric->GetMovingImageStandardDeviation() << std::endl;

  metric->Print( std::cout );

//  itk::KernelFunctionBase::Pointer theKernel = metric->GetKernelFunction();
//  metric->SetKernelFunction( theKernel );
//  theKernel->Print( std::cout );

//  std::cout << "Try causing a exception by making std dev too small";
//  std::cout << std::endl;
//  metric->SetFixedImageStandardDeviation( 0.001 );
//  try
//    {
//    metric->Initialize();
//    std::cout << "Value = " << metric->GetValue( parameters );
//    std::cout << std::endl;
//    }
//  catch(itk::ExceptionObject &err)
//    {
//    std::cout << "Caught the exception." << std::endl;
//    std::cout << err << std::endl;
//    }
//
//  // reset standard deviation
//  metric->SetFixedImageStandardDeviation( 5.0 );

  std::cout << "Try causing a exception by making fixed image ITK_NULLPTR";
  std::cout << std::endl;
  metric->SetFixedImage( ITK_NULLPTR );
  try
    {
    metric->Initialize();
    std::cout << "Value = " << metric->GetValue( parameters );
    std::cout << std::endl;
    }
  catch( itk::ExceptionObject &err)
    {
    std::cout << "Caught the exception." << std::endl;
    std::cout << err << std::endl;
    }

  return EXIT_SUCCESS;
}
