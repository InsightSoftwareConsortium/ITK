/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaStatisticImageToImageMetricTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkKappaStatisticImageToImageMetric.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTranslationTransform.h"

/**
 *  This test exercised the various methods in the
 *  itkKappaStatisticImageToImageMetric class.  Two binary images are
 *  created for testing purposes -- one of a square and another of the
 *  same square translated in both x and y.
 *
 */

int itkKappaStatisticImageToImageMetricTest(int, char* [] )
{
  typedef itk::Image< unsigned char, 2 >                                               UCharImage2DType;
  typedef itk::Image< double, 2 >                                                      DoubleImage2DType;
  typedef itk::KappaStatisticImageToImageMetric< UCharImage2DType, UCharImage2DType >  MetricType;
  typedef itk::ImageRegionIteratorWithIndex< UCharImage2DType >                        UCharIteratorType;
  typedef itk::ImageRegionIteratorWithIndex< DoubleImage2DType >                       DoubleIteratorType;
  typedef itk::TranslationTransform< double, 2 >                                       TransformType;
  typedef itk::NearestNeighborInterpolateImageFunction< UCharImage2DType, double >     InterpolatorType;

  double epsilon = 0.000001;

  TransformType::Pointer    transform    = TransformType::New(); 
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  UCharImage2DType::SizeType imageSize;
    imageSize[0] = 128;
    imageSize[1] = 128;

  // Create fixed image
  UCharImage2DType::Pointer fixedImage = UCharImage2DType::New();
    fixedImage->SetRegions(imageSize);
    fixedImage->Allocate();
    fixedImage->FillBuffer(0);
    fixedImage->Update();
    
  UCharIteratorType fixedIt( fixedImage, fixedImage->GetBufferedRegion() );
  for ( fixedIt.GoToBegin(); !fixedIt.IsAtEnd(); ++fixedIt )
    {
    UCharImage2DType::IndexType index = fixedIt.GetIndex();
    if ((index[0]>=48)&&(index[0]<=80)&&(index[1]>=48)&&(index[1]<=80))
      {
      fixedIt.Set(255);
      }
    }

  // Create moving image
  UCharImage2DType::Pointer movingImage = UCharImage2DType::New();
    movingImage->SetRegions(imageSize);
    movingImage->Allocate();
    movingImage->FillBuffer(0);
    movingImage->Update();
    
  UCharIteratorType movingIt( movingImage, movingImage->GetBufferedRegion() );
  for ( movingIt.GoToBegin(); !movingIt.IsAtEnd(); ++movingIt )
    {
    UCharImage2DType::IndexType index = movingIt.GetIndex();
    if ((index[0]>=55)&&(index[0]<=87)&&(index[1]>=55)&&(index[1]<=87))
      {
      movingIt.Set(255);
      }
    }

  MetricType::Pointer metric = MetricType::New();


  //------------------------------------------------------------------
  // exercise [Set,Get]ForegroundValue method
  //------------------------------------------------------------------
  std::cout << "Test [Set,Get]ForegroundValue method..." << std::endl;

    metric->SetForegroundValue(255);
  if (metric->GetForegroundValue()!=255)
    {
    std::cerr << "Error!" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;
  

  //------------------------------------------------------------------
  // exercise GetValue method
  //------------------------------------------------------------------
  std::cout << "Test GetValue method..." << std::endl;
  transform->SetIdentity();

  TransformType::ParametersType parameters = transform->GetParameters();

    metric->SetFixedImage( fixedImage );
    metric->SetMovingImage( movingImage );
    metric->SetInterpolator( interpolator );
    metric->SetTransform( transform );
    metric->SetFixedImageRegion( fixedImage->GetBufferedRegion() );
  try
    {
    metric->Initialize();
    }
  catch ( itk::ExceptionObject &excp )
    {
    std::cerr << "Exception caught while initializing metric." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // The value 0.620753 was computed by hand for these two images
  if (!((metric->GetValue( parameters ) >= 0.620753-epsilon)&&(metric->GetValue( parameters ) <= 0.620753+epsilon)))
    {
    std::cerr << "Error!" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;


  //------------------------------------------------------------------
  // exercise ComputeGradient method
  //------------------------------------------------------------------
  std::cout << "Test ComputeGradient method..." << std::endl;
  metric->ComputeGradient();

  DoubleImage2DType::Pointer xGradImage = DoubleImage2DType::New();
    xGradImage->SetRegions(imageSize);
    xGradImage->Allocate();
    xGradImage->FillBuffer(0);
    xGradImage->Update();

  DoubleImage2DType::Pointer yGradImage = DoubleImage2DType::New();
    yGradImage->SetRegions(imageSize);
    yGradImage->Allocate();
    yGradImage->FillBuffer(0);
    yGradImage->Update();

  DoubleIteratorType xGradIt( xGradImage, xGradImage->GetBufferedRegion() );
  DoubleIteratorType yGradIt( yGradImage, yGradImage->GetBufferedRegion() );

  xGradIt.GoToBegin();
  yGradIt.GoToBegin();

  // Construct the gradient images explicitly based on what we know
  // they should be and use them to validate metric's version
  while ( !xGradIt.IsAtEnd() )
    {
    DoubleImage2DType::IndexType index = xGradIt.GetIndex();

    if (((index[0]==54)||(index[0]==55))&&(index[1]>=55)&&(index[1]<=87))
      {
      xGradIt.Set(1);
      }
    if (((index[0]==87)||(index[0]==88))&&(index[1]>=55)&&(index[1]<=87))
      {
      xGradIt.Set(-1);
      }
    if (((index[1]==54)||(index[1]==55))&&(index[0]>=55)&&(index[0]<=87))
      {
      yGradIt.Set(1);
      }
    if (((index[1]==87)||(index[1]==88))&&(index[0]>=55)&&(index[0]<=87))
      {
      yGradIt.Set(-1);
      }

    ++xGradIt;
    ++yGradIt;
    }

  typedef itk::ImageRegionIteratorWithIndex< const MetricType::GradientImageType >  GradIteratorType;
  GradIteratorType gradIt( metric->GetGradientImage(), metric->GetGradientImage()->GetBufferedRegion() );
  gradIt.GoToBegin();
  xGradIt.GoToBegin();
  yGradIt.GoToBegin();
  while ( !gradIt.IsAtEnd() )
    {
    if (((gradIt.Get())[0] != xGradIt.Get())||((gradIt.Get())[1] != yGradIt.Get()))
      {
      std::cerr << "Error!" << std::endl;
      return EXIT_FAILURE;
      }

    ++gradIt;
    ++xGradIt;
    ++yGradIt;
    }
  std::cout << " [ PASSED ] " << std::endl;


  //------------------------------------------------------------------
  // exercise GetDerivative method
  //------------------------------------------------------------------
  std::cout << "Test GetDerivative method..." << std::endl;
  MetricType::DerivativeType derivative;
  metric->GetDerivative( parameters, derivative );

  // The value 0.0477502 was computed by hand
  if (!((derivative[0]>=-0.0477502-epsilon)&&(derivative[0]<=-0.0477502+epsilon)))
    {
    std::cerr << "Error!" << std::endl;
    return EXIT_FAILURE;
    }
  if (!((derivative[1]>=-0.0477502-epsilon)&&(derivative[1]<=-0.0477502+epsilon)))
    {
    std::cerr << "Error!" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;


  //------------------------------------------------------------------
  // exercise Complement method
  //------------------------------------------------------------------
  std::cout << "Test Complement method..." << std::endl;
  metric->ComplementOn();

  // The value 0.379247 was computed by hand
  if (!((metric->GetValue(parameters)>=0.379247-epsilon)&&(metric->GetValue(parameters)<=0.379247+epsilon)))
    {
    std::cerr << "Error!" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " [ PASSED ] " << std::endl;    


  return EXIT_SUCCESS;
}
