/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    MeanSquaresImageMetric1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// This example illustrates how to explore the domain of an image metric.  This
// is a useful exercise to do before starting a registration process, since
// getting familiar with the characteristics of the metric is fundamental for
// the apropriate selection of the optimizer to be use for driving the
// registration process, as well as for selecting the optimizer parameters.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkTranslationTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkLinearInterpolateImageFunction.h"


int main( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  fixedImage  movingImage" << std::endl;
    return 1;
    }

  const     unsigned int   Dimension = 2;
  typedef   unsigned char  PixelType;

  typedef itk::Image< PixelType, Dimension >   ImageType;
  typedef itk::Image< PixelType, Dimension >   ImageType;


  typedef itk::ImageFileReader< ImageType >  ReaderType;

  ReaderType::Pointer fixedReader  = ReaderType::New();
  ReaderType::Pointer movingReader = ReaderType::New();

  fixedReader->SetFileName(  argv[ 1 ] );
  movingReader->SetFileName( argv[ 2 ] );

  try 
    {
    fixedReader->Update();
    movingReader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }


  typedef itk::MeanSquaresImageToImageMetric< ImageType, ImageType >  MetricType;

  MetricType::Pointer metric = MetricType::New();



  typedef itk::TranslationTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();



//  typedef itk::LinearInterpolateImageFunction< 
  typedef itk::NearestNeighborInterpolateImageFunction< 
                                 ImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();


  metric->SetInterpolator( interpolator );
  metric->SetTransform( transform );


  transform->SetIdentity();

  ImageType::ConstPointer fixedImage  = fixedReader->GetOutput();
  ImageType::ConstPointer movingImage = movingReader->GetOutput();

  metric->SetFixedImage(  fixedImage  );
  metric->SetMovingImage( movingImage );

  metric->SetFixedImageRegion(  fixedImage->GetBufferedRegion()  );

  try 
    {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    return -1;
    }


  MetricType::TransformParametersType displacement( Dimension );

  int rangex = 50;
  int rangey = 50;

  for( int dx = -rangex; dx <= rangex; dx++ )
    {
    for( int dy = -rangey; dy <= rangey; dy++ )
      {
      displacement[0] = dx;
      displacement[1] = dy;
      const double value = metric->GetValue( displacement );

      std::cout << dx << "   "  << dy << "   " << value << std::endl;
      }
    }

  std::cout << std::endl;


  return 0;
}

// Software Guide : EndCodeSnippet

