/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeBasedKmeansEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapContourOverlayImageFilter.h"


int itkLabelMapContourOverlayImageFilterTest1(int argc, char * argv[])
{
  if( argc != 9 )
    {
    std::cerr << "usage: " << argv[0] << " input input output opacity type thickness dilation priority sliceDim" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;

  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< IType > ConverterType;
  ConverterType::Pointer converter = ConverterType::New();
  converter->SetInput( reader->GetOutput() );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

//  typedef itk::RGBPixel< unsigned char > RGBPixelType;
//  typedef itk::Image< RGBPixelType, dim > RGBImageType;

  typedef itk::LabelMapContourOverlayImageFilter< ConverterType::OutputImageType, IType > ColorizerType;
  ColorizerType::Pointer colorizer = ColorizerType::New();
  colorizer->SetInput( converter->GetOutput() );
  colorizer->SetFeatureImage( reader2->GetOutput() );
  colorizer->SetOpacity( atof(argv[4]) );
  colorizer->SetType( atof(argv[5]) );
  ColorizerType::SizeType r;
  r.Fill( atoi(argv[6]) );
  colorizer->SetContourThickness( r );
  r.Fill( atoi(argv[7]) );
  colorizer->SetDilationRadius( r );
  colorizer->SetPriority( atof(argv[8]) );


  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  typedef itk::ImageFileWriter< ColorizerType::OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( colorizer->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();
  return 0;
}
