/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelComponentImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkRelabelComponentImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkFilterWatcher.h"
#include "itkChangeInformationImageFilter.h"

int itkRelabelComponentImageFilterTest(int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage threshold_low threshold_hi" << std::endl;
    return 1;
    }

  typedef   unsigned short  InternalPixelType;
  typedef   unsigned short  OutputPixelType;
  typedef   unsigned char   WritePixelType;
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  typedef itk::Image<OutputPixelType, Dimension> OutputImageType;
  typedef itk::Image<WritePixelType, Dimension> WriteImageType;

  typedef itk::ImageFileReader< InternalImageType > ReaderType;
  typedef itk::ImageFileWriter<  WriteImageType  > WriterType;


  typedef itk::ChangeInformationImageFilter<InternalImageType> ChangeFilterType;
  typedef itk::BinaryThresholdImageFilter< InternalImageType, InternalImageType > ThresholdFilterType;
  typedef itk::ConnectedComponentImageFilter< InternalImageType, InternalImageType > ConnectedComponentType;
  typedef itk::RelabelComponentImageFilter< InternalImageType, OutputImageType > RelabelComponentType;
  typedef itk::BinaryThresholdImageFilter<OutputImageType, WriteImageType> FinalThresholdFilterType;


  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  ChangeFilterType::Pointer change = ChangeFilterType::New();
  ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();
  ConnectedComponentType::Pointer connected = ConnectedComponentType::New();
  RelabelComponentType::Pointer relabel = RelabelComponentType::New();
  FinalThresholdFilterType::Pointer finalThreshold = FinalThresholdFilterType::New();

  
  FilterWatcher watcher(relabel);

  reader->SetFileName( argv[1] );

  // try changing the spacing on the output to test the sorting on
  // physical size
  ChangeFilterType::ArrayType changeSpacing;
  changeSpacing[0] = 1;
  changeSpacing[1] = 0.5;
  change->SetInput( reader->GetOutput() );
  change->SetOutputSpacing(changeSpacing);
  change->ChangeSpacingOn();

  // Create a binary input image to label
  InternalPixelType threshold_low, threshold_hi;
  threshold_low = atoi( argv[3]);
  threshold_hi = atoi( argv[4]);

  threshold->SetInput (change->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::One);
  threshold->SetOutsideValue(itk::NumericTraits<InternalPixelType>::Zero);
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();

  // Label the components in the image and relabel them so that object
  // numbers increase as the size of the objects decrease.
  connected->SetInput (threshold->GetOutput());
  relabel->SetInput( connected->GetOutput() );

  // pull out the largest object
  finalThreshold->SetInput( relabel->GetOutput() );
  finalThreshold->SetLowerThreshold( 1 ); // object #1
  finalThreshold->SetUpperThreshold( 1 ); // object #1
  finalThreshold->SetInsideValue(255);
  finalThreshold->SetOutsideValue(itk::NumericTraits<WritePixelType>::Zero);
  
  try
    {
    writer->SetInput (finalThreshold->GetOutput());
    writer->SetFileName( argv[2] );
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  return 0;
}
