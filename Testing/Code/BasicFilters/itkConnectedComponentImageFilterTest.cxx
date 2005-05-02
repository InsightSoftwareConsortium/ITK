/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedComponentImageFilterTest.cxx
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
#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkFilterWatcher.h"
#include "itkImageRegionIterator.h"
#include "vnl/vnl_sample.h"

int itkConnectedComponentImageFilterTest(int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage threshold_low threshold_hi [fully_connected]" << std::endl;
    return 1;
    }

  typedef   unsigned short  InternalPixelType;
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  typedef itk::Image<unsigned short,Dimension> OutputImageType;

  typedef itk::RGBPixel<unsigned char>   RGBPixelType;
  typedef itk::Image<RGBPixelType, Dimension>    RGBImageType;

  typedef itk::ImageFileReader< InternalImageType > ReaderType;
  typedef itk::ImageFileWriter<  RGBImageType  > WriterType;

  
  typedef itk::BinaryThresholdImageFilter< InternalImageType, InternalImageType > ThresholdFilterType;
  typedef itk::ConnectedComponentImageFilter< InternalImageType, OutputImageType > FilterType;
  typedef itk::RelabelComponentImageFilter< OutputImageType, OutputImageType > RelabelType;

  
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();
  FilterType::Pointer filter = FilterType::New();
  RelabelType::Pointer relabel = RelabelType::New();
  
  FilterWatcher watcher(filter);
  watcher.QuietOn();

  reader->SetFileName( argv[1] );

  InternalPixelType threshold_low, threshold_hi;
  threshold_low = atoi( argv[3]);
  threshold_hi = atoi( argv[4]);

  threshold->SetInput (reader->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::One);
  threshold->SetOutsideValue(itk::NumericTraits<InternalPixelType>::Zero);
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();
  
  filter->SetInput (threshold->GetOutput());
  if (argc > 5)
    {
    int fullyConnected = atoi( argv[5] );
    filter->SetFullyConnected( fullyConnected );
    }
  relabel->SetInput( filter->GetOutput() );
  relabel->SetMinimumObjectSize(100000);
  
  try
    {
    relabel->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Relabel: exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  // Remap the labels to viewable colors
  RGBImageType::Pointer colored = RGBImageType::New();
  colored->SetRegions( filter->GetOutput()->GetBufferedRegion() );
  colored->Allocate();

  unsigned short numObjects = relabel->GetNumberOfObjects();
  
  std::vector<RGBPixelType> colormap;
  RGBPixelType px;
  colormap.resize( numObjects+1 );
  vnl_sample_reseed( 1031571 );
  for (unsigned short i=0; i < colormap.size(); ++i)
    {
    px.SetRed(
      static_cast<unsigned char>(255*vnl_sample_uniform( 0.3333, 1.0 ) ));
    px.SetGreen(
      static_cast<unsigned char>(255*vnl_sample_uniform( 0.3333, 1.0 ) ));
    px.SetBlue(
      static_cast<unsigned char>(255*vnl_sample_uniform( 0.3333, 1.0 ) ));

    colormap[i] = px;
    }
  
  itk::ImageRegionIterator<OutputImageType>
    it(relabel->GetOutput(), relabel->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<RGBImageType> cit(colored,
                                             colored->GetBufferedRegion());
  
  while( !it.IsAtEnd() )
    {
    if (it.Get() == 0)
      {
      cit.Set(RGBPixelType());
      }
    else
      {
      cit.Set( colormap[it.Get()] );
      }
    ++it;
    ++cit;
    }
  
  try
    {
    writer->SetInput (colored);
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
