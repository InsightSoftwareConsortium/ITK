/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMorphologicalWatershedImageFilterTest.cxx
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkSimpleFilterWatcher.h"
#include <itkIntensityWindowingImageFilter.h>
#include <itkMinimumMaximumImageCalculator.h>
#include "itkMorphologicalWatershedImageFilter.h"
#include "itkLabelOverlayImageFilter.h"

int itkMorphologicalWatershedImageFilterTest(int argc, char * argv[])
{
  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage MarkWatershedLine FullyConnected Level [OvelayOutput [Alpha]]" << std::endl;
    return EXIT_FAILURE;
    }
  const int dim = 2;
  
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::MorphologicalWatershedImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );

  // test default values
  if ( filter->GetMarkWatershedLine( ) != true )
    {
    std::cerr << "Wrong default MarkWatershedLine." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetFullyConnected( ) != false )
    {
    std::cerr << "Wrong default FullyConnected." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetLevel( ) != 0 )
    {
    std::cerr << "Wrong default Level." << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetMarkWatershedLine( atoi( argv[3] ) );
  if ( filter->GetMarkWatershedLine( ) != (bool)atoi(argv[3]) )
    {
    std::cerr << "Set/Get MarkWatershedLine problem." << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetFullyConnected( atoi( argv[4] ) );
  if ( filter->GetFullyConnected( ) != (bool)atoi(argv[4]) )
    {
    std::cerr << "Set/Get FullyConnected problem." << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetLevel( atoi( argv[5] ) );
  if ( filter->GetLevel( ) != atoi(argv[5]) )
    {
    std::cerr << "Set/Get Level problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // rescale the output to have a better display
  typedef itk::MinimumMaximumImageCalculator< IType > MaxCalculatorType;
  MaxCalculatorType::Pointer max = MaxCalculatorType::New();
  max->SetImage( filter->GetOutput() );
  max->Compute();

  typedef itk::IntensityWindowingImageFilter< IType, IType > RescaleType;
  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( filter->GetOutput() );
  rescale->SetWindowMinimum( itk::NumericTraits< PType >::Zero );
  rescale->SetWindowMaximum( max->GetMaximum() );
  rescale->SetOutputMaximum( itk::NumericTraits< PType >::max() );
  rescale->SetOutputMinimum( itk::NumericTraits< PType >::Zero );

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( argc > 6 )
    {
    typedef itk::RGBPixel<unsigned char>   RGBPixelType;
    typedef itk::Image<RGBPixelType, dim>    RGBImageType;
    
    typedef itk::LabelOverlayImageFilter<IType, IType, RGBImageType> OverlayType;
    OverlayType::Pointer overlay = OverlayType::New();
    overlay->SetInput( reader->GetOutput() );
    overlay->SetLabelImage( filter->GetOutput() );

    typedef itk::ImageFileWriter< RGBImageType > RGBWriterType;
    RGBWriterType::Pointer rgbwriter = RGBWriterType::New();
    rgbwriter->SetInput( overlay->GetOutput() );
    rgbwriter->SetFileName( argv[6] );

    if( argc > 7 )
      {
      overlay->SetOpacity( atof( argv[7] ) );
      }

    try
      {
      rgbwriter->Update();
      }
    catch ( itk::ExceptionObject & excp )
      {
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }

    }

  return EXIT_SUCCESS;

}
