/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelToRGBImageFilterTest.cxx
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
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"
#include "itkLabelToRGBImageFilter.h"


int itkLabelToRGBImageFilterTest(int argc, char * argv[])
{
  const int Dimension = 2;

  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " LabelImage OutputImage" << std::endl;
    return 1;
    }
 
  typedef unsigned char                               PixelType;
  typedef itk::Image< PixelType, Dimension >          ImageType;
  typedef itk::RGBPixel<unsigned char>                ColorPixelType;
  typedef itk::Image< ColorPixelType, Dimension >     ColorImageType;
  typedef itk::ImageFileReader< ImageType >           ReaderType;

  //Read in the input image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //Instantiate the filter
  typedef itk::LabelToRGBImageFilter< 
    ImageType, ColorImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Exercising Background Value methods
  filter->SetBackgroundValue( 10 );
  if( filter->GetBackgroundValue() != 10 )
    {
    std::cerr << "Background value Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }


  //Set the filter input and label images
  filter->SetInput( reader->GetOutput() );
  filter->SetBackgroundValue( 0 );
  
  itk::SimpleFilterWatcher watcher(filter, "filter");

  //Instantiate output image
  typedef itk::ImageFileWriter< ColorImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // exercise the methods to change the colors
  unsigned int numberOfColors1 = filter->GetNumberOfColors();
  filter->AddColor( 1, 255, 255 );

  unsigned int numberOfColors2 = filter->GetNumberOfColors();

  if( numberOfColors2 != numberOfColors1 + 1 )
    {
    std::cerr << "Error in GetNumberOfColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
    }

  filter->ResetColors();
  filter->AddColor( 255, 255, 255 );
   
  unsigned int numberOfColors3 = filter->GetNumberOfColors();

  if( numberOfColors3 != 1 )
    {
    std::cerr << "Error in GetNumberOfColors() or ResetColors() or AddColor() " << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
