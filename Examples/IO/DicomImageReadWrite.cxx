/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomImageReadWrite.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a single DICOM slice and write it back
//  as another DICOM slice. In the process an intensity rescaling is also
//  applied.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include "itkGDCMImageIO.h"
// Software Guide : EndCodeSnippet

#include <list>
#include <fstream>

int main(int ac, char* av[])
{

  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " DicomImage OutputDicomImage OutputImage RescalDicomImage\n";
    return EXIT_FAILURE;
    }


// Software Guide : BeginCodeSnippet
  typedef short InputPixelType;

  typedef itk::Image< InputPixelType, 2 > InputImageType;

  typedef itk::ImageFileReader< InputImageType > ReaderType;
// Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( av[1] );
  
// Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO        ImageIOType;

  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();

  reader->SetImageIO( gdcmImageIO );
// Software Guide : EndCodeSnippet

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  // Rewrite the image in DICOM format
  //
  typedef itk::ImageFileWriter< InputImageType >  Writer1Type;

  Writer1Type::Pointer writer1 = Writer1Type::New();

  writer1->SetFileName( av[2] );
 
  
  writer1->SetInput( reader->GetOutput() );
  
  writer1->SetImageIO( gdcmImageIO );

  try
    {
    writer1->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }



  // Rescale intensities and rewrite the image in another format
  //
  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
               InputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  
  typedef itk::ImageFileWriter< WriteImageType >  Writer2Type;

  Writer2Type::Pointer writer2 = Writer2Type::New();

  writer2->SetFileName( av[3] );
 
  rescaler->SetInput( reader->GetOutput() );
  writer2->SetInput( rescaler->GetOutput() );

  try
    {
    writer2->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }
 
  // Rewrite the image in DICOM format but using less bits per pixel
  //
  typedef itk::ImageFileWriter< WriteImageType >  Writer3Type;

  Writer3Type::Pointer writer3 = Writer3Type::New();

  writer3->SetFileName( av[4] );
 
  writer3->SetInput( rescaler->GetOutput() );
  writer3->UseInputMetaDataDictionaryOff ();
  writer3->SetImageIO( gdcmImageIO );

  try
    {
    writer3->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}

