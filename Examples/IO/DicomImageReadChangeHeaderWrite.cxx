/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomImageReadChangeHeaderWrite.cxx
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

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a single DICOM slice and write it back
//  with some changed header information as another DICOM
//  slice. Header Key/Value pairs can be specified on the command
//  line. The keys are defined in Insight/Utilities/gdcm/Dicts/dicomV3.dic.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include "itkGDCMImageIO.h"
// Software Guide : EndCodeSnippet

#include <list>
#include <fstream>

int main(int argc, char* argv[])
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomImage OutputDicomImage Entry Value\n";
    return EXIT_FAILURE;
    }


  // Software Guide : BeginCodeSnippet
  typedef short InputPixelType;

  typedef itk::Image< InputPixelType, 2 > InputImageType;

  typedef itk::ImageFileReader< InputImageType > ReaderType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );
  
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

  // Change some header information
  typedef itk::MetaDataDictionary   DictionaryType;
  DictionaryType & dictionary = reader->GetOutput()->GetMetaDataDictionary();

  for (int i = 3; i < argc; i+=2)
    {
    std::string entryId( argv[i] );
    std::string value( argv[i+1] );
    itk::EncapsulateMetaData<std::string>( dictionary, entryId, value );
    }

  // Rewrite the image in DICOM format
  //
  typedef itk::ImageFileWriter< InputImageType >  Writer1Type;

  Writer1Type::Pointer writer1 = Writer1Type::New();

  writer1->SetFileName( argv[2] );
 
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

  return EXIT_SUCCESS;

}

