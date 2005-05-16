/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomSeriesReadImageWrite2.cxx
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
//  This example illustrates how to read a DICOM series into a volume
//  and then save this volume in another file format.
//
//  The example begins by including the appropriate headers.
//
//  \index{itk::ImageSeriesReader!header}
//  \index{itk::GDCMImageIO!header}
//  \index{itk::GDCMSeriesFileNames!header}
//  \index{itk::ImageFileWriter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageSeriesReader.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkImageFileWriter.h"
// Software Guide : EndCodeSnippet

int main( int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " DicomDirectory  outputFileName  [seriesName]" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  // The dimensionality of the ImageType is 3 for a conventional 3D image reader.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO                        ImageIOType;
  ImageIOType::Pointer dicomIO = ImageIOType::New();
  // Software Guide : EndCodeSnippet

  // Get the DICOM filenames from the directory
  // Software Guide : BeginLatex
  //
  // GDCMSeriesFileNames generates a sequence of filenames for DICOM files 
  // for one study/series. The files pertinent to the study are contained
  // in the directory specified by the \code{SetInputDirectory()} method.
  //
  //  \index{itk::GDCMSeriesFileNames!SetInputDirectory()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GDCMSeriesFileNames NamesGeneratorType;
  NamesGeneratorType::Pointer nameGenerator = NamesGeneratorType::New();
  nameGenerator->SetDirectory( argv[1] );
  // Software Guide : EndCodeSnippet
  

  try
    {
    std::cout << std::endl << "The directory: " << std::endl;
    std::cout << std::endl << argv[1] << std::endl << std::endl;
    std::cout << "Contains the following DICOM Series: ";
    std::cout << std::endl << std::endl;
    typedef std::vector<std::string> seriesIdContainer;
    const seriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();
    seriesIdContainer::const_iterator seriesItr = seriesUID.begin();
    seriesIdContainer::const_iterator seriesEnd = seriesUID.end();
    while( seriesItr != seriesEnd )
      {
      std::cout << seriesItr->c_str() << std::endl;
      seriesItr++;
      }
  
    std::cout << std::endl << std::endl;
    std::cout << "Now reading series: " << std::endl << std::endl;
    typedef std::vector<std::string> fileNamesContainer;
    fileNamesContainer fileNames;

    // Software Guide : BeginLatex
    // The \code{GetInputFileNames()} method returns a vector container 
    // containing an ordered list of input file names in the specified 
    // directory. Which are then pass to the \doxygen{ImageSeriesReader} using SetFileNames
    //  
    //  \index{itk::GDCMSeriesFileNames!GetInputFileNames()}
    //  \index{itk::ImageSeriesReader!SetFileNames()}
    //
    // Software Guide : EndLatex
      
    if( argc < 4 ) // If no optional third argument
      {
      std::cout << seriesUID.begin()->c_str() << std::endl;
      fileNames = nameGenerator->GetFileNames(seriesUID.begin()->c_str());
      }
    else
      {
      std::cout << argv[3] << std::endl;
      fileNames = nameGenerator->GetFileNames( argv[3] );
      }
    std::cout << std::endl << std::endl;
    // Software Guide : BeginCodeSnippet
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileNames( fileNames );
    // Software Guide : EndCodeSnippet

    // Software Guide : BeginLatex
    // Eventhough we were using a GDCM specific class: GDCMSeriesFileNames, we still need
    // to explicitely specify the GDCMImageIO.
    // Software Guide : EndLatex
    // Software Guide : BeginCodeSnippet
    reader->SetImageIO( dicomIO );
    // Software Guide : EndCodeSnippet

    try
      {
      // Software Guide : BeginCodeSnippet
      reader->Update();
      // Software Guide : EndCodeSnippet
      }
    catch (itk::ExceptionObject &ex)
      {
      std::cout << ex << std::endl;
      return EXIT_FAILURE;
      }


    // Software Guide : BeginLatex
    // Now we will save the files in another user specified file format. Only the file extension
    // is needed in this case thanks to the ImageIO factory.
    // Software Guide : EndLatex
    // Software Guide : BeginCodeSnippet    
    typedef itk::ImageFileWriter< ImageType > WriterType;
    WriterType::Pointer writer = WriterType::New();
    // Software Guide : EndCodeSnippet    

    std::cout  << "Writing the image as " << std::endl << std::endl;
    std::cout  << argv[2] << std::endl << std::endl;

    // Software Guide : BeginCodeSnippet    
    writer->SetFileName( argv[2] );
    writer->SetInput( reader->GetOutput() );
    // Software Guide : EndCodeSnippet    

    try
      {
      // Software Guide : BeginCodeSnippet    
      writer->Update();
      // Software Guide : EndCodeSnippet    
      }
    catch (itk::ExceptionObject &ex)
      {
      std::cout << ex << std::endl;
      return EXIT_FAILURE;
      }
    }
  catch (itk::ExceptionObject &ex)
    {
      std::cout << ex << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
