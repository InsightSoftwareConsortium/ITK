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

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a DICOM series into a volume
//  and then save this volume in another file format.
//
//  Software Guide : EndLatex 


#include "itkImageSeriesReader.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkImageFileWriter.h"

int main( int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomDirectory  outputFileName  [seriesName]" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  // The dimensionality of the ImageType is 3 for a conventional 3D image reader.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // GDCMImageIO is an ImageIO class for reading and writing DICOM v3 and 
  // ACR/NEMA images. It uses GDCM, an open source package developed by the 
  // Creatis team at INSA-Lyon \cite{CreatisINSA-Lyon}. 
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO                        ImageIOType;
  ImageIOType::Pointer dicomIO = ImageIOType::New();
  // Software Guide : EndCodeSnippet

  // Get the DICOM filenames from the directory
  // Software Guide : BeginLatex
  // GDCMSeriesFileNames generates a sequence of filenames for DICOM files 
  // for one study/series. The files pertinent to the study are contained
  // in the directory specified by the \code{SetInputDirectory()} method.
  // Software Guide : EndLatex
  // Software Guide : BeginCodeSnippet
  typedef itk::GDCMSeriesFileNames NamesGeneratorType;
  NamesGeneratorType::Pointer nameGenerator = NamesGeneratorType::New();
  nameGenerator->SetInputDirectory( argv[1] );
  // Software Guide : EndCodeSnippet
  

  try
    {
    // Software Guide : BeginLatex
    // The \code{GetInputFileNames()} method returns a vector container 
    // containing an ordered list of input file names in the specified 
    // directory.
    // Software Guide : EndLatex
      
    // Software Guide : BeginCodeSnippet
    typedef std::vector<std::string> fileNamesContainer;
    fileNamesContainer fileNames;
    fileNames = nameGenerator->GetInputFileNames();
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileNames( fileNames );
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
    // Now we will save the files in another specified file format.
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
      std::cout << ex;
      return EXIT_FAILURE;
      }
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
