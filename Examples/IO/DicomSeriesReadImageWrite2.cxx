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
//  Probably the most common representation of datasets in clinical
//  applications is the one that uses sets of DICOM slices in order to compose
//  tridimensional images. This is the case for CT, MRI and PET scanners. It is
//  very common therefore for image analysts to have to process volumetric
//  images that are stored in the form of a set of DICOM files belonging to a
//  common DICOM series. 
//
//  The following example illustrates how to use ITK functionalities in order
//  to read a DICOM series into a volume and then save this volume in another
//  file format.
//
//  The example begins by including the appropriate headers. In particular we
//  will need the GDCMImageIO object in order to have access to the
//  capabilities of the GDCM library for reading DICOM files, and the
//  GDCMSeriesFileNames object that is capable of generating the lists of
//  filenames identifying the slices of a common volumetric dataset.
//
//  \index{itk::ImageSeriesReader!header}
//  \index{itk::GDCMImageIO!header}
//  \index{itk::GDCMSeriesFileNames!header}
//  \index{itk::ImageFileWriter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkImageSeriesReader.h"
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
// 
// We define the pixel type and dimension of the image to be read. In this
// particular case, the dimensionality of the image is 3, and we assume a
// \code{signed short} pixel type that is commonly used for X-Rays CT scanners.
// 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef signed short    PixelType;
  const unsigned int      Dimension = 3;

  typedef itk::Image< PixelType, Dimension >         ImageType;
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
// 
// We use the image type for instantiating the type of the series reader and
// for constructing one object of its type.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageSeriesReader< ImageType >        ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
// 
// A GDCMImageIO object is created and connected to the reader. This object is
// the one that is aware of the internal intricacies of the DICOM format. 
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO       ImageIOType;
  ImageIOType::Pointer dicomIO = ImageIOType::New();
  
  reader->SetImageIO( dicomIO );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// Now we face one of the main challenges of the process of reading a DICOM
// series, that is, to identify from a given directory the set of filenames
// that belong together to the same volumetric image. Fortunately for us, GDCM
// offers a powerful answer to this problem and we just need to invoke that
// functionality through an ITK class that encapsulates a communication with
// GDCM classes. This ITK object is the GDCMSeriesFileNames. Conveniently for
// us, we only need to pass to this class the name of the directory where the
// DICOM slices are stored. This is done with the \code{SetInputDirectory()}
// method. The GDCMSeriesFileNames object will explore the directory and will
// generate a sequence of filenames for DICOM files for one study/series. 
//
// \index{itk::GDCMSeriesFileNames!SetInputDirectory()}
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


    
// Software Guide : BeginLatex
// 
// The GDCMSeriesFileNames object first identifies the list of DICOM series
// that are present in the given directory. We receive that list in a reference
// to a container of strings and then we can do things like printing out all
// the series identifiers that the generator had found. Since the process of
// finding the series identifiers can potentially throw exceptions, it is
// recommended that you put this code inside a try/catch block.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
    typedef std::vector<std::string> seriesIdContainer;
    
    const seriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();
    
    seriesIdContainer::const_iterator seriesItr = seriesUID.begin();
    seriesIdContainer::const_iterator seriesEnd = seriesUID.end();
    while( seriesItr != seriesEnd )
      {
      std::cout << seriesItr->c_str() << std::endl;
      seriesItr++;
      }
// Software Guide : EndCodeSnippet
  

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


    reader->SetFileNames( fileNames );

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
