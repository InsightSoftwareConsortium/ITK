/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

//====================================================================
//
//
//   WARNING WARNING WARNIG WARNING WARNING WARNIG !!!!
//   WARNING WARNING WARNIG WARNING WARNING WARNIG !!!!
//
//
//
//   The DICOM classes illustrated in this example are OBSOLETE and are
//   scheduled for being removed from the toolkit.
//
//   Please refer to DicomSeriesReadImageWrite2.cxx instead, where the GDCM
//   classes are used. GDCM classes are the ones currently recommended for
//   performing reading and writing of DICOM images.
//
//
//
//   WARNING WARNING WARNIG WARNING WARNING WARNIG !!!!
//   WARNING WARNING WARNIG WARNING WARNING WARNIG !!!!
//
//
//====================================================================

//  Software Guide : BeginLatex
//
//  Probably the most common representation of datasets in clinical
//  applications is the one that uses sets of DICOM slices in order to compose
//  tridimensional images. This is the case for CT, MRI and PET scanners. It is
//  very common therefore for image analysists to have to process volumetric
//  images that are stored in the form of a set of DICOM files belonging to a
//  common DICOM series.
//
//  The following example illustrates how to use ITK functionalities in order
//  to read a DICOM series into a volume and then save this volume in another
//  file format.
//
//  Software Guide : EndLatex


#include "itkImageSeriesReader.h"
#include "itkDICOMImageIO2.h"
#include "itkDICOMSeriesFileNames.h"
#include "itkImageFileWriter.h"

int main( int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " DicomDirectory  outputFileName  [seriesName]" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,3>                  ImageType;
  typedef itk::ImageSeriesReader< ImageType >  ReaderType;

  itk::DICOMImageIO2::Pointer dicomIO = itk::DICOMImageIO2::New();

  // Get the DICOM filenames from the directory
  itk::DICOMSeriesFileNames::Pointer nameGenerator = itk::DICOMSeriesFileNames::New();
  nameGenerator->SetDirectory( argv[1] );


  try
    {
    typedef std::vector<std::string> seriesIdContainer;
    const seriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();

    seriesIdContainer::const_iterator seriesItr = seriesUID.begin();
    seriesIdContainer::const_iterator seriesEnd = seriesUID.end();

    std::cout << std::endl << "The directory: " << std::endl;
    std::cout << std::endl << argv[1] << std::endl << std::endl;
    std::cout << "Contains the following DICOM Series: ";
    std::cout << std::endl << std::endl;

    while( seriesItr != seriesEnd )
      {
      std::cout << seriesItr->c_str() << std::endl;
      ++seriesItr;
      }

    std::cout << std::endl << std::endl;
    std::cout << "Now reading series: " << std::endl << std::endl;

    typedef std::vector<std::string> fileNamesContainer;
    fileNamesContainer fileNames;

    if( argc < 4 ) // If no optional third argument
      {
      std::cout << seriesUID.begin()->c_str() << std::endl;
      fileNames = nameGenerator->GetFileNames();
      }
    else
      {
      std::cout << argv[3] << std::endl;
      fileNames = nameGenerator->GetFileNames( argv[3] );
      }
    std::cout << std::endl << std::endl;

    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileNames( fileNames );
    reader->SetImageIO( dicomIO );

    try
      {
      reader->Update();
      }
    catch (itk::ExceptionObject &ex)
      {
      std::cout << ex << std::endl;
      return EXIT_FAILURE;
      }

    typedef itk::ImageFileWriter< ImageType > WriterType;
    WriterType::Pointer writer = WriterType::New();

    std::cout  << "Writing the image as " << std::endl << std::endl;
    std::cout  << argv[2] << std::endl << std::endl;

    writer->SetFileName( argv[2] );

    writer->SetInput( reader->GetOutput() );

    try
      {
      writer->Update();
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
