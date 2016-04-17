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

//
//  This example illustrates how to read a DICOM series into a volume
//  and then save this volume into another DICOM series using the
//  exact same name.
//  It makes use of the GDCM library
//

#define ITK_LEGACY_TEST
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"

int itkGDCMSeriesMissingDicomTagTest( int argc, char* argv[] )
{
  if( argc < 2)
    {
    std::cerr << "Usage: " << argv[0] <<
      " DicomDirectory" << std::endl;
    return EXIT_FAILURE;
    }

#if ! defined ( ITK_LEGACY_REMOVE )
  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  typedef itk::GDCMImageIO                        ImageIOType;
  typedef itk::GDCMSeriesFileNames                SeriesFileNames;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();
  SeriesFileNames::Pointer it = SeriesFileNames::New();

  // Get the DICOM filenames from the directory
  // First add a restriction *before* selecting the input directory
  // since SetInputDirectory has a side effect of executing
  gdcm::SerieHelper *sh = it->GetSeriesHelper( );
  sh->AddRestriction(0x0010, 0x0010, "Wes Turner", gdcm::GDCM_EQUAL);
  sh->AddRestriction(0x0020, 0x0013, "75", gdcm::GDCM_GREATEROREQUAL);
  sh->AddRestriction(0x0020, 0x0013, "77", gdcm::GDCM_LESSOREQUAL);
  it->SetInputDirectory( argv[1] );

  ReaderType::Pointer reader = ReaderType::New();

  const ReaderType::FileNamesContainer & fileNames = it->GetInputFileNames();
  const size_t numberOfFileNames = fileNames.size();
  std::cout << numberOfFileNames << std::endl;
  for(unsigned int fni = 0; fni < numberOfFileNames; ++fni)
    {
    std::cout << "filename # " << fni << " = ";
    std::cout << fileNames[fni] << std::endl;
    }

  reader->SetFileNames( fileNames );
  reader->SetImageIO( gdcmIO );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;

    return EXIT_FAILURE;
    }

  ReaderType::DictionaryArrayRawPointer dictArray =
    reader->GetMetaDataDictionaryArray();
  unsigned dictSize = static_cast<unsigned>( dictArray->size() );
  if(dictSize != 2)
    {
    std::cerr << "Expected 2 elements in MetaDataDictionary array, found "
              << dictArray->size() << std::endl;
    return EXIT_FAILURE;
    }
  // This key artificially removed from the second file in the series.
  std::string missingKey("0008|0033");
  std::string val;
  if(!itk::ExposeMetaData<std::string>( *((*dictArray)[0]) ,missingKey, val ))
    {
    std::cerr << "Missing data object " << missingKey
              << "in first slice" << std::endl;
    return EXIT_FAILURE;
    }
  if(itk::ExposeMetaData<std::string>( *((*dictArray)[1]) ,missingKey, val ))
    {
    std::cerr << "DataObject " << missingKey
              << "found in second slice where it should be missing" << std::endl;
    return EXIT_FAILURE;
    }
#endif

  return EXIT_SUCCESS;
}
