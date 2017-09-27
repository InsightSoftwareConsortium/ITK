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
//  It makes use of the DCMTK library
//

#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkDCMTKImageIO.h"
#include "itkDCMTKSeriesFileNames.h"

int itkDCMTKSeriesReadImageWrite( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] <<
      " DicomDirectory  outputFile" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  typedef itk::DCMTKImageIO                       ImageIOType;
  typedef itk::DCMTKSeriesFileNames               SeriesFileNames;

  ImageIOType::Pointer dcmtkIO = ImageIOType::New();
  SeriesFileNames::Pointer it = SeriesFileNames::New();

  it->SetInputDirectory( argv[1] );

  ReaderType::Pointer reader = ReaderType::New();

  const ReaderType::FileNamesContainer & fileNames = it->GetInputFileNames();
  const unsigned int numberOfFileNames =  fileNames.size();
  std::cout << numberOfFileNames << std::endl;
  for(unsigned int fni = 0; fni < numberOfFileNames; ++fni)
    {
    std::cout << "filename # " << fni << " = ";
    std::cout << fileNames[fni] << std::endl;
    }

  reader->SetFileNames( fileNames );
  reader->SetImageIO( dcmtkIO );

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

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->SetInput( reader->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;

    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
