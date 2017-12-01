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

#include "itkBruker2dseqImageIO.h"
#include "itkBruker2dseqImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMetaImageIO.h"
#include "itkTestingMacros.h"

int itkBrukerImageTest( int argc, char *argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
      << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
    }

  // Legacy compat with older MetaImages
  itk::MetaImageIO::SetDefaultDoublePrecision(6);
  std::cout << "Running Bruker2DSeq Test" << std::endl;

  typedef float                      PixelType;
  typedef itk::Image< PixelType, 3 > ImageType;

  itk::Bruker2dseqImageIOFactory::RegisterOneFactory();

  itk::Bruker2dseqImageIO::Pointer brukerImageIO =
    itk::Bruker2dseqImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( brukerImageIO, Bruker2dseqImageIO,
    ImageIOBase );

  const char* inputFilename = argv[1];
  bool canReadFile = brukerImageIO->CanReadFile( inputFilename );
  if( canReadFile )
    {
    typedef itk::ImageFileReader< ImageType > ReaderType;
    typedef itk::ImageFileWriter< ImageType > WriterType;
    ReaderType::Pointer reader = ReaderType::New();
    WriterType::Pointer writer = WriterType::New();
    reader->SetFileName( argv[1] );
    TRY_EXPECT_NO_EXCEPTION( reader->Update() );
    // Bruker has a lot of extraneous meta-data, get rid of it
    reader->GetOutput()->GetMetaDataDictionary().Clear();
    writer->SetFileName( argv[2] );
    writer->SetInput( reader->GetOutput() );
    TRY_EXPECT_NO_EXCEPTION( writer->Update() );
    }
  else
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Cannot read input file: " << inputFilename << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
