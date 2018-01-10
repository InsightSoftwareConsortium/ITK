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
#include "itkImage.h"
#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkNumericSeriesFileNames.h"
#include "itkChangeInformationImageFilter.h"

int main( int argc, char * argv [] )
{

  if( argc < 8 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " filenameExpression firstSliceValue lastSliceValue";
    std::cerr << " spacingX spacingY spacingZ outputImageFile " << std::endl;
    return EXIT_FAILURE;
    }



  typedef unsigned char                       PixelType;
  const unsigned int Dimension = 3;

  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageSeriesReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter<   ImageType >  WriterType;

  typedef itk::ChangeInformationImageFilter< ImageType > ChangerType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  ChangerType::Pointer changer = ChangerType::New();

  const unsigned int first = atoi( argv[2] );
  const unsigned int last  = atoi( argv[3] );

  ImageType::SpacingType spacing;
  spacing[0] = atof( argv[4] );
  spacing[1] = atof( argv[5] );
  spacing[2] = atof( argv[6] );

  changer->SetOutputSpacing( spacing );
  changer->ChangeSpacingOn();

  const char * outputFilename = argv[7];

  typedef itk::NumericSeriesFileNames    NameGeneratorType;

  NameGeneratorType::Pointer nameGenerator = NameGeneratorType::New();

  nameGenerator->SetSeriesFormat( argv[1] );

  nameGenerator->SetStartIndex( first );
  nameGenerator->SetEndIndex( last );
  nameGenerator->SetIncrementIndex( 1 );

  reader->SetFileNames( nameGenerator->GetFileNames()  );

  writer->SetFileName( outputFilename );
  changer->SetInput( reader->GetOutput() );
  writer->SetInput( changer->GetOutput() );

  writer->UseCompressionOn();

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
