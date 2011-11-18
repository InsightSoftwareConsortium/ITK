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

#include "itkGDCMImageIO.h"
#include "itkNumericSeriesFileNames.h"
#include "itkImageSeriesWriter.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkImageSeriesReader.h"


int itkImageReadDICOMSeriesWriteTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << "InputImage  OutputDicomDirectory" << std::endl;
    return EXIT_FAILURE;
    }


  typedef signed short    PixelType;
  const unsigned int      Dimension = 3;

  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

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


  typedef itk::GDCMImageIO                        ImageIOType;
  typedef itk::NumericSeriesFileNames             NamesGeneratorType;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();

  const char * outputDirectory = argv[2];

  itksys::SystemTools::MakeDirectory( outputDirectory );


  typedef signed short    OutputPixelType;
  const unsigned int      OutputDimension = 2;

  typedef itk::Image< OutputPixelType, OutputDimension >    Image2DType;

  typedef itk::ImageSeriesWriter<
                         ImageType, Image2DType >  SeriesWriterType;

  NamesGeneratorType::Pointer namesGenerator = NamesGeneratorType::New();

  itk::MetaDataDictionary & dict = gdcmIO->GetMetaDataDictionary();
  std::string tagkey, value;
  tagkey = "0008|0060"; // Modality
  value = "MR";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value );
  tagkey = "0008|0008"; // Image Type
  value = "DERIVED\\SECONDARY";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0008|0064"; // Conversion Type
  value = "DV";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);


  SeriesWriterType::Pointer seriesWriter = SeriesWriterType::New();

  seriesWriter->SetInput( reader->GetOutput() );
  seriesWriter->SetImageIO( gdcmIO );


  ImageType::RegionType region =
     reader->GetOutput()->GetLargestPossibleRegion();

  ImageType::IndexType start = region.GetIndex();
  ImageType::SizeType  size  = region.GetSize();


  std::string format = outputDirectory;

  format += "/image%03d.dcm";

  namesGenerator->SetSeriesFormat( format.c_str() );

  namesGenerator->SetStartIndex( start[2] );
  namesGenerator->SetEndIndex( start[2] + size[2] - 1 );
  namesGenerator->SetIncrementIndex( 1 );


  seriesWriter->SetFileNames( namesGenerator->GetFileNames() );


  try
    {
    seriesWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while writing the series " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Now, verify that we can read the slices back and
  // reconstruct the exact same image.
  //
  typedef itk::ImageSeriesReader< ImageType >     SeriesReaderType;
  SeriesReaderType::Pointer seriesReader = SeriesReaderType::New();

  //
  // Create a new GDCMImageIO instance, to make sure that
  // the read process can be done independently of the writing.
  //
  seriesReader->SetImageIO( ImageIOType::New() );

  typedef itk::GDCMSeriesFileNames DicomNamesGeneratorType;
  DicomNamesGeneratorType::Pointer nameGenerator =
    DicomNamesGeneratorType::New();

  nameGenerator->SetUseSeriesDetails( true );
  nameGenerator->AddSeriesRestriction("0008|0021" );

  nameGenerator->SetDirectory( argv[2] );

  typedef std::vector< std::string >   FileNamesContainer;
  FileNamesContainer fileNames;

  typedef std::vector< std::string >    SeriesIdContainer;

  const SeriesIdContainer & seriesUID = nameGenerator->GetSeriesUIDs();

  std::string seriesIdentifier = seriesUID.begin()->c_str();

  fileNames = nameGenerator->GetFileNames( seriesIdentifier );

  seriesReader->SetFileNames( fileNames );

  try
    {
    seriesReader->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::ImageRegionConstIterator< ImageType > IteratorType;

  const ImageType * originalImage  = reader->GetOutput();
  const ImageType * recoveredImage = seriesReader->GetOutput();

  if( originalImage->GetOrigin() != recoveredImage->GetOrigin() )
    {
    std::cerr << "ERROR: Origin values do not match" << std::endl;
    std::cerr << "Original  image origin = " << originalImage->GetOrigin() << std::endl;
    std::cerr << "Recovered image origin = " << recoveredImage->GetOrigin() << std::endl;
    }

  if( originalImage->GetSpacing() != recoveredImage->GetSpacing() )
    {
    std::cerr << "ERROR: Spacing values do not match" << std::endl;
    std::cerr << "Original  image spacing = " << originalImage->GetSpacing() << std::endl;
    std::cerr << "Recovered image spacing = " << recoveredImage->GetSpacing() << std::endl;
    }

  IteratorType itr1( originalImage, region );
  IteratorType itr2( recoveredImage, region );

  itr1.GoToBegin();
  itr2.GoToBegin();

  while( !itr1.IsAtEnd() )
    {
    if( itr1.Get() != itr2.Get() )
      {
      std::cerr << "ERROR: found pixel value difference at index ";
      std::cerr << itr1.GetIndex() << std::endl;
      std::cerr << "Original  pixel value = " << itr1.Get() << std::endl;
      std::cerr << "Recovered pixel value = " << itr2.Get() << std::endl;
      return EXIT_FAILURE;
      }
    ++itr1;
    ++itr2;
    }

  return EXIT_SUCCESS;
}
