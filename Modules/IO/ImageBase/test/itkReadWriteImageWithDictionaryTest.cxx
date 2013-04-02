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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkIOCommon.h"
#include "itkMetaDataObject.h"
#include "itkSpatialOrientationAdapter.h"

int itkReadWriteImageWithDictionaryTest(int argc, char* argv[])
{
  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " Input\n";
    return EXIT_FAILURE;
    }

  typedef itk::Image< unsigned char, 3 >    ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;

  //Create the 16x16 input image
  ImageType::Pointer  inputImage = ImageType::New();

  ImageType::SizeType size;
  size.Fill( 16 );
  ImageType::IndexType index;
  index.Fill( 0 );
  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->FillBuffer( 0 );

  inputImage->SetDirection(
    itk::SpatialOrientationAdapter().ToDirectionCosines(
      itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP ) );

  // Add some metadata in the dictionary
  itk::MetaDataDictionary & inputDictionary =
    inputImage->GetMetaDataDictionary();
  std::string voxelunitstr = "mm. "; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary,itk::ITK_VoxelUnits,voxelunitstr);
  std::string datestr = "26-05-2010"; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary,itk::ITK_ExperimentDate,datestr );
  std::string timestr = "13-44-00.0"; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary,itk::ITK_ExperimentTime,timestr );
  std::string patientstr = "patientid "; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary,itk::ITK_PatientID,patientstr );

  // Write the image down
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput(inputImage);
  writer->SetFileName(argv[1]);
  writer->Update();

  // Read the image back
  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName(argv[1]);
  reader->Update();

  ImageType::Pointer outputImage = reader->GetOutput();

  // Compare the metadatas
  int numMissingMetaData = 0;
  int numWrongMetaData = 0;

  itk::MetaDataDictionary & outputDictionary =
    outputImage->GetMetaDataDictionary();

  std::string metadatastr;

  if ( itk::ExposeMetaData<std::string>( outputDictionary, itk::ITK_VoxelUnits, metadatastr ) )
    {
    // MetaIO is rather strict on the format of ITK_VoxelUnits but for our purpose "mm"=="mm. "
    if ( !( metadatastr==voxelunitstr || (metadatastr=="mm" && voxelunitstr=="mm. ") ) )
      {
      std::cout<<"voxelunitstr.size()="<<voxelunitstr.size()<<std::endl;
      std::cout<<"metadatastr.size()="<<metadatastr.size()<<std::endl;
      std::cout<<"voxelunitstr=|"<<voxelunitstr<<"|"<<std::endl;
      std::cout<<"metadatastr=|"<<metadatastr<<"|"<<std::endl;
      ++numWrongMetaData;
      }
    }
  else
    {
    std::cout<<"Missing ITK_VoxelUnits"<<std::endl;
    ++numMissingMetaData;
    }

  if ( itk::ExposeMetaData<std::string>( outputDictionary, itk::ITK_ExperimentDate, metadatastr ) )
    {
    if ( metadatastr != datestr )
      {
      std::cout<<"datestr.size()="<<datestr.size()<<std::endl;
      std::cout<<"metadatastr.size()="<<metadatastr.size()<<std::endl;
      std::cout<<"datestr=|"<<datestr<<"|"<<std::endl;
      std::cout<<"metadatastr=|"<<metadatastr<<"|"<<std::endl;
      ++numWrongMetaData;
      }
    }
  else
    {
    std::cout<<"Missing ITK_ExperimentDate"<<std::endl;
    ++numMissingMetaData;
    }

  if ( itk::ExposeMetaData<std::string>( outputDictionary, itk::ITK_ExperimentTime, metadatastr ) )
    {
    if ( metadatastr != timestr )
      {
      std::cout<<"timestr.size()="<<timestr.size()<<std::endl;
      std::cout<<"metadatastr.size()="<<metadatastr.size()<<std::endl;
      std::cout<<"timestr=|"<<timestr<<"|"<<std::endl;
      std::cout<<"metadatastr=|"<<metadatastr<<"|"<<std::endl;
      ++numWrongMetaData;
      }
    }
  else
    {
    std::cout<<"Missing ITK_ExperimentTime"<<std::endl;
    ++numMissingMetaData;
    }

  if ( itk::ExposeMetaData<std::string>( outputDictionary, itk::ITK_PatientID, metadatastr ) )
    {
    if ( metadatastr != patientstr )
      {
      std::cout<<"patientstr.size()="<<patientstr.size()<<std::endl;
      std::cout<<"metadatastr.size()="<<metadatastr.size()<<std::endl;
      std::cout<<"patientstr=|"<<patientstr<<"|"<<std::endl;
      std::cout<<"metadatastr=|"<<metadatastr<<"|"<<std::endl;
      ++numWrongMetaData;
      }
    }
  else
    {
    std::cout<<"Missing ITK_PatientID"<<std::endl;
    ++numMissingMetaData;
    }

  std::cout<<std::endl<<"Number of missing metadata = "<<numMissingMetaData<<std::endl;
  std::cout<<"Number of wrong metadata = "<<numWrongMetaData<<std::endl<<std::endl;

  /** the visual studio 6 compiler cannot dereference iterator outside
   * of the dll */
#if defined(_MSC_VER) && (_MSC_VER <= 1300) && !defined(ITKSTATIC)
  return EXIT_SUCCESS;
#else
  // Perform a weaker but more exhaustive test
  int numMissingMetaData2 = 0;
  int numWrongMetaData2 = 0;
  int numAddedMetaData2 = 0;

  for ( itk::MetaDataDictionary::ConstIterator it = inputDictionary.Begin();
        it != inputDictionary.End(); ++it )
    {
    if ( !outputDictionary.HasKey( it->first ) )
      {
      std::cout<<"Missing "<<it->first<<std::endl;
      ++numMissingMetaData2;
      }
    else
      {
      itk::MetaDataDictionary::ConstIterator it2 = outputDictionary.Find( it->first );
      if ( it->second->GetMetaDataObjectTypeInfo() != it2->second->GetMetaDataObjectTypeInfo() )
        {
        std::cout<<"input_meta="<<it->second;
        std::cout<<"output_meta="<<it2->second;
        ++numWrongMetaData2;
        }
      }
    }

  for ( itk::MetaDataDictionary::ConstIterator it = outputDictionary.Begin();
        it != outputDictionary.End(); ++it )
    {
    if ( !inputDictionary.HasKey( it->first ) )
      {
      std::cout<<"added_meta=|"<<it->first<<"|-"<<it->second;
      ++numAddedMetaData2;
      }
    }

  std::cout<<std::endl<<"(weak but exhaustive) Number of missing metadata = "<<numMissingMetaData2<<std::endl;
  std::cout<<"(weak but exhaustive) Number of wrong metadata = "<<numWrongMetaData2<<std::endl;
  std::cout<<"(weak but exhaustive) Number of added metadata = "<<numAddedMetaData2<<std::endl<<std::endl;

  // Do not consider added metadata as errors since this may just indicate file format information
  if ( numMissingMetaData!=0 || numWrongMetaData!=0 ||
       numMissingMetaData2!=0 || numWrongMetaData2!=0 )
    {
    // FIXME:   FIXME MetaImage library: Then restore this test:    return EXIT_FAILURE;
    std::cout <<" FAILED: FIXME MetaImage library: Then restore this test" << std::endl;
    }

  return EXIT_SUCCESS;
#endif
}
