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
#include "itkMetaDataObject.h"
#include "itksys/SystemTools.hxx"
#include "itkNrrdImageIO.h"

/** This test was written in response to ITK task 2549
 * https://itk.icts.uiowa.edu/jira/browse/ITK-2549
 *
 * The complaint was that MetaData items were not getting
 * copied from an image to the NRRD file on disk.
 *
 * If this test succeeds, this bug has been fixed.
 */
int
itkNrrdMetaDataTest( int ac, char* av[] )
{

  if(ac < 2)
    {
    std::cerr << "Missing data directory argument" << std::endl;
    return EXIT_FAILURE;
    }

  // Image type
  typedef itk::Image<unsigned char,3> ImageType;
  // create dummy image
  ImageType::Pointer image1 = ImageType::New();
  ImageType::SizeType size = { {2,2,2} };
  image1->SetRegions(size);
  image1->Allocate();
  image1->FillBuffer(1);
  const char *metaDataObjectName = "NrrdTest";
  const char *metaDataObjectValue = "123456";
  // add a metadataobject to the dictionary
  itk::MetaDataDictionary &dict = image1->GetMetaDataDictionary();
  itk::EncapsulateMetaData<std::string>(dict,metaDataObjectName,metaDataObjectValue);

  // write the file then read it back in.
  typedef itk::ImageFileWriter<ImageType> ImageWriterType;
  typedef itk::ImageFileReader<ImageType> ImageReaderType;

  // test uses 1st arg to specify where to drop data
  std::string fname = av[1];
  fname += "/metadatatest.nrrd";
  // set up writer
  ImageWriterType::Pointer writer = ImageWriterType::New();
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->SetFileName(fname.c_str());
  writer->SetInput(image1);
  // set up reader
  ImageReaderType::Pointer reader = ImageReaderType::New();
  reader->SetFileName(fname.c_str());
  reader->SetImageIO(itk::NrrdImageIO::New());
  ImageType::Pointer image2;
  // write and then read
  try
    {
    writer->Update();
    reader->Update();
    image2 = reader->GetOutput();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "Exception in file reader or writer " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
    }
  // see if the test metaDataObject was copied to the output and is
  // present in the image read in.
  dict = image2->GetMetaDataDictionary();
  std::string NrrdTest;
  // if it exists and the string matches what we put in on the image
  // to write, AOK.
  if(itk::ExposeMetaData<std::string>(dict,metaDataObjectName,NrrdTest) != false &&
     NrrdTest == metaDataObjectValue)
    {
    return EXIT_SUCCESS;
    }
  // oops!
  return EXIT_FAILURE;
}
