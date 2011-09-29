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
#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int itkImageFileWriterUpdateLargestPossibleRegionTest(int argc, char* argv[])
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
    }

  // We remove the output file
  itksys::SystemTools::RemoveFile(argv[2]);

  typedef unsigned char             PixelType;
  typedef itk::Image<PixelType,2>   ImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  typedef itk::ImageFileWriter< ImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(reader->GetOutput());
  writer->SetFileName( argv[2] );

  ImageType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
  ImageType::IndexType index = region.GetIndex();
  ImageType::SizeType size = region.GetSize();

  itk::ImageIORegion ioregion(2);
  ioregion.SetIndex(0, index[0]);
  ioregion.SetIndex(1, index[1]);
  ioregion.SetSize(0, size[0]/2);
  ioregion.SetSize(1, size[1]/2);

  writer->SetIORegion(ioregion);

  // using Update() should fail because the paste feature is not supported by the png writer
  int status = 1;
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
    }
  if (status)
    {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
    }

  // but it should succeed with UpdateLargestPossibleRegion() because the paste region
  // is not used
  try
    {
    writer->UpdateLargestPossibleRegion();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
