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

// This test is designed to test reading and writing of miss matched
// dimensions

int itkImageFileReaderDimensionsTest(int argc, char* argv[])
{

  if (argc < 4)
    {
    std::cout << "usage: itkIOTests itkImageFileReaderTest inputFileName outputDirectory outputExtension" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short, 2> Image2DType;
  typedef itk::Image<short, 3> Image3DType;
  typedef itk::Image<short, 4> Image4DType;
  typedef itk::Image<char, 2>  CharImage2DType;

  typedef itk::ImageFileReader<Image2DType>     Reader2DType;
  typedef itk::ImageFileReader<Image3DType>     Reader3DType;
  typedef itk::ImageFileReader<Image4DType>     Reader4DType;
  typedef itk::ImageFileReader<CharImage2DType> CharReader2DType;

  typedef itk::ImageFileWriter<Image3DType> Writer3DType;
  typedef itk::ImageFileWriter<Image4DType> Writer4DType;


  std::string tempFile1 = std::string( argv[2] ) + std::string( "/itkImageFileReaderDimensionsTest_1." ) + std::string( argv[3] );
  std::string tempFile2 = std::string( argv[2] ) + std::string( "/itkImageFileReaderDimensionsTest_2." ) + std::string( argv[3] );
  std::string tempFile3 = std::string( argv[2] ) + std::string( "/itkImageFileReaderDimensionsTest_3." ) + std::string( argv[3] );
  std::string tempFile4 = std::string( argv[2] ) + std::string( "/itkImageFileReaderDimensionsTest_4." ) + std::string( argv[3] );
  std::string tempFile5 = std::string( argv[2] ) + std::string( "/itkImageFileReaderDimensionsTest_5." ) + std::string( argv[3] );

  // we expect the filename to be 2 or 3 dimensions
  // and reading it into a 4D
  std::cout << "testing reading to 4D image" << std::endl;
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(argv[1]);

    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile1);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  // read the new 4D file into a 3D file
  std::cout << "testing reading from 4D image into 3D" << std::endl;
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    // we expect the filename to be 4 dimensions
    reader->SetFileName(tempFile1);


    Writer3DType::Pointer writer = Writer3DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile2);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  // stream read new 3D file into 4D
  std::cout << "testing requested stream reading from 3D image into 4D" << std::endl;
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(tempFile2);

    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile3);
    writer->SetNumberOfStreamDivisions(4);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  // stream read new 4D file into 3D
  std::cout << "testing requested stream reading from 4D image into 3D" << std::endl;
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    reader->SetFileName(tempFile3);
    reader->Update();

    Writer3DType::Pointer writer = Writer3DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile4);
    writer->SetNumberOfStreamDivisions(4);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  // read the new 4D file into a 2D file
  // we expect the first 2D slice to be read
  std::cout << "testing reading from 4D image into 2D" << std::endl;
  try
    {
    Reader2DType::Pointer reader = Reader2DType::New();
    // we expect the filename to be 4 dimensions
    reader->SetFileName(tempFile1);
    reader->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  int status = 1;
  // read the 4D file into a 4D image, then try to stream it as a 3D
  // IORegion
  std::cout << "testing requested invalid paste IORegion" << std::endl;
  try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileName(tempFile4);

    Image4DType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();

    // the dimension of this ioregion is an error, since it is one
    // less then the image file dimension
    itk::ImageIORegion ioregion(3);
    for (unsigned int i = 0; i < 3; ++i)
      {
      ioregion.SetIndex(i, 0);
      ioregion.SetSize(i, region.GetSize(i));
      }


    Writer4DType::Pointer writer = Writer4DType::New();
    writer->SetInput(reader->GetOutput());
    writer->SetFileName(tempFile5); // this file name should not
                                    // matter since it should never be written
    writer->SetIORegion(ioregion);
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    // this exception is expected since the ioregion should be invalid
    std::cout << "------------------ Caught expected exception!" << std::endl;
    std::cout << ex;
    status = 0;
    }
  if (status)
    {
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
    }

  // regression test of bug #10529
  // we expect the filename to be 2 or 3 dimensions
  // and reading it into a 4D
  std::cout << "testing reading to char 2D image" << std::endl;
  try
    {
    CharReader2DType::Pointer reader = CharReader2DType::New();
    reader->SetFileName(argv[1]);
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
