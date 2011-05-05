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

#include <iostream>
#include "itkDecimateFramesVideoFilter.h"
#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkFileListVideoIOFactory.h"
#include "itkFileListVideoIO.h"
#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"

// typedefs
const unsigned int Dimension =                      2;
typedef unsigned char                               PixelType;
typedef itk::Image< PixelType, Dimension >          FrameType;
typedef itk::VideoStream< FrameType >               VideoType;
typedef itk::DecimateFramesVideoFilter< VideoType > FilterType;
typedef itk::VideoFileReader< VideoType >           ReaderType;
typedef itk::VideoFileWriter< VideoType >           WriterType;

namespace itk
{
namespace DecimateFramesVideoFilterTest
{

/**
 * Compare two images pixel by pixel
 */
bool FramesAreEqual(const FrameType* f1, const FrameType* f2)
{
  typedef ImageRegionConstIterator<FrameType> IterType;
  IterType it1(f1, f1->GetLargestPossibleRegion());
  IterType it2(f2, f2->GetLargestPossibleRegion());

  while (!it1.IsAtEnd())
    {
    if (it1.Get() != it2.Get())
      {
      return false;
      }
    ++it1;
    ++it2;
    }

  return true;
}

}
}


/**
 * Main test
 */
int itkDecimateFramesVideoFilterTest( int argc, char* argv[] )
{

  //////
  // Check Arguments
  //////
  if (argc < 3)
    {
    std::cout << "Usage: " << argv[0] << " input_file_string output_file_string" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Set up pipeline
  //////

  // Instantiate reader, writer, and filter
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  FilterType::Pointer filter = FilterType::New();

  // Connect the pipeline
  filter->SetInput(reader->GetOutput());
  writer->SetInput(filter->GetOutput());

  // Set the filenames on the reader/writer
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Keep every 3rd frame
  filter->SetPreservedFrameSpacing(3);


  //////
  // Run the pipeline
  //////

  // Register FileListIO with the factory -- shouldn't have to do this. Needs fixing
  itk::ObjectFactoryBase::RegisterFactory( itk::FileListVideoIOFactory::New() );

  // For the sake of debugging output, just use one thread
  filter->SetNumberOfThreads(1);

  // Update the writer to run everything
  writer->Update();


  //////
  // Check the results
  //////

  // Get the list of files for input and output
  std::vector<std::string> inputFiles = itk::FileListVideoIO::SplitFileNames(argv[1]);
  std::vector<std::string> outputFiles = itk::FileListVideoIO::SplitFileNames(argv[2]);

  // Set up two readers to read in the frames that should have been written and
  // compare against those that actually were
  typedef itk::ImageFileReader<FrameType> FrameReaderType;
  FrameReaderType::Pointer inputFrameReader = FrameReaderType::New();
  FrameReaderType::Pointer outputFrameReader = FrameReaderType::New();

  // Compare input frame 0 and output frame 0
  inputFrameReader->SetFileName(inputFiles[0]);
  inputFrameReader->Update();
  outputFrameReader->SetFileName(outputFiles[0]);
  outputFrameReader->Update();
  if (!itk::DecimateFramesVideoFilterTest::FramesAreEqual(
        inputFrameReader->GetOutput(), outputFrameReader->GetOutput()))
    {
    std::cerr << "Input frame 0 and output frame 0 don't match" << std::endl;
    return EXIT_FAILURE;
    }

  // Compare input frame 3 and output frame 1
  inputFrameReader->SetFileName(inputFiles[3]);
  inputFrameReader->Update();
  outputFrameReader->SetFileName(outputFiles[1]);
  outputFrameReader->Update();
  if (!itk::DecimateFramesVideoFilterTest::FramesAreEqual(
        inputFrameReader->GetOutput(), outputFrameReader->GetOutput()))
    {
    std::cerr << "Input frame 3 and output frame 1 don't match" << std::endl;
    return EXIT_FAILURE;
    }

  //////
  // Return successfully
  //////
  return EXIT_SUCCESS;
}
