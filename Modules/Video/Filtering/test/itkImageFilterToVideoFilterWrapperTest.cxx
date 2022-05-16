/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkImageFilterToVideoFilterWrapper.h"
#include "itkImage.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkImageFileReader.h"
#include "itkFileListVideoIO.h"
#include "itkFileListVideoIOFactory.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"

/**
 * Main test
 */
int
itkImageFilterToVideoFilterWrapperTest(int argc, char * argv[])
{
  // Check parameters
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " input_video output_video" << std::endl;
    return EXIT_FAILURE;
  }

  // Get the lists of input and output files
  std::vector<std::string> inputFiles = itk::FileListVideoIO::SplitFileNames(argv[1]);
  std::vector<std::string> outputFiles = itk::FileListVideoIO::SplitFileNames(argv[2]);
  if (inputFiles.size() != outputFiles.size())
  {
    std::cerr << "Must specify the same number of input and output frames" << std::endl;
    return EXIT_FAILURE;
  }

  // Typedefs
  using PixelType = unsigned char;
  using FrameType = itk::Image<PixelType, 2>;
  using VideoType = itk::VideoStream<FrameType>;
  using GaussianImageFilterType = itk::RecursiveGaussianImageFilter<FrameType, FrameType>;
  using GaussianVideoFilterType = itk::ImageFilterToVideoFilterWrapper<GaussianImageFilterType>;
  using VideoReaderType = itk::VideoFileReader<VideoType>;
  using VideoWriterType = itk::VideoFileWriter<VideoType>;

  // Register FileListIO with the factory -- shouldn't have to do this. Needs fixing
  itk::ObjectFactoryBase::RegisterFactory(itk::FileListVideoIOFactory::New());

  // Set up reader and writer
  auto reader = VideoReaderType::New();
  auto writer = VideoWriterType::New();
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Instantiate a new video filter and an image filter
  auto imgGauss = GaussianImageFilterType::New();
  auto vidGauss = GaussianVideoFilterType::New();

  // Set the parameters on the image filter and plug it into the video filter
  imgGauss->SetSigma(3);
  vidGauss->SetImageFilter(imgGauss);

  // String the pipeline together
  vidGauss->SetInput(reader->GetOutput());
  writer->SetInput(vidGauss->GetOutput());

  // Run the pipeline
  writer->Update();

  //
  // Check output
  //
  using ImageReaderType = itk::ImageFileReader<FrameType>;
  using DifferenceFilterType = itk::Testing::ComparisonImageFilter<FrameType, FrameType>;
  auto imReader1 = ImageReaderType::New();
  auto imReader2 = ImageReaderType::New();
  auto differ = DifferenceFilterType::New();

  imgGauss->SetInput(imReader1->GetOutput());
  differ->SetValidInput(imgGauss->GetOutput());
  differ->SetTestInput(imReader2->GetOutput());

  for (unsigned int i = 0; i < inputFiles.size(); ++i)
  {
    imReader1->SetFileName(inputFiles[i]);
    imReader2->SetFileName(outputFiles[i]);
    differ->Update();
    if (itk::Math::NotAlmostEquals(differ->GetTotalDifference(), 0))
    {
      std::cerr << "Frame " << i << " didn't produce the correct output. Difference = " << differ->GetTotalDifference()
                << std::endl;
      return EXIT_FAILURE;
    }
  }

  //////
  // Return successfully
  //////
  return EXIT_SUCCESS;
}
