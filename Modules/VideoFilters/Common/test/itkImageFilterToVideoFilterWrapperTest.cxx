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
#include "itkImageFilterToVideoFilterWrapper.h"
#include "itkImage.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkVideoFileReader.h"
#include "itkVideoFileWriter.h"
#include "itkFileListVideoIOFactory.h"

/**
 * Main test
 */
int itkImageFilterToVideoFilterWrapperTest( int argc, char* argv[] )
{
  // Check parameters
  if (argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " input_video output_video" << std::endl;
    return EXIT_FAILURE;
    }

  // Typedefs
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> FrameType;
  typedef itk::VideoStream< FrameType > VideoType;
  typedef itk::RecursiveGaussianImageFilter< FrameType, FrameType > GaussianImageFilterType;
  typedef itk::ImageFilterToVideoFilterWrapper< GaussianImageFilterType >
    GaussianVideoFilterType;
  typedef itk::VideoFileReader< VideoType > VideoReaderType;
  typedef itk::VideoFileWriter< VideoType > VideoWriterType;

  // Register FileListIO with the factory -- shouldn't have to do this. Needs fixing
  itk::ObjectFactoryBase::RegisterFactory( itk::FileListVideoIOFactory::New() );

  // Set up reader and writer
  VideoReaderType::Pointer reader = VideoReaderType::New();
  VideoWriterType::Pointer writer = VideoWriterType::New();
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Instantiate a new video filter and an image filter
  GaussianImageFilterType::Pointer imgGauss = GaussianImageFilterType::New();
  GaussianVideoFilterType::Pointer vidGauss = GaussianVideoFilterType::New();

  // Set the parameters on the image filter and plug it into the video filter
  imgGauss->SetSigma(3);
  vidGauss->SetImageFilter(imgGauss);

  // String the pipeline together
  vidGauss->SetInput(reader->GetOutput());
  writer->SetInput(vidGauss->GetOutput());

  // Run the pipeline
  writer->Update();

  //////
  // Return successfully
  //////
  return EXIT_SUCCESS;
}
