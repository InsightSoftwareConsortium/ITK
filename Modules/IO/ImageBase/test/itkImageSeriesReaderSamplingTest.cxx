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

#include "itkImageSeriesReader.h"

int
itkImageSeriesReaderSamplingTest(int ac, char * av[])
{

  if (ac < 3)
  {
    std::cerr << "usage: itkIOTests itkImageSeriesReaderSamplingTest inputFileName(s)" << std::endl;
    return EXIT_FAILURE;
  }


  using Image3DType = itk::Image<short, 3>;
  using Reader3DType = itk::ImageSeriesReader<Image3DType>;

  Reader3DType::FileNamesContainer fnames;
  for (int i = 1; i < ac; ++i)
  {
    std::cout << av[i] << std::endl;
    fnames.push_back(av[i]);
  }

  std::cout << "testing reading a series of 2D images to 3D with extra slices" << std::endl;
  try
  {
    Reader3DType::Pointer reader = Reader3DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    bool globalNonUniformSampling = false;
    if (itk::ExposeMetaData<bool>(
          reader->GetOutput()->GetMetaDataDictionary(), "ITK_non_uniform_sampling", globalNonUniformSampling) &&
        globalNonUniformSampling)
    {
      std::cout << "output ITK_non_uniform_sampling detected " << std::endl;
    }
    else
    {
      std::cout << "output ITK_non_uniform_sampling not found" << std::endl;
      return EXIT_FAILURE;
    }

    double globalSamplingDeviation = 0.0;
    if (itk::ExposeMetaData<double>(
          reader->GetOutput()->GetMetaDataDictionary(), "ITK_non_uniform_sampling_deviation", globalSamplingDeviation))
    {
      std::cout << "output ITK_non_uniform_sampling_deviation = " << globalSamplingDeviation << std::endl;
    }
    else
    {
      std::cout << "output ITK_non_uniform_sampling_deviation not found" << std::endl;
      return EXIT_FAILURE;
    }

    // iterate over all slices to detect offending slice
    for (auto d : *reader->GetMetaDataDictionaryArray())
    {
      itk::MetaDataDictionary theMetadata = *d;
      bool                    nonUniformSampling = false;
      if (itk::ExposeMetaData<bool>(theMetadata, "ITK_non_uniform_sampling", nonUniformSampling) && nonUniformSampling)
      {
        std::cout << "slice ITK_non_uniform_sampling detected" << std::endl;
      }
      else
      {
        std::cout << "slice ITK_non_uniform_sampling not detected" << std::endl;
      }
    }
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cout << ex;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
