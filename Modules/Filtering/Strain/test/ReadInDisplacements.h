/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef __ReadInDisplacements_h
#define __ReadInDisplacements_h

#include "itkImageFileReader.h"

template <typename TDisplacementImageType>
int
ReadInDisplacements(const char * inputFile, typename TDisplacementImageType::Pointer & inputImage)
{
  using InputImageType = TDisplacementImageType;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputFile);

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << ex << std::endl;
    return EXIT_FAILURE;
  }

  inputImage = reader->GetOutput();

  return EXIT_SUCCESS;
}

#endif
