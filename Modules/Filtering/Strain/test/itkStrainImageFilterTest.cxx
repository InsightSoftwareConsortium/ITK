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

#include "itkStrainImageFilter.h"

#include "ReadInDisplacements.h"
#include "WriteOutStrains.h"

int
itkStrainImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputDisplacementImage outputPrefix ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputDisplacementImageFileName = argv[1];
  const char * outputFileNamePrefix = argv[2];

  const unsigned int                                    Dimension = 2;
  typedef float                                         PixelType;
  typedef itk::Vector<PixelType, Dimension>             DisplacementVectorType;
  typedef itk::Image<DisplacementVectorType, Dimension> InputImageType;

  typedef itk::StrainImageFilter<InputImageType, PixelType, PixelType> FilterType;
  typedef FilterType::OutputImageType                                  TensorImageType;

  FilterType::Pointer filter = FilterType::New();

  InputImageType::Pointer inputDisplacements;
  if (ReadInDisplacements<InputImageType>(inputDisplacementImageFileName, inputDisplacements) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  filter->SetInput(inputDisplacements);
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

  if (WriteOutStrains<PixelType, Dimension, TensorImageType>(outputFileNamePrefix, filter->GetOutput()) == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
