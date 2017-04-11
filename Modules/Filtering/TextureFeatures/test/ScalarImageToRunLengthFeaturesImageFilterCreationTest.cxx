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
#include "itkScalarImageToRunLengthFeaturesImageFilter.h"

#include "itkImage.h"
#include "itkVector.h"
#include "itkImageFileReader.h"

int
ScalarImageToRunLengthFeaturesImageFilterCreationTest(int argc, char * argv[])
{
  // Setup types
  typedef itk::Image<int, 3>                    InputImageType;
  typedef itk::Image<itk::Vector<float, 10>, 3> OutputImageType;
  typedef itk::ImageFileReader<InputImageType>  readerType;

  // Apply the filter
  typedef itk::Statistics::ScalarImageToRunLengthFeaturesImageFilter<InputImageType, OutputImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  return EXIT_SUCCESS;
}
