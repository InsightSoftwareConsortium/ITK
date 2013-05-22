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

#include "itkImage.h"
#include "itkLandweberDeconvolutionImageFilter.h"
#include "itkProjectedIterativeDeconvolutionImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkProjectedIterativeDeconvolutionImageFilterTest(int, char* [])
{
  // Declare the image type
  typedef itk::Image<float, 2> ImageType;

  // Declare the base deconvolution filter choice
  typedef itk::LandweberDeconvolutionImageFilter< ImageType >
    BaseDeconvolutionFilterType;

  // Declare a projected version of the base deconvolution image filter
  typedef itk::ProjectedIterativeDeconvolutionImageFilter< BaseDeconvolutionFilterType >
    ProjectedDeconvolutionFilterType;

  // Just instantiate the filter and print it
  ProjectedDeconvolutionFilterType::Pointer deconvolutionFilter =
    ProjectedDeconvolutionFilterType::New();
  deconvolutionFilter->Print( std::cout );

  itk::SimpleFilterWatcher watcher(deconvolutionFilter);

  return EXIT_SUCCESS;
}
