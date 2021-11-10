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

#include "itkImage.h"
#include "itkLandweberDeconvolutionImageFilter.h"
#include "itkProjectedIterativeDeconvolutionImageFilter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkObjectFactoryBase.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#  include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#endif

int
itkProjectedIterativeDeconvolutionImageFilterTest(int, char *[])
{
  // Declare the image type
  using ImageType = itk::Image<float, 2>;

#ifndef ITK_FFT_FACTORY_REGISTER_MANAGER // Manual factory registration is required for ITK FFT tests
#  if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
  itk::ObjectFactoryBase::RegisterInternalFactoryOnce<
    itk::FFTImageFilterFactory<itk::FFTWRealToHalfHermitianForwardFFTImageFilter>>();
  itk::ObjectFactoryBase::RegisterInternalFactoryOnce<
    itk::FFTImageFilterFactory<itk::FFTWHalfHermitianToRealInverseFFTImageFilter>>();
#  endif
  itk::ObjectFactoryBase::RegisterInternalFactoryOnce<
    itk::FFTImageFilterFactory<itk::VnlRealToHalfHermitianForwardFFTImageFilter>>();
  itk::ObjectFactoryBase::RegisterInternalFactoryOnce<
    itk::FFTImageFilterFactory<itk::VnlHalfHermitianToRealInverseFFTImageFilter>>();
#endif

  // Declare the base deconvolution filter choice
  using BaseDeconvolutionFilterType = itk::LandweberDeconvolutionImageFilter<ImageType>;

  // Declare a projected version of the base deconvolution image filter
  using ProjectedDeconvolutionFilterType = itk::ProjectedIterativeDeconvolutionImageFilter<BaseDeconvolutionFilterType>;

  // Just instantiate the filter and print it
  auto deconvolutionFilter = ProjectedDeconvolutionFilterType::New();
  deconvolutionFilter->Print(std::cout);

  itk::SimpleFilterWatcher watcher(deconvolutionFilter);

  return EXIT_SUCCESS;
}
