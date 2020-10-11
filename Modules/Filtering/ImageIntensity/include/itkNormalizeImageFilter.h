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
#ifndef itkNormalizeImageFilter_h
#define itkNormalizeImageFilter_h

#include "itkStatisticsImageFilter.h"
#include "itkShiftScaleImageFilter.h"

#include "itkEventObject.h"

namespace itk
{
/**
 *\class NormalizeImageFilter
 * \brief Normalize an image by setting its mean to zero and variance to one.
 *
 * NormalizeImageFilter shifts and scales an image so that the pixels
 * in the image have a zero mean and unit variance. This filter uses
 * StatisticsImageFilter to compute the mean and variance of the input
 * and then applies ShiftScaleImageFilter to shift and scale the pixels.
 *
 * NB: since this filter normalizes the data such that the mean is at 0, and
 * \f$-\sigma\f$ to \f$+\sigma\f$ is mapped to -1.0 to 1.0,
 * output image integral types will produce an image that DOES NOT HAVE
 * a unit variance due to 68% of the intensity values being mapped to the
 * real number range of -1.0 to 1.0 and then cast to the output
 * integral value.
 *
 * \sa NormalizeToConstantImageFilter
 *
 * \ingroup MathematicalImageFilters
 * \ingroup ITKImageIntensity
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageIntensity/NormalizeImage,Normalize Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT NormalizeImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NormalizeImageFilter);

  /** Standard Self type alias */
  using Self = NormalizeImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(NormalizeImageFilter, ImageToImageFilter);

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** NormalizeImageFilter must call modified on its internal filters */
  void
  Modified() const override;

protected:
  NormalizeImageFilter();

  /** GenerateData. */
  void
  GenerateData() override;

  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

private:
  typename StatisticsImageFilter<TInputImage>::Pointer m_StatisticsFilter;

  typename ShiftScaleImageFilter<TInputImage, TOutputImage>::Pointer m_ShiftScaleFilter;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNormalizeImageFilter.hxx"
#endif

#endif
