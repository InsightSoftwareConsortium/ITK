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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkThresholdImageFilter_h
#define itkThresholdImageFilter_h

#include "itkInPlaceImageFilter.h"

#include "itkConceptChecking.h"

namespace itk
{
/**
 *\class ThresholdImageFilter
 * \brief Set image values to a user-specified value if they are below,
 * above, or between simple threshold values.
 *
 * ThresholdImageFilter sets image values to a user-specified "outside"
 * value (by default, "black") if the image values are below, above, or
 * between simple threshold values.
 *
 * The available methods are:
 *
 * ThresholdAbove():
 * The values greater than the threshold value are set to OutsideValue
 *
 * ThresholdBelow():
 * The values less than the threshold value are set to OutsideValue
 *
 * ThresholdOutside():
 * The values outside the threshold range (less than lower or greater
 * than upper) are set to OutsideValue
 *
 * Note that these definitions indicate that pixels equal to the threshold
 * value are not set to OutsideValue in any of these methods
 *
 * The pixels must support the operators >= and <=.
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKThresholding
 *
 * \sphinx
 * \sphinxexample{Filtering/Thresholding/ThresholdAnImage,Threshold An Image}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ThresholdImageFilter : public InPlaceImageFilter<TImage, TImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ThresholdImageFilter);

  /** Standard class type aliases. */
  using Self = ThresholdImageFilter;
  using Superclass = InPlaceImageFilter<TImage, TImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdImageFilter, InPlaceImageFilter);

  /** Typedef to describe the type of pixel. */
  using PixelType = typename TImage::PixelType;

  /** The pixel type must support comparison operators. */
#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(PixelTypeComparableCheck, (Concept::Comparable<PixelType>));
  itkConceptMacro(PixelTypeOStreamWritableCheck, (Concept::OStreamWritable<PixelType>));
  // End concept checking
#endif

  /** Set the "outside" pixel value. The default value
   * NumericTraits<PixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, PixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, PixelType);

  /** The values greater than or equal to the value are set to OutsideValue. */
  void
  ThresholdAbove(const PixelType & thresh);

  /** The values less than or equal to the value are set to OutsideValue. */
  void
  ThresholdBelow(const PixelType & thresh);

  /** The values outside the range are set to OutsideValue. */
  void
  ThresholdOutside(const PixelType & lower, const PixelType & upper);

  /** Set/Get methods to set the lower threshold. */
  itkSetMacro(Lower, PixelType);
  itkGetConstMacro(Lower, PixelType);

  /** Set/Get methods to set the upper threshold. */
  itkSetMacro(Upper, PixelType);
  itkGetConstMacro(Upper, PixelType);

  /** Additional type alias for the input image. */
  using InputImageType = TImage;
  using InputImagePointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Additional type alias for the output image. */
  using OutputImageType = TImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

protected:
  ThresholdImageFilter();
  ~ThresholdImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** ThresholdImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData(). DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread".
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  PixelType m_OutsideValue;
  PixelType m_Lower;
  PixelType m_Upper;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkThresholdImageFilter.hxx"
#endif

#endif
