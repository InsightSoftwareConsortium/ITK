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
#ifndef itkButterworthFilterFreqImageSource_h
#define itkButterworthFilterFreqImageSource_h

#include "itkGenerateImageSource.h"

namespace itk
{

/** \class ButterworthFilterFreqImageSource
 *
 * \ingroup PhaseSymmetry
 */
template <typename TOutputImage>
class ButterworthFilterFreqImageSource : public GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ButterworthFilterFreqImageSource);

  /** Standard class type alias. */
  using Self = ButterworthFilterFreqImageSource;
  using Superclass = GenerateImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(ButterworthFilterFreqImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Dimensionality of the output image. */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  using OutputImageType = TOutputImage;
  using SpacingType = typename TOutputImage::SpacingType;
  using PointType = typename TOutputImage::PointType;
  using DirectionType = typename TOutputImage::DirectionType;
  using SizeType = typename TOutputImage::SizeType;

  /** Set/Get the cutoff frequency. Should be in the range [0, 0.5], where 0.5
   * corresponds to the Nyquist frequency. */
  itkSetClampMacro(Cutoff, double, 0.0, 0.5);
  itkGetConstMacro(Cutoff, double);

  /** Set/Get the filter order */
  itkSetMacro(Order, double);
  itkGetConstMacro(Order, double);

protected:
  ButterworthFilterFreqImageSource();
  ~ButterworthFilterFreqImageSource() override;

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename TOutputImage::RegionType;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  double m_Cutoff{ 0.4 };
  double m_Order{ 4 };
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkButterworthFilterFreqImageSource.hxx"
#endif

#endif
