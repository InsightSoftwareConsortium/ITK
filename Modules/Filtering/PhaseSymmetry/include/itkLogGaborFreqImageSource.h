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
#ifndef itkLogGaborFreqImageSource_h
#define itkLogGaborFreqImageSource_h

#include "itkGenerateImageSource.h"

namespace itk
{

/** \class LogGaborFreqImageSource
 *
 * \ingroup PhaseSymmetry
 */
template <typename TOutputImage>
class LogGaborFreqImageSource : public GenerateImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LogGaborFreqImageSource);

  /** Standard class type alias. */
  using Self = LogGaborFreqImageSource;
  using Superclass = GenerateImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Dimensionality of the output image */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  using OutputImageType = TOutputImage;
  using OutputImageRegionType = typename TOutputImage::RegionType;
  using SizeType = typename TOutputImage::SizeType;
  using SpacingType = typename TOutputImage::SpacingType;
  using PointType = typename TOutputImage::PointType;
  using DirectionType = typename TOutputImage::DirectionType;

  using ArrayType = FixedArray<double, ImageDimension>;

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(LogGaborFreqImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the Gaussian width parameter. */
  itkSetMacro(Sigma, double);
  itkGetConstMacro(Sigma, double);

  /** Set/Get the wavelengths in each direction. */
  itkSetMacro(Wavelengths, ArrayType);
  itkGetConstReferenceMacro(Wavelengths, ArrayType);

protected:
  LogGaborFreqImageSource();
  ~LogGaborFreqImageSource() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

private:
  // Ratio of k/wo in each direction
  double m_Sigma{ 1.0 };

  // The wavelengths in each direction
  ArrayType m_Wavelengths;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLogGaborFreqImageSource.hxx"
#endif

#endif
