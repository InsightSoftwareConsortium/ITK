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
#ifndef itkKappaSigmaThresholdImageFilter_h
#define itkKappaSigmaThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkKappaSigmaThresholdImageCalculator.h"

namespace itk
{
/**
 *\class KappaSigmaThresholdImageFilter
 * \brief Threshold an image using Kappa-Sigma-Clipping.
 *
 * This filter creates a binary thresholded image which separates background and
 * foreground modes by iteratively rejecting pixels that do not belong to
 * the dominant mode. The filter computes the threshold using the KappaSigmaThre-
 * sholdImageCalculator and applies that threshold to the input image using the
 * BinaryThresholdImageFilter.
 *
 * \author Gaetan Lehmann
 *
 * \note This class was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/367
 *
 * \sa KappaSigmaThresholdImageCalculator
 * \ingroup IntensityImageFilters
 * \ingroup ITKThresholding
 */

template <typename TInputImage,
          typename TMaskImage = Image<unsigned char, TInputImage::ImageDimension>,
          class TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT KappaSigmaThresholdImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(KappaSigmaThresholdImageFilter);

  /** Standard Self type alias */
  using Self = KappaSigmaThresholdImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(KappaSigmaThresholdImageFilter, ImageToImageFilter);

  /** Standard image type within this class. */
  using InputImageType = TInputImage;
  using MaskImageType = TMaskImage;

  /** Image pixel value type alias. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using MaskPixelType = typename TMaskImage::PixelType;

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;
  using MaskImagePointer = typename TMaskImage::Pointer;

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue, OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstMacro(InsideValue, OutputPixelType);

  /** Get the computed threshold. */
  itkGetConstMacro(Threshold, InputPixelType);

  /** Set the mask value used to select which pixels will be considered in the
   * threshold computation (optional, only in case a MaskImage is set). */
  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  /** Set the Sigma multiplier (Kappa) to adjust the pixel rejection rate. */
  itkSetMacro(SigmaFactor, double);
  itkGetConstMacro(SigmaFactor, double);

  /** Set the number of rejection passes. */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputComparableCheck, (Concept::Comparable<OutputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

  /** Set the mask image */
  void
  SetMaskImage(const MaskImageType * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<MaskImageType *>(input));
  }

  /** Get the mask image */
  const MaskImageType *
  GetMaskImage() const
  {
    return static_cast<MaskImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(const TInputImage * input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void
  SetInput2(const MaskImageType * input)
  {
    this->SetMaskImage(input);
  }

protected:
  KappaSigmaThresholdImageFilter();
  ~KappaSigmaThresholdImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  GenerateData() override;

  using InputSizeType = typename TInputImage::SizeType;
  using InputIndexType = typename TInputImage::IndexType;
  using InputImageRegionType = typename TInputImage::RegionType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputImageRegionType = typename TOutputImage::RegionType;

  using CalculatorType = KappaSigmaThresholdImageCalculator<TInputImage, TMaskImage>;

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

private:
  MaskPixelType   m_MaskValue;
  double          m_SigmaFactor{ 2 };
  unsigned int    m_NumberOfIterations{ 2 };
  InputPixelType  m_Threshold;
  OutputPixelType m_InsideValue;
  OutputPixelType m_OutsideValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkKappaSigmaThresholdImageFilter.hxx"
#endif

#endif
