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
#ifndef itkPeakSignalToNoiseRatioCalculator_h
#define itkPeakSignalToNoiseRatioCalculator_h

#include "itkMacro.h"
#include "itkImage.h"

namespace itk
{

/**
 *\class PeakSignalToNoiseRatioCalculator
 * \brief Compute the PSNR of a noisy image
 *
 * \todo This class needs to be refactored and made into a proper
 * process object.
 *
 * \ingroup ITKImageNoise
 */
template <class TInputImage>
class ITK_TEMPLATE_EXPORT PeakSignalToNoiseRatioCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PeakSignalToNoiseRatioCalculator);

  /** Standard class type aliases. */
  using Self = PeakSignalToNoiseRatioCalculator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PeakSignalToNoiseRatioCalculator, Object);

  /** Extract the dimension of the image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard image type within this class. */
  using InputImageType = TInputImage;

  /** Standard image type pointer within this class. */
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputPixelType = typename InputImageType::PixelType;

  /** Set the input image. */
  virtual void
  SetImage(const InputImageType * image)
  {
    if (m_Image != image)
    {
      m_Image = image;
      this->Modified();
      m_Valid = false;
    }
  }

  virtual void
  SetNoisyImage(const InputImageType * image)
  {
    if (m_NoisyImage != image)
    {
      m_NoisyImage = image;
      this->Modified();
      m_Valid = false;
    }
  }

  void
  Compute();

  const double &
  GetOutput() const;

protected:
  PeakSignalToNoiseRatioCalculator();
  ~PeakSignalToNoiseRatioCalculator() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool m_Valid; // Have moments been computed
                // yet?
  double m_Output;

  InputImageConstPointer m_Image;
  InputImageConstPointer m_NoisyImage;

}; // class PeakSignalToNoiseRatioCalculator

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPeakSignalToNoiseRatioCalculator.hxx"
#endif

#endif /* itkPeakSignalToNoiseRatioCalculator_h */
