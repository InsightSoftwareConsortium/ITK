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
#ifndef itkRobustAutomaticThresholdCalculator_h
#define itkRobustAutomaticThresholdCalculator_h

#include "itkMacro.h"
#include "itkImage.h"

namespace itk
{
/** \class RobustAutomaticThresholdCalculator
 * \brief Compute the robust automatic threshold
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Robust Automatic Threshold Selection"
 * by Lehmann G.
 * https://hdl.handle.net/1926/370
 * http://www.insight-journal.org/browse/publication/134
 *
 *
 * \ingroup Operators
 * \ingroup ITKReview
 */
template <typename TInputImage, typename TGradientImage>
class ITK_TEMPLATE_EXPORT RobustAutomaticThresholdCalculator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RobustAutomaticThresholdCalculator);

  /** Standard class type aliases. */
  using Self = RobustAutomaticThresholdCalculator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RobustAutomaticThresholdCalculator, Object);

  /** Extract the dimension of the image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard image type within this class. */
  using InputImageType = TInputImage;
  using GradientImageType = TGradientImage;

  /** Standard image type pointer within this class. */
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using GradientImagePointer = typename GradientImageType::Pointer;
  using GradientImageConstPointer = typename GradientImageType::ConstPointer;
  using InputPixelType = typename InputImageType::PixelType;
  using GradientPixelType = typename GradientImageType::PixelType;

  /** Set the input image. */
  virtual void
  SetInput(const InputImageType * image)
  {
    if (m_Input != image)
    {
      m_Input = image;
      this->Modified();
      m_Valid = false;
    }
  }

  virtual void
  SetGradient(const GradientImageType * image)
  {
    if (m_Gradient != image)
    {
      m_Gradient = image;
      this->Modified();
      m_Valid = false;
    }
  }

  itkSetMacro(Pow, double);
  itkGetConstMacro(Pow, double);

  /** Compute moments of a new or modified image.
   * This method computes the moments of the image given as a
   * parameter and stores them in the object.  The values of these
   * moments and related parameters can then be retrieved by using
   * other methods of this object. */
  void
  Compute();

  const InputPixelType &
  GetOutput() const;

protected:
  RobustAutomaticThresholdCalculator();
  ~RobustAutomaticThresholdCalculator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool           m_Valid{ false }; // Have moments been computed yet?
  double         m_Pow{ 1 };
  InputPixelType m_Output;

  InputImageConstPointer    m_Input;
  GradientImageConstPointer m_Gradient;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRobustAutomaticThresholdCalculator.hxx"
#endif

#endif
