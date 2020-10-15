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
#ifndef itkBinaryThresholdImageFunction_h
#define itkBinaryThresholdImageFunction_h

#include "itkImageFunction.h"

namespace itk
{
/**
 *\class BinaryThresholdImageFunction
 * \brief Returns true is the value of an image lies within a range
 *        of thresholds
 * This ImageFunction returns true (or false) if the pixel value lies
 * within (outside) a lower and upper threshold value. The threshold
 * range can be set with the ThresholdBelow, ThresholdBetween or
 * ThresholdAbove methods.  The input image is set via method
 * SetInputImage().
 *
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex
 * respectively evaluate the function at an geometric point, image index
 * and continuous image index.
 *
 * \ingroup ImageFunctions
 *
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT BinaryThresholdImageFunction : public ImageFunction<TInputImage, bool, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryThresholdImageFunction);

  /** Standard class type aliases. */
  using Self = BinaryThresholdImageFunction;
  using Superclass = ImageFunction<TInputImage, bool, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;

  /** Typedef to describe the type of pixel. */
  using PixelType = typename TInputImage::PixelType;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** BinaryThreshold the image at a point position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */

  bool
  Evaluate(const PointType & point) const override
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return (this->EvaluateAtIndex(index));
  }

  /** BinaryThreshold the image at a continuous index position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  bool
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override
  {
    IndexType nindex;

    this->ConvertContinuousIndexToNearestIndex(index, nindex);
    return this->EvaluateAtIndex(nindex);
  }

  /** BinaryThreshold the image at an index position.
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  bool
  EvaluateAtIndex(const IndexType & index) const override
  {
    PixelType value = this->GetInputImage()->GetPixel(index);

    return (m_Lower <= value && value <= m_Upper);
  }

  /** Get the lower threshold value. */
  itkGetConstReferenceMacro(Lower, PixelType);

  /** Get the upper threshold value. */
  itkGetConstReferenceMacro(Upper, PixelType);

  /** Values greater than or equal to the value are inside. */
  void
  ThresholdAbove(PixelType thresh);

  /** Values less than or equal to the value are inside. */
  void
  ThresholdBelow(PixelType thresh);

  /** Values that lie between lower and upper inclusive are inside. */
  void
  ThresholdBetween(PixelType lower, PixelType upper);

protected:
  BinaryThresholdImageFunction();
  ~BinaryThresholdImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  PixelType m_Lower;
  PixelType m_Upper;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryThresholdImageFunction.hxx"
#endif

#endif
