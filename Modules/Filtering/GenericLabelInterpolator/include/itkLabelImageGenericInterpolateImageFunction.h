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
#ifndef itkLabelImageGenericInterpolateImageFunction_h
#define itkLabelImageGenericInterpolateImageFunction_h

#include <itkInterpolateImageFunction.h>
#include "itkLabelSelectionImageAdaptor.h"
#include <vector>
#include <set>

namespace itk
{

/** \class LabelImageGenericInterpolateImageFunction
 * \brief Interpolation function for multi-label images that implicitly interpolates each
 * unique value in the image corresponding to each label set element and returns the
 * corresponding label set element with the largest weight.
 *
 * This filter is an alternative to nearest neighbor interpolation for multi-label
 * images. It can use almost any underlying interpolator.
 * * \ingroup ITKImageFunction
 * * \ingroup GenericLabelInterpolator
 */

template <typename TInputImage, template <typename, typename> class TInterpolator, typename TCoordRep = double>
class ITK_EXPORT LabelImageGenericInterpolateImageFunction : public InterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelImageGenericInterpolateImageFunction);

  /** Standard class type alias. */
  using Self = LabelImageGenericInterpolateImageFunction;
  using Superclass = InterpolateImageFunction<TInputImage, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputPixelType = typename TInputImage::PixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabelImageGenericInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** ImageDimension constant */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** OutputType type alias support. */
  using OutputType = typename Superclass::OutputType;

  /** InputImageType type alias support. */
  using InputImageType = typename Superclass::InputImageType;

  /** RealType type alias support. */
  using RealType = typename Superclass::RealType;

  /** Index type alias support. */
  using IndexType = typename Superclass::IndexType;

  /** Size type alias support. */
  using SizeType = typename Superclass::SizeType;

  /** ContinuousIndex type alias support. */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  using LabelSelectionAdaptorType = LabelSelectionImageAdaptor<TInputImage, double>;

  // The interpolator used for individual binary masks corresponding to each label
  using InternalInterpolatorType = TInterpolator<LabelSelectionAdaptorType, TCoordRep>;

  /**
   * Evaluate at the given index
   */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const override
  {
    return this->EvaluateAtContinuousIndex(cindex, nullptr);
  }

  void
  SetInputImage(const TInputImage * image) override;

  /** Get the radius required for interpolation.
   *
   * This defines the number of surrounding pixels required to interpolate at
   * a given point.
   */
  SizeType
  GetRadius() const override
  {
    return SizeType::Filled(1);
  }

protected:
  LabelImageGenericInterpolateImageFunction() = default;
  ~LabelImageGenericInterpolateImageFunction() override = default;

  std::vector<typename InternalInterpolatorType::Pointer>  m_InternalInterpolators;
  std::vector<typename LabelSelectionAdaptorType::Pointer> m_LabelSelectionAdaptors;
  using LabelSetType = std::set<typename TInputImage::PixelType>;
  LabelSetType m_Labels;

private:
  /**
   * Evaluate function value at the given index
   */
  virtual OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType &, OutputType *) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelImageGenericInterpolateImageFunction.hxx"
#endif

#endif
