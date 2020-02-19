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
#ifndef itkContinuousBorderWarpImageFilter_h
#define itkContinuousBorderWarpImageFilter_h

#include "itkWarpImageFilter.h"

namespace itk
{

/** \class ContinuousBorderWarpImageFilter
 *  \brief Warps an image using an input deformation field with continuous boundary conditions.
 *
 *  ContinuousBorderWarpImageFilter warps an existing image with respect to
 *  a given displacement field and a given interpolation scheme.The default
 *  interpolation typed used is the LinearInterpolateImageFunction.
 *  The user can specify a particular interpolation function via
 *  SetInterpolator().
 *
 *  Positions mapped to outside of the input image buffer are assigned
 *  the value of the next boundary pixel in the image.
 *
 *  The input image is set via SetInput(). The input displacement field
 *  is set via SetDisplacementField().
 *
 *  This filter is implemented as a multithreaded filter.
 *
 *  \sa WarpImageFilter
 *
 *  \warning This filter assumes that the input type, output type
 *  and displacement field type all have the same number of dimensions.
 *
 *  \ingroup VariationalRegistration
 *  \ingroup GeometricTransforms
 *  \ingroup MultiThreaded
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TInputImage, typename TOutputImage, typename TDisplacementField>
class ContinuousBorderWarpImageFilter : public WarpImageFilter<TInputImage, TOutputImage, TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ContinuousBorderWarpImageFilter);

  /** Standard class type alias. */
  using Self = ContinuousBorderWarpImageFilter;
  using Superclass = WarpImageFilter<TInputImage, TOutputImage, TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ContinuousBorderWarpImageFilter, WarpImageFilter);

  /** Typedef to describe the output image region type. */
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;

  /** Inherit some types from the superclass. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using IndexType = typename OutputImageType::IndexType;
  using SizeType = typename OutputImageType::SizeType;
  using PixelType = typename OutputImageType::PixelType;
  using SpacingType = typename OutputImageType::SpacingType;

  /** Determine the image dimension. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int DeformationFieldDimension = TDisplacementField::ImageDimension;

  /** Displacement field type alias support. */
  using DisplacementFieldType = TDisplacementField;
  using DisplacementFieldPointer = typename DisplacementFieldType::Pointer;
  using DisplacementType = typename DisplacementFieldType::PixelType;

  /** Interpolator type alias support. */
  using InterpolatorType = typename Superclass::InterpolatorType;

  /** Point type */
  using PointType = typename Superclass::PointType;

protected:
  ContinuousBorderWarpImageFilter() = default;
  ~ContinuousBorderWarpImageFilter() override = default;

  /** WarpImageFilter is implemented as a multi-threaded filter. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkContinuousBorderWarpImageFilter.hxx"
#endif

#endif
