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

#ifndef itkLabelImageGaussianInterpolateImageFunction_h
#define itkLabelImageGaussianInterpolateImageFunction_h

#include "itkGaussianInterpolateImageFunction.h"

namespace itk
{

/**
 *\class LabelImageGaussianInterpolateImageFunction
 * \brief Interpolation function for multi-label images that implicitly smooths each
 * unique value in the image corresponding to each label set element and returns the
 * corresponding label set element with the largest weight.
 *
 * This filter is an alternative to nearest neighbor interpolation for multi-label
 * images. Given a multi-label image \c I with label set \c L, this function returns a
 * label at the non-voxel position \c I(x), based on the following rule
 *
 * \f[
 * I(x) = \arg\max_{l \in L} (G_\sigma * I_l)(x)
 * \f]
 *
 * Where \f$ I_l \f$ is the \c l-th binary component of the multilabel image. In other words,
 * each label in the multi-label image is convolved with a Gaussian, and the label
 * for which the response is largest is returned. For sigma=0, this is just nearest
 * neighbor interpolation.
 *
 * This class defines an N-dimensional Gaussian interpolation function for label
 * using the vnl error function.  The two parameters associated with this function
 * are:
 * \li \c Sigma - a scalar array of size ImageDimension determining the width
 *      of the interpolation function.
 * \li \c Alpha - a scalar specifying the cutoff distance over which the function
 *      is calculated.
 *
 * \note The input image can be of any type, but the number of unique intensity values
 * in the image will determine the amount of memory needed to complete each interpolation.
 *
 *
 * \author Paul Yushkevich
 * \author Nick Tustison
 *
 * \ingroup ITKImageFunction
 *
 * \sphinx
 * \sphinxexample{Core/ImageFunction/ResampleSegmentedImage,Resample Segmented Image}
 * \endsphinx
 */

template <typename TInputImage,
          typename TCoordRep = double,
          typename TPixelCompare = std::less<typename itk::NumericTraits<typename TInputImage::PixelType>::RealType>>
class ITK_TEMPLATE_EXPORT LabelImageGaussianInterpolateImageFunction
  : public GaussianInterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(LabelImageGaussianInterpolateImageFunction);

  /** Standard class type aliases. */
  using Self = LabelImageGaussianInterpolateImageFunction;
  using Superclass = GaussianInterpolateImageFunction<TInputImage, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using InputPixelType = typename TInputImage::PixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabelImageGaussianInterpolateImageFunction, GaussianInterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** ImageDimension constant */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** OutputType type alias support */
  using OutputType = typename Superclass::OutputType;

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;

  /** RealType type alias support */
  using RealType = typename Superclass::RealType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Array type alias support */
  using ArrayType = typename Superclass::ArrayType;

  /**
   * Evaluate at the given index
   */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const override
  {
    return this->EvaluateAtContinuousIndex(cindex, nullptr);
  }

protected:
  LabelImageGaussianInterpolateImageFunction() = default;
  ~LabelImageGaussianInterpolateImageFunction() override = default;

private:
  /**
   * Evaluate function value at the given index
   */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType &, OutputType *) const override;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelImageGaussianInterpolateImageFunction.hxx"
#endif

#endif
