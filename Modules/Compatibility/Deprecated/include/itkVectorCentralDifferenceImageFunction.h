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
#ifndef itkVectorCentralDifferenceImageFunction_h
#define itkVectorCentralDifferenceImageFunction_h

#include "itkImageFunction.h"
#include "itkMatrix.h"

namespace itk
{

/**
 * \class VectorCentralDifferenceImageFunction
 * \brief Calculate the derivative by central differencing.
 *
 * This class is templated over the input image type and
 * the coordinate representation type (e.g. float or double).
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/510
 *
 * \deprecated Please use CentralDifferenceImageFunction instead.
 *
 * \ingroup ImageFunctions
 * \ingroup ITKDeprecated
 */
template <typename TInputImage, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT VectorCentralDifferenceImageFunction
  : public ImageFunction<TInputImage,
                         Matrix<double, TInputImage::PixelType::Dimension, TInputImage::ImageDimension>,
                         TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorCentralDifferenceImageFunction);

  using InputPixelType = typename TInputImage::PixelType;

  /** Extract the vector dimension from the pixel template parameter. */
  static constexpr unsigned int Dimension = InputPixelType::Dimension;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = VectorCentralDifferenceImageFunction;
  using Superclass = ImageFunction<TInputImage, Matrix<double, Self::Dimension, Self::ImageDimension>, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorCentralDifferenceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;

  /** OutputType typdef support. */
  using typename Superclass::OutputType;

  /** Index type alias support */
  using typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using typename Superclass::ContinuousIndexType;

  /** Point type alias support */
  using typename Superclass::PointType;

  /** Evalulate the image derivative by central differencing at specified index.
   *
   *  No bounds checking is done.
   *  The point is assume to lie within the image buffer.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   *  calling the method. */
  OutputType
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evalulate the image derivative by central differencing at non-integer
   *  positions.
   *
   *  No bounds checking is done.
   *  The point is assumed to lie within the image buffer.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   *  calling the method. */
  OutputType
  Evaluate(const PointType & point) const override
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex(index);
  }

  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const override
  {
    IndexType index;

    this->ConvertContinuousIndexToNearestIndex(cindex, index);
    return this->EvaluateAtIndex(index);
  }

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not. The flag ON will take into
   * account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

protected:
  VectorCentralDifferenceImageFunction();
  ~VectorCentralDifferenceImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorCentralDifferenceImageFunction.hxx"
#endif

#endif
