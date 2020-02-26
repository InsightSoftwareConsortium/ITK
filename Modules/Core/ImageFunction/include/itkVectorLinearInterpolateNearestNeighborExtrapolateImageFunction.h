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
#ifndef itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction_h
#define itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction_h

#include "itkVectorInterpolateImageFunction.h"

namespace itk
{
/**
 * \class VectorLinearInterpolateNearestNeighborExtrapolateImageFunction
 * \brief Linearly interpolate or NN extrapolate a vector image at
 * specified positions.
 *
 * VectorLinearInterpolateNearestNeighborExtrapolateImageFunction
 * linearly interpolates (or NN extrapolates) a vector
 * image intensity non-integer pixel position. This class is templated
 * over the input image type and the coordinate representation type.
 *
 * This class is designed to work as a VectorInterpolateImageFunction,
 * hence the  IsInsideBuffer(PointType p) is overridden to always
 * answer true
 *
 * This function works for N-dimensional images.
 *
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 *
 * \warning This function work only for Vector images. For
 * scalar images use LinearInterpolateImageFunction.
 *
 *  This paper was contributed in the Insight Journal paper:
 *  https://hdl.handle.net/1926/510
 *
 * \ingroup ImageFunctions ImageInterpolators
 * \ingroup ITKImageFunction
 *
 */
template <typename TInputImage, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT VectorLinearInterpolateNearestNeighborExtrapolateImageFunction
  : public VectorInterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorLinearInterpolateNearestNeighborExtrapolateImageFunction);

  /** Standard class type aliases. */
  using Self = VectorLinearInterpolateNearestNeighborExtrapolateImageFunction;
  using Superclass = VectorInterpolateImageFunction<TInputImage, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorLinearInterpolateNearestNeighborExtrapolateImageFunction, VectorInterpolateImageFunction);

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;
  using PixelType = typename Superclass::PixelType;
  using ValueType = typename Superclass::ValueType;
  using RealType = typename Superclass::RealType;

  using PointType = typename Superclass::PointType;

  /** Grab the vector dimension from the superclass. */
  // static constexpr unsigned int Dimension = //                    Superclass::Dimension;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Output type is Vector<double,Dimension> */
  using OutputType = typename Superclass::OutputType;

  /** Should check if an index is inside the image buffer, however we
   * require that it answers true to use the extrapolation possibility. */
  bool
  IsInsideBuffer(const IndexType &) const override
  {
    return true;
  }

  /** Should check if a point is inside the image buffer, however we
   * require that it answers true to use the extrapolation possibility. */
  bool
  IsInsideBuffer(const PointType &) const override
  {
    return true;
  }

  /** Should check if a continuous index is inside the image buffer, however we
   * require that it answers true to use the extrapolation possibility. */
  bool
  IsInsideBuffer(const ContinuousIndexType &) const override
  {
    return true;
  }

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the linearly interpolated image intensity at a
   * specified point position. If the point does not lie within the
   * image buffer a nearest neighbor interpolation is done. */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override;

  /** Evaluate the function at an index position
   *
   * Simply returns the image value at the
   * specified index position. If the index does not lie within the
   * image buffer a nearest neighbor interpolation is done. */
  OutputType
  EvaluateAtIndex(const IndexType & index) const override;

protected:
  VectorLinearInterpolateNearestNeighborExtrapolateImageFunction() = default;
  ~VectorLinearInterpolateNearestNeighborExtrapolateImageFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Number of neighbors used in the interpolation */
  static const unsigned int m_Neighbors;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunction.hxx"
#endif

#endif
