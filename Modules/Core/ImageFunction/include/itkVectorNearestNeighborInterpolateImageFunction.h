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
#ifndef itkVectorNearestNeighborInterpolateImageFunction_h
#define itkVectorNearestNeighborInterpolateImageFunction_h

#include "itkVectorInterpolateImageFunction.h"

namespace itk
{
/**
 * \class VectorNearestNeighborInterpolateImageFunction
 * \brief Nearest neighbor interpolate a vector image at specified positions.
 *
 * VectorNearestNeighborInterpolateImageFunction interpolates vector
 * image intensity non-integer pixel position using nearest neighbor interpolation.
 * This class is templated over the input image type and the coordinate
 * representation type.
 *
 * This function works for N-dimensional images.
 *
 * \warning This function works only for Vector images.
 *
 * \ingroup ImageFunctions ImageInterpolators
 *
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TCoordRep = double>
class VectorNearestNeighborInterpolateImageFunction : public VectorInterpolateImageFunction<TInputImage, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorNearestNeighborInterpolateImageFunction);

  /** Standard class type aliases. */
  using Self = VectorNearestNeighborInterpolateImageFunction;
  using Superclass = VectorInterpolateImageFunction<TInputImage, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorNearestNeighborInterpolateImageFunction, VectorInterpolateImageFunction);

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;
  using PixelType = typename Superclass::PixelType;
  using ValueType = typename Superclass::ValueType;
  using RealType = typename Superclass::RealType;

  /** Grab the vector dimension from the superclass. */
  static constexpr unsigned int Dimension = Superclass::Dimension;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Output type is Vector<double,Dimension> */
  using OutputType = typename Superclass::OutputType;

  /** Evaluate the function at a ContinuousIndex position
   *
   * Returns the interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override
  {
    IndexType nindex;

    this->ConvertContinuousIndexToNearestIndex(index, nindex);
    return static_cast<OutputType>(this->GetInputImage()->GetPixel(nindex));
  }

protected:
  VectorNearestNeighborInterpolateImageFunction() = default;
  ~VectorNearestNeighborInterpolateImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }
};
} // end namespace itk

#endif
