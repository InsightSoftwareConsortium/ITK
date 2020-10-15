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
#ifndef itkVectorMeanImageFunction_h
#define itkVectorMeanImageFunction_h

#include "itkImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class VectorMeanImageFunction
 * \brief Calculate the mean value in the neighborhood of a pixel in a Vector image.
 *
 * Calculate the mean pixel value over the standard 8, 26, etc. connected
 * neighborhood.  This calculation uses a ZeroFluxNeumannBoundaryCondition.
 *
 * If called with a ContinuousIndex or Point, the calculation is performed
 * at the nearest neighbor.
 *
 * This class is templated over the input image type and the
 * coordinate representation type (e.g. float or double).
 *
 * \sa MeanImageFunction
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT VectorMeanImageFunction
  : public ImageFunction<TInputImage, typename NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorMeanImageFunction);

  /** Standard class type aliases. */
  using Self = VectorMeanImageFunction;
  using Superclass =
    ImageFunction<TInputImage, typename NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorMeanImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;

  /** OutputType typdef support. */
  using OutputType = typename Superclass::OutputType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  /** Datatype used for the mean */
  using RealType = typename NumericTraits<typename TInputImage::PixelType>::RealType;

  /** Evalulate the function at specified index */
  RealType
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at non-integer positions */
  RealType
  Evaluate(const PointType & point) const override
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex(index);
  }

  RealType
  EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const override
  {
    IndexType index;

    this->ConvertContinuousIndexToNearestIndex(cindex, index);
    return this->EvaluateAtIndex(index);
  }

  /** Get/Set the radius of the neighborhood over which the
      statistics are evaluated */
  itkSetMacro(NeighborhoodRadius, unsigned int);
  itkGetConstReferenceMacro(NeighborhoodRadius, unsigned int);

protected:
  VectorMeanImageFunction();
  ~VectorMeanImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_NeighborhoodRadius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorMeanImageFunction.hxx"
#endif

#endif
