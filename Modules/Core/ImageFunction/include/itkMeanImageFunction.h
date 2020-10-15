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
#ifndef itkMeanImageFunction_h
#define itkMeanImageFunction_h

#include "itkImageFunction.h"
#include "itkImageNeighborhoodOffsets.h"
#include "itkNumericTraits.h"
#include "itkOffset.h"

#include <vector>

namespace itk
{
/**
 * \class MeanImageFunction
 * \brief Calculate the mean value in the neighborhood of a pixel
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
 * \sa VectorMeanImageFunction
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT MeanImageFunction
  : public ImageFunction<TInputImage, typename NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MeanImageFunction);

  /** Standard class type aliases. */
  using Self = MeanImageFunction;
  using Superclass =
    ImageFunction<TInputImage, typename NumericTraits<typename TInputImage::PixelType>::RealType, TCoordRep>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;

  /** InputPixel type alias support */
  using typename Superclass::InputPixelType;

  /** OutputType typdef support. */
  using OutputType = typename Superclass::OutputType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Size type of the underlying image. */
  using ImageSizeType = typename InputImageType::SizeType;

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  /** Datatype used for the mean */
  using RealType = typename NumericTraits<typename InputImageType::PixelType>::RealType;

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
  void
  SetNeighborhoodRadius(unsigned int);
  itkGetConstReferenceMacro(NeighborhoodRadius, unsigned int);

protected:
  MeanImageFunction();
  ~MeanImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_NeighborhoodRadius{ 1 };

  std::vector<Offset<ImageDimension>> m_NeighborhoodOffsets{ GenerateRectangularImageNeighborhoodOffsets(
    ImageSizeType::Filled(1)) };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMeanImageFunction.hxx"
#endif

#endif
