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
#ifndef itkNeighborhoodOperatorImageFunction_h
#define itkNeighborhoodOperatorImageFunction_h

#include "itkImageFunction.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class NeighborhoodOperatorImageFunction
 * \brief Compute the convolution of a neighborhood operator with the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 * \ingroup ITKImageFunction
 *
 * \sphinx
 * \sphinxexample{Core/ImageFunction/MultiplyKernelWithAnImageAtLocation,Multiply Kernel With An Image At Location}
 * \endsphinx
 */
template <typename TInputImage, typename TOutput>
class ITK_TEMPLATE_EXPORT NeighborhoodOperatorImageFunction : public ImageFunction<TInputImage, TOutput>
{
public:
  /**Standard "Self" type alias */
  using Self = NeighborhoodOperatorImageFunction;

  /** Standard "Superclass" type alias */
  using Superclass = ImageFunction<TInputImage, TOutput>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using IndexType = typename Superclass::IndexType;
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NeighborhoodOperatorImageFunction, ImageFunction);

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using NeighborhoodType = Neighborhood<TOutput, Self::ImageDimension>;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Set the input image. */
  // virtual void SetInputImage( InputImageType * ptr );

  /** Sets the operator that is used to filter the image. Note
   * that the operator is stored as an internal COPY (it
   * is not part of the pipeline). */
  void
  SetOperator(const NeighborhoodType & p) const
  {
    m_Operator = p;
    this->Modified();
  }

  /** Evalutate the  in the given dimension at specified point
   *  Subclasses should override this method. */
  TOutput
  Evaluate(const PointType &) const override
  {
    std::cout << "NeighborhoodOperatorImageFunction::Evaluate(): Not implemented!" << std::endl;
    TOutput out;
    out = 0;
    return out;
  }

  /** Evaluate the function at specified Index position */
  TOutput
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at specified ContinuousIndex position.
   * Subclasses should override this method. */
  TOutput
  EvaluateAtContinuousIndex(const ContinuousIndexType &) const override
  {
    std::cout << "NeighborhoodOperatorImageFunction::EvaluateAtContinuousIndex():Not implemented!" << std::endl;
    TOutput out;
    out = 0;
    return out;
  }

protected:
  NeighborhoodOperatorImageFunction() = default;
  NeighborhoodOperatorImageFunction(const Self &) {}

  ~NeighborhoodOperatorImageFunction() override = default;

  void
  operator=(const Self &)
  {}
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  mutable NeighborhoodType m_Operator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNeighborhoodOperatorImageFunction.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperatorImageFunction.hxx"
#endif
*/

#endif
