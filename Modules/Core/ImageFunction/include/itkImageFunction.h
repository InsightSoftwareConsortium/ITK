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
#ifndef itkImageFunction_h
#define itkImageFunction_h

#include "itkFunctionBase.h"
#include "itkIndex.h"
#include "itkImageBase.h"

namespace itk
{
/**
 *\class ImageFunction
 * \brief Evaluates a function of an image at specified position.
 *
 * ImageFunction is a baseclass for all objects that evaluates
 * a function of an image at index, continuous index or point.
 * This class is templated over the input image type, the type
 * of the function output and the coordinate representation type
 * (e.g. float or double).
 *
 * The input image is set via method SetInputImage().
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex
 * respectively evaluates the function at an geometric point,
 * image index and continuous image index.
 *
 * \warning Image BufferedRegion information is cached during
 * in SetInputImage( image ). If the image BufferedRegion has changed
 * one must call SetInputImage( image ) again to update the cache
 * to the current values.
 *
 * \sa Point
 * \sa Index
 * \sa ContinuousIndex
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TOutput, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT ImageFunction : public FunctionBase<Point<TCoordRep, TInputImage::ImageDimension>, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageFunction);

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = ImageFunction;

  using Superclass = FunctionBase<Point<TCoordRep, Self::ImageDimension>, TOutput>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageFunction, FunctionBase);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;

  /** InputPixel type alias support */
  using InputPixelType = typename InputImageType::PixelType;

  /** InputImagePointer type alias support */
  using InputImageConstPointer = typename InputImageType::ConstPointer;

  /** OutputType type alias support */
  using OutputType = TOutput;

  /** CoordRepType type alias support */
  using CoordRepType = TCoordRep;

  /** Index Type. */
  using IndexType = typename InputImageType::IndexType;
  using IndexValueType = typename InputImageType::IndexValueType;

  /** ContinuousIndex Type. */
  using ContinuousIndexType = ContinuousIndex<TCoordRep, Self::ImageDimension>;

  /** Point Type. */
  using PointType = Point<TCoordRep, Self::ImageDimension>;

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  virtual void
  SetInputImage(const InputImageType * ptr);

  /** Get the input image. */
  const InputImageType *
  GetInputImage() const
  {
    return m_Image.GetPointer();
  }

  /** Evaluate the function at specified Point position.
   * Subclasses must provide this method. */
  TOutput
  Evaluate(const PointType & point) const override = 0;

  /** Evaluate the function at specified Index position.
   * Subclasses must provide this method. */
  virtual TOutput
  EvaluateAtIndex(const IndexType & index) const = 0;

  /** Evaluate the function at specified ContinuousIndex position.
   * Subclasses must provide this method. */
  virtual TOutput
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const = 0;

  /** Check if an index is inside the image buffer.
   * We take into account the fact that each voxel has its
   * center at the integer coordinate and extends half way
   * to the next integer coordinate.
   * \warning For efficiency, no validity checking of
   * the input image is done. */
  virtual bool
  IsInsideBuffer(const IndexType & index) const
  {
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      if (index[j] < m_StartIndex[j])
      {
        return false;
      }
      if (index[j] > m_EndIndex[j])
      {
        return false;
      }
    }
    return true;
  }

  /** Check if a continuous index is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image is done. */
  virtual bool
  IsInsideBuffer(const ContinuousIndexType & index) const
  {
    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      /* Test for negative of a positive so we can catch NaN's. */
      if (!(index[j] >= m_StartContinuousIndex[j] && index[j] < m_EndContinuousIndex[j]))
      {
        return false;
      }
    }
    return true;
  }

  /** Check if a point is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image pointer is done. */
  virtual bool
  IsInsideBuffer(const PointType & point) const
  {
    ContinuousIndexType index;
    m_Image->TransformPhysicalPointToContinuousIndex(point, index);
    /* Call IsInsideBuffer to test against BufferedRegion bounds.
     * TransformPhysicalPointToContinuousIndex tests against
     * LargestPossibleRegion */
    bool isInside = IsInsideBuffer(index);
    return isInside;
  }

  /** Convert point to nearest index. */
  void
  ConvertPointToNearestIndex(const PointType & point, IndexType & index) const
  {
    ContinuousIndexType cindex;

    m_Image->TransformPhysicalPointToContinuousIndex(point, cindex);
    this->ConvertContinuousIndexToNearestIndex(cindex, index);
  }

  /** Convert point to continuous index */
  void
  ConvertPointToContinuousIndex(const PointType & point, ContinuousIndexType & cindex) const
  {
    m_Image->TransformPhysicalPointToContinuousIndex(point, cindex);
  }

  /** Convert continuous index to nearest index. */
  inline void
  ConvertContinuousIndexToNearestIndex(const ContinuousIndexType & cindex, IndexType & index) const
  {
    index.CopyWithRound(cindex);
  }

  itkGetConstReferenceMacro(StartIndex, IndexType);
  itkGetConstReferenceMacro(EndIndex, IndexType);

  itkGetConstReferenceMacro(StartContinuousIndex, ContinuousIndexType);
  itkGetConstReferenceMacro(EndContinuousIndex, ContinuousIndexType);

protected:
  ImageFunction();
  ~ImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Const pointer to the input image. */
  InputImageConstPointer m_Image;

  /** Cache some values for testing if indices are inside buffered region. */
  IndexType m_StartIndex;
  IndexType m_EndIndex;

  ContinuousIndexType m_StartContinuousIndex;
  ContinuousIndexType m_EndContinuousIndex;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageFunction.hxx"
#endif

#endif
