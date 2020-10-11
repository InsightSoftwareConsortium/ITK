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
#ifndef itkPointSetFunction_h
#define itkPointSetFunction_h

#include "itkFunctionBase.h"
#include "itkPoint.h"

namespace itk
{


/** \class PointSetFunction
 * \brief Evaluates a function of an image at specified position.
 *
 * FIXME: Documentation was copy-pasted from ImageFunction.
 *
 * PointSetFunction is a baseclass for all objects that evaluates
 * a function of an image at index, continuous index or point.
 * This class is templated over the input image type, the type
 * of the function output and the coordinate representation type
 * (e.g. float or double).
 *
 * The input image is set via method SetInputPointSet().
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex
 * respectively evaluates the function at an geometric point,
 * image index and continuous image index.
 *
 * \warning Image BufferedRegion information is cached during
 * in SetInputPointSet( image ). If the image BufferedRegion has changed
 * one must call SetInputPointSet( image ) again to update the cache
 * to the current values.
 *
 * \sa Point
 * \sa Index
 * \sa ContinuousIndex
 *
 * \ingroup ITKMetricsv4
 */
template <typename TInputPointSet, typename TOutput, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT PointSetFunction : public FunctionBase<typename TInputPointSet::PointType, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PointSetFunction);

  /** Dimension underlying input point set. */
  static constexpr unsigned int Dimension = TInputPointSet::PointDimension;

  /** Standard class type aliases. */
  using Self = PointSetFunction;
  using Superclass = FunctionBase<typename TInputPointSet::PointType, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetFunction, FunctionBase);

  /** InputPointSetType type alias support */
  using InputPointSetType = TInputPointSet;

  /** InputPixel type alias support */
  using InputPointType = typename InputPointSetType::PointType;
  using InputPixelType = typename InputPointSetType::PixelType;

  /** InputPointSetPointer type alias support */
  using InputPointSetConstPointer = typename InputPointSetType::ConstPointer;

  /** OutputType type alias support */
  using OutputType = TOutput;

  /** CoordRepType type alias support */
  using CoordRepType = TCoordRep;

  /** Set the input point set.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputPointSet again to update cached values. */
  virtual void
  SetInputPointSet(const InputPointSetType * ptr);

  /** Get the input image. */
  const InputPointSetType *
  GetInputPointSet() const
  {
    return m_PointSet.GetPointer();
  }

  /** Evaluate the function at specified Point position.
   * Subclasses must provide this method. */
  TOutput
  Evaluate(const InputPointType & point) const override = 0;

protected:
  PointSetFunction();
  ~PointSetFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Const pointer to the input image. */
  InputPointSetConstPointer m_PointSet;
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_PointSetFunction(_, EXPORT, x, y)                                                                 \
  namespace itk                                                                                                        \
  {                                                                                                                    \
  _(3(class EXPORT PointSetFunction<ITK_TEMPLATE_3 x>))                                                                \
  namespace Templates                                                                                                  \
  {                                                                                                                    \
  using PointSetFunction##y = PointSetFunction<ITK_TEMPLATE_3 x>;                                                      \
  }                                                                                                                    \
  }


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPointSetFunction.hxx"
#endif


#endif
