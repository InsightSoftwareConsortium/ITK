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
#ifndef itkCentralDifferenceImageFunction_h
#define itkCentralDifferenceImageFunction_h

#include "itkImageFunction.h"
#include "itkCovariantVector.h"
#include "itkInterpolateImageFunction.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{
/**
 * \class CentralDifferenceImageFunction
 * \brief Calculate the derivative by central differencing.
 *
 * This class is templated over the input image type,
 * the coordinate representation type (e.g. float or double),
 * and the output derivative type.
 *
 * This class supports both scalar and vector pixel types
 * for the input image, including VectorImage types.
 *
 * For vector-pixel image types, the TOutputType template
 * parameter must be set to a vector of appropriate size, to
 * accommodate a result for each pixel component in each dimension.
 * The output is packed by pixel component, i.e.
 *
 *  [C0D0, C0D1, ..., C0DN, C1D0, ...]
 *
 * where C = pixel component, and D = image dimension.
 *
 * The output type can be, for example:
 *
 *     \code CovariantVector<double, numberOfPixelComponents * ImageDimension> \endcode
 *  or
 *     \code Matrix<double, numberOfPixelComponents, ImageDimension> \endcode
 *
 * Possible improvements:
 *
 * 1) speed performance:
 * The template-specialization of the Evaluate*() methods (needed
 * to support vector-pixel types) incur a performance penalty for the
 * scalar-pixel case, when compared with previous scalar-only
 * versions of the code. On MacOS (2.4GHz Core 2 Duo, gcc 4.2)
 * the penalty is 0.5-2%, depending on the method. To recover this loss,
 * the specialization of the methods would have to be done such that
 * a nested subroutine need not be called, ie the specialization is
 * performed on the Evaluate* methods directly. At the moment is seems
 * this can't be done without requiring a template parameter on the
 * methods.
 *
 * 2) the use of Neighborhood operators may improve efficiency.
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 */
template <typename TInputImage,
          typename TCoordRep = float,
          typename TOutputType = CovariantVector<double, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT CentralDifferenceImageFunction : public ImageFunction<TInputImage, TOutputType, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(CentralDifferenceImageFunction);

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard class type aliases. */
  using Self = CentralDifferenceImageFunction;
  using Superclass = ImageFunction<TInputImage, TOutputType, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CentralDifferenceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;

  /** InputPixelType type alias support */
  using InputPixelType = typename InputImageType::PixelType;

  /** InputPixelConvert type alias support */
  using InputPixelConvertType = DefaultConvertPixelTraits<InputPixelType>;

  /** OutputType typdef support. */
  using OutputType = typename Superclass::OutputType;

  /** Output convert type alias support */
  using OutputConvertType = DefaultConvertPixelTraits<OutputType>;

  /** Output value type alias support */
  using OutputValueType = typename OutputConvertType::ComponentType;

  /** Scalar derivative type alias support */
  using ScalarDerivativeType = CovariantVector<OutputValueType, Self::ImageDimension>;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Spacing type alias support */
  using SpacingType = typename TInputImage::SpacingType;

  /** Interpolator type alias support */
  using InterpolatorType = InterpolateImageFunction<TInputImage, TCoordRep>;
  using InterpolatorPointer = typename InterpolatorType::Pointer;

  /** Set the input image.  This must be set by the user. */
  void
  SetInputImage(const TInputImage * inputData) override;

  /** Set interpolator. The interpolator is used in the methods
   * \c Evaluate and \c EvaluateAtContinuousIndex. */
  virtual void
  SetInterpolator(InterpolatorType * interpolator);

  /** Get the interpolator. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType);

  /** Evalulate the image derivative by central differencing at specified index.
   *
   *  No bounds checking is done.
   *  The point is assumed to lie within the image buffer.
   *
   *  If \c index lies on a boundary in a given dimension, 0 is returned for
   *  that dimension.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evalulate the image derivative by central differencing at non-integer
   *  point.
   *
   *  No bounds checking is done.
   *  The point is assumed to lie within the image buffer. If not, 0 is
   *  returned for the derivative without any error return, because of
   *  bounds-checking performed on the neighboring points.
   *
   *  If \c point lies on a boundary in a given dimension, 0 is returned for
   *  that dimension. Note that points are centered on the voxel.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  Evaluate(const PointType & point) const override;

  /** Evalulate the image derivative by central differencing at non-integer
   *  index.
   *
   *  No bounds checking is done.
   *  The point is assumed to lie within the image buffer.
   *
   *  If \c cindex lies on a boundary in a given dimension, 0 is returned for
   *  that dimension.
   *
   *  ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const override;

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not.
   * For \c EvaluateAtIndex and \c EvaluateAtContinuousIndex, the flag ON will
   * take into account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * For \c Evaluate, the opposite is true: the flag OFF will ignore the image
   * direction and will result in an extra matrix multiplication compared to the
   * amount of computation performed when the flag is ON.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

protected:
  CentralDifferenceImageFunction();
  ~CentralDifferenceImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** Structure for specialization of Evaulate* methods on OutputType */
  template <typename T>
  struct OutputTypeSpecializationStructType
  {
    using Type = T;
  };

  /** Specialized versions of EvaluteAtIndex() method to handle scalar or vector pixel types.*/
  template <typename Type>
  inline void
  EvaluateAtIndexSpecialized(const IndexType & index,
                             OutputType &      derivative,
                             OutputTypeSpecializationStructType<OutputType>) const;
  template <typename Type>
  inline void
  EvaluateAtIndexSpecialized(const IndexType & index,
                             OutputType &      derivative,
                             OutputTypeSpecializationStructType<Type>) const;

  /** Specialized versions of EvaluteAtContinuousIndex() method to handle scalar or vector pixel types.*/
  template <typename Type>
  inline void
  EvaluateAtContinuousIndexSpecialized(const ContinuousIndexType & index,
                                       OutputType &                derivative,
                                       OutputTypeSpecializationStructType<OutputType>) const;
  template <typename Type>
  inline void
  EvaluateAtContinuousIndexSpecialized(const ContinuousIndexType & index,
                                       OutputType &                derivative,
                                       OutputTypeSpecializationStructType<Type>) const;

  /** Specialized versions of Evalute() method to handle scalar or vector pixel types.*/
  // NOTE: for some unknown reason, making these methods inline (as those above are inlined) makes them run *slower*.
  template <typename Type>
  void
  EvaluateSpecialized(const PointType & point,
                      OutputType &      derivative,
                      OutputTypeSpecializationStructType<OutputType>) const;
  template <typename Type>
  void
  EvaluateSpecialized(const PointType & point, OutputType & derivative, OutputTypeSpecializationStructType<Type>) const;

  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection;

  // interpolator
  InterpolatorPointer m_Interpolator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCentralDifferenceImageFunction.hxx"
#endif

#endif
