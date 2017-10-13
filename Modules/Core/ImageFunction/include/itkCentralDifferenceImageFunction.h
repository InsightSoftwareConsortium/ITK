/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
 * accomadate a result for each pixel component in each dimension.
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
template<
  typename TInputImage,
  typename TCoordRep = float,
  typename TOutputType = CovariantVector<double, TInputImage::ImageDimension >
  >
class ITK_TEMPLATE_EXPORT CentralDifferenceImageFunction:
  public ImageFunction< TInputImage,
                        TOutputType,
                        TCoordRep >
{
public:
  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef CentralDifferenceImageFunction   Self;
  typedef ImageFunction< TInputImage,
                         TOutputType,
                         TCoordRep >       Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CentralDifferenceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** InputPixelType typedef support */
  typedef typename InputImageType::PixelType InputPixelType;

  /** InputPixelConvert typedef support */
  typedef DefaultConvertPixelTraits< InputPixelType > InputPixelConvertType;

  /** OutputType typdef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Output convert typedef support */
  typedef DefaultConvertPixelTraits<OutputType> OutputConvertType;

  /** Output value typedef support */
  typedef typename OutputConvertType::ComponentType OutputValueType;

  /** Scalar derivative typedef support */
  typedef CovariantVector<OutputValueType, itkGetStaticConstMacro(ImageDimension) > ScalarDerivativeType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Spacing typedef support. */
  typedef typename TInputImage::SpacingType SpacingType;

  /** Interpolator typedef support. */
  typedef InterpolateImageFunction< TInputImage, TCoordRep > InterpolatorType;
  typedef typename InterpolatorType::Pointer                 InterpolatorPointer;

  /** Set the input image.  This must be set by the user. */
  virtual void SetInputImage(const TInputImage *inputData) ITK_OVERRIDE;

  /** Set interpolator. The interpolator is used in the methods
   * \c Evaluate and \c EvaluateAtContinuousIndex. */
  virtual void SetInterpolator(InterpolatorType *interpolator);

  /** Get the interpolator. */
  itkGetModifiableObjectMacro(Interpolator, InterpolatorType );

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
  virtual OutputType EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE;

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
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE;

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
  virtual OutputType EvaluateAtContinuousIndex( const ContinuousIndexType & cindex) const ITK_OVERRIDE;

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
  ~CentralDifferenceImageFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CentralDifferenceImageFunction);


  /** Structure for specialization of Evaulate* methods on OutputType */
  template<typename T>
  struct OutputTypeSpecializationStructType
  {
    typedef T Type;
  };

  /** Specialized versions of EvaluteAtIndex() method to handle scalar or vector pixel types.*/
  template< typename Type >
  inline void EvaluateAtIndexSpecialized( const IndexType & index, OutputType & derivative, OutputTypeSpecializationStructType<OutputType>) const;
  template< typename Type >
  inline void EvaluateAtIndexSpecialized( const IndexType & index, OutputType & derivative, OutputTypeSpecializationStructType<Type>) const;

  /** Specialized versions of EvaluteAtContinuousIndex() method to handle scalar or vector pixel types.*/
  template< typename Type >
  inline void EvaluateAtContinuousIndexSpecialized( const ContinuousIndexType & index, OutputType & derivative, OutputTypeSpecializationStructType<OutputType>) const;
  template< typename Type >
  inline void EvaluateAtContinuousIndexSpecialized( const ContinuousIndexType & index, OutputType & derivative, OutputTypeSpecializationStructType<Type>) const;

  /** Specialized versions of Evalute() method to handle scalar or vector pixel types.*/
  // NOTE: for some unknown reason, making these methods inline (as those above are inlined) makes them run *slower*.
  template< typename Type >
  void EvaluateSpecialized( const PointType & point, OutputType & derivative, OutputTypeSpecializationStructType<OutputType>) const;
  template< typename Type >
  void EvaluateSpecialized( const PointType & point, OutputType & derivative, OutputTypeSpecializationStructType<Type>) const;

  // flag to take or not the image direction into account
  // when computing the derivatives.
  bool m_UseImageDirection;

  // interpolator
  InterpolatorPointer   m_Interpolator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCentralDifferenceImageFunction.hxx"
#endif

#endif
