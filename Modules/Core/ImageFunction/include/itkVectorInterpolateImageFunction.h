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
#ifndef itkVectorInterpolateImageFunction_h
#define itkVectorInterpolateImageFunction_h

#include "itkImageFunction.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class VectorInterpolateImageFunction
 * \brief Base class for all vector image interpolaters.
 *
 * VectorInterpolateImageFunction is the base for all ImageFunctions that
 * interpolates image with vector pixel types. This function outputs
 * a return value of type Vector<double,Dimension>.
 *
 * This class is templated input image type and the coordinate
 * representation type.
 *
 * \warning This hierarchy of functions work only for images
 * with Vector-based pixel types. For scalar images use
 * InterpolateImageFunction.
 *
 * \sa InterpolateImageFunction
 * \ingroup ImageFunctions ImageInterpolators
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TCoordRep = double >
class ITK_TEMPLATE_EXPORT VectorInterpolateImageFunction:
  public ImageFunction<
    TInputImage,
    typename NumericTraits< typename TInputImage::PixelType >::RealType,
    TCoordRep >
{
public:
  /** Extract the vector dimension from the pixel template parameter. */
  itkStaticConstMacro(Dimension, unsigned int,
                      TInputImage::PixelType::Dimension);

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef VectorInterpolateImageFunction Self;
  typedef ImageFunction< TInputImage,
                         typename NumericTraits< typename TInputImage::PixelType >::RealType,
                         TCoordRep >                          Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorInterpolateImageFunction, ImageFunction);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType           InputImageType;
  typedef typename InputImageType::PixelType            PixelType;
  typedef typename PixelType::ValueType                 ValueType;
  typedef typename NumericTraits< ValueType >::RealType RealType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType      IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Output type is RealType of TInputImage::PixelType. */
  typedef typename Superclass::OutputType OutputType;

  /** CoordRep typedef support. */
  typedef TCoordRep CoordRepType;

  /** Returns the interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point, index);
    return ( this->EvaluateAtContinuousIndex(index) );
  }

  /** Interpolate the image at a continuous index position
   *
   * Returns the interpolated image intensity at a
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * Subclasses must override this method.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const ITK_OVERRIDE = 0;

  /** Interpolate the image at an index position.
   * Simply returns the image value at the
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE
  {
    OutputType output;
    PixelType  input = this->GetInputImage()->GetPixel(index);

    for ( unsigned int k = 0;
              k < this->GetInputImage()->GetNumberOfComponentsPerPixel(); k++ )
      {
      output[k] = static_cast< double >( input[k] );
      }
    return ( output );
  }

protected:
  VectorInterpolateImageFunction() {}
  ~VectorInterpolateImageFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorInterpolateImageFunction);
};
} // end namespace itk

#endif
