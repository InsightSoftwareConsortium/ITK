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
#ifndef itkExtrapolateImageFunction_h
#define itkExtrapolateImageFunction_h

#include "itkImageFunction.h"

namespace itk
{
/** \class ExtrapolateImageFunction
 * \brief Base class for all image extrapolaters.
 *
 * ExtrapolateImageFunction is the base for all ImageFunctions that
 * extrapolates image intensity at a non-integer pixel position
 * outside the image buffer.
 * This class is templated over the input image type and the
 * coordinate representation type (e.g. float or double ).
 *
 * \warning This hierarchy of functions work only for images
 * with scalar pixel types.
 *
 * \ingroup ImageFunctions
 *
 *
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TCoordRep = float >
class ExtrapolateImageFunction:
  public ImageFunction< TInputImage,
                        typename NumericTraits< typename TInputImage::PixelType >::RealType, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef ExtrapolateImageFunction                        Self;
  typedef ImageFunction< TInputImage,
                         typename NumericTraits< typename TInputImage::PixelType >::RealType,
                         TCoordRep >                      Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtrapolateImageFunction, ImageFunction);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** RealType typedef support. */
  typedef typename NumericTraits< typename TInputImage::PixelType >::RealType RealType;

  /** Extrapolate the image at a point position
   *
   * Returns the extrapolated image intensity at a
   * specified point position.
   */
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point, index);
    return ( this->EvaluateAtContinuousIndex(index) );
  }

  /** Extrapolate the image at a continuous index position
   *
   * Returns the extrapolated image intensity at a
   * specified point position.
   */
  virtual OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const ITK_OVERRIDE = 0;

  /** Extrapolate the image at an index position.
   *
   * Returns the extrapolated image intensity at a
   * specified point position.
   */
  virtual OutputType EvaluateAtIndex(
    const IndexType & index) const ITK_OVERRIDE = 0;

protected:
  ExtrapolateImageFunction(){}
  ~ExtrapolateImageFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { Superclass::PrintSelf(os, indent); }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExtrapolateImageFunction);
};
} // end namespace itk

#endif
