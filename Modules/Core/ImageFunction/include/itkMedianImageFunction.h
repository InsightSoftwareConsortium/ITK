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
#ifndef itkMedianImageFunction_h
#define itkMedianImageFunction_h

#include "itkImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class MedianImageFunction
 * \brief Calculate the median value in the neighborhood of a pixel
 *
 * Calculate the median pixel value over the standard 8, 26, etc. connected
 * neighborhood.  This calculation uses a ZeroFluxNeumannBoundaryCondition.
 *
 * If called with a ContinuousIndex or Point, the calculation is performed
 * at the nearest neighbor.
 *
 * This class is templated over the input image type and the
 * coordinate representation type (e.g. float or double ).
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 *
 * \wiki
 * \wikiexample{Functions/MedianImageFunction,Compute the median of an image at a pixels (in a regular neighborhood)}
 * \endwiki
 */
template< typename TInputImage, typename TCoordRep = float >
class ITK_TEMPLATE_EXPORT MedianImageFunction:
  public ImageFunction< TInputImage, typename TInputImage::PixelType,
                        TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef MedianImageFunction Self;
  typedef ImageFunction< TInputImage, typename TInputImage::PixelType,
                         TCoordRep >                     Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MedianImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage                         InputImageType;
  typedef typename Superclass::InputPixelType InputPixelType;

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Evalulate the function at specified index */
  OutputType EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at non-integer positions */
  OutputType Evaluate(const PointType & point) const override
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex(index);
  }

  OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex) const override
  {
    IndexType index;

    this->ConvertContinuousIndexToNearestIndex(cindex, index);
    return this->EvaluateAtIndex(index);
  }

  /** Get/Set the radius of the neighborhood over which the
      statistics are evaluated */
  itkSetMacro( NeighborhoodRadius, unsigned int );
  itkGetConstReferenceMacro( NeighborhoodRadius, unsigned int );

protected:
  MedianImageFunction();
  ~MedianImageFunction() override {}
  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MedianImageFunction);

  unsigned int m_NeighborhoodRadius;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMedianImageFunction.hxx"
#endif

#endif
