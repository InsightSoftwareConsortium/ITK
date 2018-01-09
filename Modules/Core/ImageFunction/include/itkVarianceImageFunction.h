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
#ifndef itkVarianceImageFunction_h
#define itkVarianceImageFunction_h

#include "itkImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class VarianceImageFunction
 * \brief Calculate the variance in the neighborhood of a pixel
 *
 * Calculate the variance pixel value over the standard 8, 26, etc. connected
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
 */
template< typename TInputImage, typename TCoordRep = float >
class ITK_TEMPLATE_EXPORT VarianceImageFunction:
  public ImageFunction< TInputImage, typename NumericTraits< typename TInputImage::PixelType >::RealType,
                        TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef VarianceImageFunction Self;
  typedef ImageFunction< TInputImage, typename NumericTraits< typename TInputImage::PixelType >::RealType,
                         TCoordRep > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(VarianceImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef TInputImage InputImageType;

  /** OutputType typdef support. */
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

  /** Datatype used for the variance */
  typedef typename NumericTraits< typename InputImageType::PixelType >::RealType
  RealType;

  /** Evalulate the function at specified index */
  RealType EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at non-integer positions */
  RealType Evaluate(const PointType & point) const override
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex(index);
  }

  RealType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex) const override
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
  VarianceImageFunction();
  ~VarianceImageFunction() override {}
  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VarianceImageFunction);

  unsigned int m_NeighborhoodRadius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVarianceImageFunction.hxx"
#endif

#endif
