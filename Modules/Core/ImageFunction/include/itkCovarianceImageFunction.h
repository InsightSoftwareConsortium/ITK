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
#ifndef itkCovarianceImageFunction_h
#define itkCovarianceImageFunction_h

#include "itkImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class CovarianceImageFunction
 * \brief Calculate the covariance matrix in the neighborhood of a pixel in a Vector image.
 *
 * Calculate the covariance matrix  over the standard 8, 26, etc. connected
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
template< typename TInputImage, typename TCoordRep = float >
class ITK_TEMPLATE_EXPORT CovarianceImageFunction:
  public ImageFunction< TInputImage,
                        vnl_matrix<
                          typename NumericTraits< typename TInputImage::PixelType::ValueType >::RealType >,
                        TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef CovarianceImageFunction Self;
  typedef ImageFunction< TInputImage,
                         vnl_matrix<
                           typename NumericTraits< typename TInputImage::PixelType::ValueType >::RealType >,
                         TCoordRep >                     Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CovarianceImageFunction, ImageFunction);

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

  /** Datatype used for the covariance matrix */
  typedef vnl_matrix<
    typename NumericTraits< typename InputImageType::PixelType::ValueType >::RealType >
  RealType;

  /** Evalulate the function at specified index */
  virtual RealType EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE;

  /** Evaluate the function at non-integer positions */
  virtual RealType Evaluate(const PointType & point) const ITK_OVERRIDE
  {
    IndexType index;

    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex(index);
  }

  virtual RealType EvaluateAtContinuousIndex(
    const ContinuousIndexType & cindex) const ITK_OVERRIDE
  {
    IndexType index;

    this->ConvertContinuousIndexToNearestIndex(cindex, index);
    return this->EvaluateAtIndex(index);
  }

  /** Get/Set the radius of the neighborhood over which the
   *  statistics are evaluated. */
  itkSetMacro(NeighborhoodRadius, unsigned int);
  itkGetConstReferenceMacro(NeighborhoodRadius, unsigned int);

protected:
  CovarianceImageFunction();
  ~CovarianceImageFunction() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CovarianceImageFunction);

  unsigned int m_NeighborhoodRadius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovarianceImageFunction.hxx"
#endif

#endif
