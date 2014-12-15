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
#ifndef itkMahalanobisDistanceThresholdImageFunction_h
#define itkMahalanobisDistanceThresholdImageFunction_h

#include "itkImageFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"

namespace itk
{
/** \class MahalanobisDistanceThresholdImageFunction
 * \brief Returns true if the pixel value of a vector image has a
 * Mahalanobis distance below the value specified by the threshold.
 *
 * This ImageFunction returns true if the pixel value of a vector image has a
 * Mahalanobis distance below the value specided by the threshold. The
 * Mahalanobis distance is computed with the
 * MahalanobisDistanceMembershipFunction class which has to be initialized with
 * a mean and covariance. This class is intended to be used only
 * with images whose pixel type is a vector (array).
 *
 * The input image is set via method SetInputImage().
 *
 * Methods Evaluate, EvaluateAtIndex and EvaluateAtContinuousIndex respectively
 * evaluate the function at an geometric point, image index and continuous
 * image index.
 *
 * \ingroup ImageFunctions
 *
 *
 * \ingroup ITKImageFunction
 */
template< typename TInputImage, typename TCoordRep = float >
class MahalanobisDistanceThresholdImageFunction:
  public ImageFunction< TInputImage, bool, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef MahalanobisDistanceThresholdImageFunction     Self;
  typedef ImageFunction< TInputImage, bool, TCoordRep > Superclass;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MahalanobisDistanceThresholdImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType PixelType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Type used to represent the Covariance matrix of the vector population */
  typedef vnl_matrix< double > CovarianceMatrixType;

  /** Type used to represent the Mean Vector of the vector population */
  typedef vnl_vector< double > MeanVectorType;

  /** BinaryThreshold the image at a point position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool Evaluate(const PointType & point) const;

  /** BinaryThreshold the image at a continuous index position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const;

  /** BinaryThreshold the image at an index position.
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual bool EvaluateAtIndex(const IndexType & index) const;

  /**
   *
   * Returns the actual value of the MahalanobisDistance at that point.
   * The point is assumed to lie within the image buffer.
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual double EvaluateDistance(const PointType & point) const;

  /**
   *
   * Returns the actual value of the MahalanobisDistance at that Index.
   * The point is assumed to lie within the image buffer.
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual double EvaluateDistanceAtIndex(const IndexType & index) const;

  /** Get the lower threshold value. */
  itkGetConstReferenceMacro(Threshold, double);
  itkSetMacro(Threshold, double);

  /** Method to set mean */
  void SetMean(const MeanVectorType & mean);

  /** Method to get the mean. */
  const MeanVectorType & GetMean() const;

  /**
   * Method to set covariance matrix **/
  void SetCovariance(const CovarianceMatrixType & cov);

  /** Get the covariance matrix **/
  const CovarianceMatrixType & GetCovariance() const;

protected:
  MahalanobisDistanceThresholdImageFunction();
  ~MahalanobisDistanceThresholdImageFunction(){}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  MahalanobisDistanceThresholdImageFunction(const Self &); //purposely not
                                                           // implemented
  void operator=(const Self &);                            //purposely not

  // implemented

  double m_Threshold;

  // This is intended only for Image of Vector pixel type.
  typedef Statistics::MahalanobisDistanceMembershipFunction<
    PixelType
    >  MahalanobisDistanceFunctionType;

  typedef typename MahalanobisDistanceFunctionType::Pointer MahalanobisDistanceFunctionPointer;
  MahalanobisDistanceFunctionPointer m_MahalanobisDistanceMembershipFunction;

  // Cached versions of the mean and covariance to manage the
  // difference in vector/matrix types between this class and the
  // membership function used internally.
  MeanVectorType       m_Mean;
  CovarianceMatrixType m_Covariance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMahalanobisDistanceThresholdImageFunction.hxx"
#endif

#endif
