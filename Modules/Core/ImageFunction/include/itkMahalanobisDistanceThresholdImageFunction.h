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
#ifndef itkMahalanobisDistanceThresholdImageFunction_h
#define itkMahalanobisDistanceThresholdImageFunction_h

#include "itkImageFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"

namespace itk
{
/**
 *\class MahalanobisDistanceThresholdImageFunction
 * \brief Returns true if the pixel value of a vector image has a
 * Mahalanobis distance below the value specified by the threshold.
 *
 * This ImageFunction returns true if the pixel value of a vector image has a
 * Mahalanobis distance below the value specified by the threshold. The
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
template <typename TInputImage, typename TCoordRep = float>
class ITK_TEMPLATE_EXPORT MahalanobisDistanceThresholdImageFunction : public ImageFunction<TInputImage, bool, TCoordRep>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MahalanobisDistanceThresholdImageFunction);

  /** Standard class type aliases. */
  using Self = MahalanobisDistanceThresholdImageFunction;
  using Superclass = ImageFunction<TInputImage, bool, TCoordRep>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MahalanobisDistanceThresholdImageFunction, ImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** InputImageType type alias support */
  using InputImageType = typename Superclass::InputImageType;

  /** Typedef to describe the type of pixel. */
  using PixelType = typename TInputImage::PixelType;

  /** Dimension underlying input image. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** ContinuousIndex type alias support */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Type used to represent the Covariance matrix of the vector population. */
  using CovarianceMatrixType = vnl_matrix<double>;

  /** Type used to represent the Mean Vector of the vector population. */
  using MeanVectorType = vnl_vector<double>;

  /** BinaryThreshold the image at a point position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria. The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  bool
  Evaluate(const PointType & point) const override;

  /** BinaryThreshold the image at a continuous index position
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria. The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  bool
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override;

  /** BinaryThreshold the image at an index position.
   *
   * Returns true if the image intensity at the specified point position
   * satisfies the threshold criteria.  The point is assumed to lie within
   * the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  bool
  EvaluateAtIndex(const IndexType & index) const override;

  /** Returns the actual value of the MahalanobisDistance at that point.
   * The point is assumed to lie within the image buffer.
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual double
  EvaluateDistance(const PointType & point) const;

  /** Returns the actual value of the MahalanobisDistance at that index.
   * The point is assumed to lie within the image buffer.
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual double
  EvaluateDistanceAtIndex(const IndexType & index) const;

  /** Get the lower threshold value. */
  itkGetConstReferenceMacro(Threshold, double);
  itkSetMacro(Threshold, double);

  /** Set the mean.
   * Set this mean value to the membership function. */
  void
  SetMean(const MeanVectorType & mean);

  /** Get the mean.
   * The mean set on the membership function matches this value. */
  itkGetConstReferenceMacro(Mean, MeanVectorType);

  /** Set the covariance matrix.
   * Set this covariance matrix to the membership function. */
  void
  SetCovariance(const CovarianceMatrixType & covariance);

  /** Get the covariance matrix.
   * The covariance matrix set on the membership function matches this value. */
  itkGetConstReferenceMacro(Covariance, CovarianceMatrixType);

protected:
  MahalanobisDistanceThresholdImageFunction();
  ~MahalanobisDistanceThresholdImageFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_Threshold;

  // This is intended only for Image of Vector pixel type.
  using MahalanobisDistanceFunctionType = Statistics::MahalanobisDistanceMembershipFunction<PixelType>;

  using MahalanobisDistanceFunctionPointer = typename MahalanobisDistanceFunctionType::Pointer;
  MahalanobisDistanceFunctionPointer m_MahalanobisDistanceMembershipFunction;

  // Cached versions of the mean and covariance to manage the
  // difference in vector/matrix types between this class and the
  // membership function used internally.
  MeanVectorType       m_Mean;
  CovarianceMatrixType m_Covariance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMahalanobisDistanceThresholdImageFunction.hxx"
#endif

#endif
