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
#ifndef itkImageGaussianModelEstimator_h
#define itkImageGaussianModelEstimator_h

#include <cmath>
#include <cfloat>

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_matrix_fixed.h"
#include "itkMath.h"
#include "vnl/algo/vnl_matrix_inverse.h"

#include "itkImageRegionIterator.h"
#include "itkMacro.h"

#include "itkImageModelEstimatorBase.h"

namespace itk
{
/**
 *\class ImageGaussianModelEstimator
 * \brief Base class for ImageGaussianModelEstimator object.
 *
 * itkImageGaussianModelEstimator generates the Gaussian model for given
 * tissue types (or class types) in an input training data set for
 * segmentation. The training data set is typically provided as a set of
 * labelled/classified data set by the user. A Gaussian model is generated
 * for each label present in the training data set.
 *
 * The user should ensure that both the input and training images
 * are of the same size. The input data consists of the raw data and the
 * training data has class labels associated with each pixel.
 *
 * A zero label is used to identify the background. A model is not
 * calculated for the background (its mean and covariance will be
 * zero). Positive labels are classes for which models will be
 * estimated. Negative labels indicate unlabeled data where no models
 * will be estimated.
 *
 * This object supports data handling of multiband images. The object
 * accepts the input image in vector format only, where each pixel is a
 * vector and each element of the vector corresponds to an entry from
 * 1 particular band of a multiband dataset. A single band image is treated
 * as a vector image with a single element for every vector. The classified
 * image is treated as a single band scalar image.
 *
 * This function is templated over the type of input and output images. In
 * addition, a third parameter for the MembershipFunction needs to be
 * specified. In this case a Membership function that stores Gaussian models
 * needs to be specified.
 *
 * The function EstimateModels() calculates the various models, creates the
 * membership function objects and populates them.
 *
 * \ingroup ClassificationFilters
 * \ingroup ITKClassifiers
 */
template <typename TInputImage, typename TMembershipFunction, typename TTrainingImage>
class ITK_TEMPLATE_EXPORT ImageGaussianModelEstimator : public ImageModelEstimatorBase<TInputImage, TMembershipFunction>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ImageGaussianModelEstimator);

  /** Standard class type aliases. */
  using Self = ImageGaussianModelEstimator;
  using Superclass = ImageModelEstimatorBase<TInputImage, TMembershipFunction>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageGaussianModelEstimator, ImageModelEstimatorBase);

  /** Type definition for the input image. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definitions for the training image. */
  using TrainingImageType = TTrainingImage;
  using TrainingImagePointer = typename TTrainingImage::Pointer;
  using TrainingImageConstPointer = typename TTrainingImage::ConstPointer;

  /** Type definition for the vector associated with
   * input image pixel type. */
  using InputImagePixelType = typename TInputImage::PixelType;

  /** Type definitions for the vector holding
   * training image pixel type. */
  using TrainingImagePixelType = typename TTrainingImage::PixelType;

  /** Type definitions for the iterators for the input and training images. */
  using InputImageIterator = ImageRegionIterator<TInputImage>;
  using InputImageConstIterator = ImageRegionConstIterator<TInputImage>;
  using TrainingImageIterator = ImageRegionIterator<TTrainingImage>;
  using TrainingImageConstIterator = ImageRegionConstIterator<TTrainingImage>;

  /** Type definitions for the membership function . */
  using MembershipFunctionType = TMembershipFunction;
  using MembershipFunctionPointer = typename TMembershipFunction::Pointer;

  /** Get/Set the training image. */
  itkSetObjectMacro(TrainingImage, TrainingImageType);
  itkGetModifiableObjectMacro(TrainingImage, TrainingImageType);

protected:
  ImageGaussianModelEstimator() = default;
  ~ImageGaussianModelEstimator() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Starts the image modelling process */
  void
  GenerateData() override;

private:
  using MatrixType = vnl_matrix<double>;

  using InputImageSizeType = typename TInputImage::SizeType;

  /** Dimension of each individual pixel vector. */
  static constexpr unsigned int VectorDimension = InputImagePixelType::Dimension;

  MatrixType   m_NumberOfSamples;
  MatrixType   m_Means;
  MatrixType * m_Covariance{ nullptr };

  TrainingImagePointer m_TrainingImage;

  /** A function that generates the
   * model based on the training input data.
   * Achieves the goal of training the classifier. */
  void
  EstimateModels() override;

  void
  EstimateGaussianModelParameters();
}; // class ImageGaussianModelEstimator
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageGaussianModelEstimator.hxx"
#endif

#endif
