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
/** \class ImageGaussianModelEstimator
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
 * calcualted for the background (its mean and covariance will be
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
template< typename TInputImage,
          typename TMembershipFunction,
          typename TTrainingImage >
class ITK_TEMPLATE_EXPORT ImageGaussianModelEstimator:
  public ImageModelEstimatorBase< TInputImage, TMembershipFunction >
{
public:
  /** Standard class typedefs. */
  typedef ImageGaussianModelEstimator                                 Self;
  typedef ImageModelEstimatorBase< TInputImage, TMembershipFunction > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageGaussianModelEstimator, ImageModelEstimatorBase);

  /** Type definition for the input image. */
  typedef TInputImage                        InputImageType;
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** Type definitions for the training image. */
  typedef TTrainingImage                        TrainingImageType;
  typedef typename TTrainingImage::Pointer      TrainingImagePointer;
  typedef typename TTrainingImage::ConstPointer TrainingImageConstPointer;

  /** Type definition for the vector associated with
   * input image pixel type. */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Type definitions for the vector holding
   * training image pixel type. */
  typedef typename TTrainingImage::PixelType TrainingImagePixelType;

  /** Type definitions for the iterators for the input and training images. */
  typedef ImageRegionIterator< TInputImage >         InputImageIterator;
  typedef ImageRegionConstIterator< TInputImage >    InputImageConstIterator;
  typedef ImageRegionIterator< TTrainingImage >      TrainingImageIterator;
  typedef ImageRegionConstIterator< TTrainingImage > TrainingImageConstIterator;

  /** Type definitions for the membership function . */
  typedef TMembershipFunction                   MembershipFunctionType;
  typedef typename TMembershipFunction::Pointer MembershipFunctionPointer;

  /** Get/Set the training image. */
  itkSetObjectMacro(TrainingImage, TrainingImageType);
  itkGetModifiableObjectMacro(TrainingImage, TrainingImageType);

protected:
  ImageGaussianModelEstimator();
  ~ImageGaussianModelEstimator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Starts the image modelling process */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageGaussianModelEstimator);

  typedef vnl_matrix< double > MatrixType;

  typedef typename TInputImage::SizeType InputImageSizeType;

  /** Dimension of each individual pixel vector. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      InputImagePixelType::Dimension);

  MatrixType  m_NumberOfSamples;
  MatrixType  m_Means;
  MatrixType *m_Covariance;

  TrainingImagePointer m_TrainingImage;

  /** A function that generates the
   * model based on the training input data.
   * Achieves the goal of training the classifier. */
  virtual void EstimateModels() ITK_OVERRIDE;

  void EstimateGaussianModelParameters();
}; // class ImageGaussianModelEstimator
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageGaussianModelEstimator.hxx"
#endif

#endif
