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
#ifndef itkImagePCAShapeModelEstimator_h
#define itkImagePCAShapeModelEstimator_h

#include <ctime>
#include <cmath>
#include <cfloat>

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "itkMath.h"
#include "vnl/algo/vnl_matrix_inverse.h"

#include "itkImageRegionIterator.h"
#include "itkMacro.h"

#include "itkImageShapeModelEstimatorBase.h"
#include "itkConceptChecking.h"
#include "itkImage.h"
#include "vnl/algo/vnl_generalized_eigensystem.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"

namespace itk
{
/** \class ImagePCAShapeModelEstimator
 * \brief Base class for ImagePCAShapeModelEstimator object
 *
 * itkImagePCAShapeModelEstimator performs a principal component analysis
 * (PCA) on a set of images. The user specifies the number of training images
 * and also the number of desired largest principal components needed.
 * The ITK pipeline mechanism sets up the storage for both input and output
 * images. The number of output images are the user specified number of desired
 * largest principal components plus 1 (for the mean image).
 *
 * The algorithm uses the VNL library to perform the eigen analysis. To speed
 * the computation of the instead of performing the eigen analysis of the
 * covariance vector A*A' where A is a matrix with p x t, p = number of
 * pixels or voxels in each images and t = number of training images, we
 * calculate the eigen vectors of the inner product matrix A'*A. The resulting
 * eigen vectors (E) are then multiplied with the the matrix A to get the
 * principal components. The covariance matrix has a dimension of p x p. Since
 * number of pixels in any image being typically very high the eigen
 * decomposition becomes computationally expensive. The inner product on the
 * other hand has the dimension of t x t, where t is typically much smaller
 * that p. Hence the eigen decomposition (most compute intensive part) is an
 * orders of magnitude faster.
 *
 * The Update() function enables the calculation of the various models, creates
 * the membership function objects and populates them.
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKImageStatistics
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageStatistics/ComputePCAShapeFromSample,Compute PCA Shape From Training Sample}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage = Image<double, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT ImagePCAShapeModelEstimator : public ImageShapeModelEstimatorBase<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImagePCAShapeModelEstimator);

  /** Standard class type aliases. */
  using Self = ImagePCAShapeModelEstimator;
  using Superclass = ImageShapeModelEstimatorBase<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImagePCAShapeModelEstimator, ImageShapeModelEstimatorBase);

  /** Type definition for the input image. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using InputImagePixelType = typename TInputImage::PixelType;

  /** Type definition for the input image iterator type. */
  using InputImageIterator = ImageRegionIterator<TInputImage>;
  using InputImageConstIterator = ImageRegionConstIterator<TInputImage>;

  /** Input Image dimension */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /** Type definition for the output image */
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Type definition for the input image iterator type. */
  using OutputImageIterator = ImageRegionIterator<TOutputImage>;

  /** Type definition for a double matrix. */
  using MatrixOfDoubleType = vnl_matrix<double>;

  /** Type definition for an integer vector. */
  using MatrixOfIntegerType = vnl_matrix<int>;

  /** Type definition for a double vector. */
  using VectorOfDoubleType = vnl_vector<double>;

  /** Set/Get the number of required largest principal components. The
   * filter produces the required number of principal components plus
   * one outputs. Output index 0 represents the mean image and the
   * remaining outputs the requested principal components. */
  virtual void
  SetNumberOfPrincipalComponentsRequired(unsigned int n);

  itkGetConstMacro(NumberOfPrincipalComponentsRequired, unsigned int);

  /** Set/Get the number of training images in the input. */
  virtual void
  SetNumberOfTrainingImages(unsigned int n);

  itkGetConstMacro(NumberOfTrainingImages, unsigned int);

  /** Get the eigen values */
  itkGetConstMacro(EigenValues, VectorOfDoubleType);

protected:
  ImagePCAShapeModelEstimator();
  ~ImagePCAShapeModelEstimator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This filter must produce all of the outputs at once, as such it
   * must override the EnlargeOutputRequestedRegion method to enlarge the
   * output request region. */
  void
  EnlargeOutputRequestedRegion(DataObject *) override;

  /** This filter requires all the input image at once, as such it
   * must override the GenerateInputRequestedRegion method. Additionally,
   * this filter assumes that the input images are at least the size as
   * the first input image. */
  void
  GenerateInputRequestedRegion() override;

  /** Starts the image modelling process */
  void
  GenerateData() override;

private:
  /** Local variable type alias */
  using InputImagePointerArray = std::vector<InputImageConstPointer>;
  using InputImageIteratorArray = std::vector<InputImageConstIterator>;

  using ImageSizeType = typename TInputImage::SizeType;

  /** Set up the vector to store the image  data. */
  using InputPixelType = typename TInputImage::PixelType;

  /** Local functions */

  /** A function that generates the cluster centers (model) corresponding to the
   * estimates of the cluster centers (in the initial codebook).
   * If no codebook is provided, then use the number of classes to
   * determine the cluster centers or the Shape model. This is the
   * the base function to call the K-means classifier. */

  void
  EstimateShapeModels() override;

  void
  EstimatePCAShapeModelParameters();

  void
  CalculateInnerProduct();

  /** Local storage variables */
  InputImageIteratorArray m_InputImageIteratorArray;

  VectorOfDoubleType m_Means;

  MatrixOfDoubleType m_InnerProduct;

  MatrixOfDoubleType m_EigenVectors;

  VectorOfDoubleType m_EigenValues;

  VectorOfDoubleType m_EigenVectorNormalizedEnergy;

  ImageSizeType m_InputImageSize;

  unsigned int m_NumberOfPixels{ 0 };

  // The number of input images for PCA
  unsigned int m_NumberOfTrainingImages{ 0 };

  // The number of output Principal Components
  unsigned int m_NumberOfPrincipalComponentsRequired;
}; // class ImagePCAShapeModelEstimator
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImagePCAShapeModelEstimator.hxx"
#endif

#endif
