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
 * principal compoenets. The covariance matrix has a dimension of p x p. Since
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
 * \wiki
 * \wikiexample{Segmentation/EstimatePCAModel,Compute a PCA shape model from a training sample}
 * \endwiki
 */

template< typename TInputImage,
          typename TOutputImage = Image< double, TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT ImagePCAShapeModelEstimator:
  public ImageShapeModelEstimatorBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ImagePCAShapeModelEstimator                               Self;
  typedef ImageShapeModelEstimatorBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                      Pointer;
  typedef SmartPointer< const Self >                                ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImagePCAShapeModelEstimator, ImageShapeModelEstimatorBase);

  /** Type definition for the input image. */
  typedef TInputImage                        InputImageType;
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Type definition for the input image iterator type. */
  typedef ImageRegionIterator< TInputImage >      InputImageIterator;
  typedef ImageRegionConstIterator< TInputImage > InputImageConstIterator;

  /** Input Image dimension */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Type definition for the output image */
  typedef TOutputImage                   OutputImageType;
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Type definition for the input image iterator type. */
  typedef ImageRegionIterator< TOutputImage > OutputImageIterator;

  /** Type definition for a double matrix. */
  typedef vnl_matrix< double > MatrixOfDoubleType;

  /** Type definition for an integer vector. */
  typedef vnl_matrix< int > MatrixOfIntegerType;

  /** Type definition for a double vector. */
  typedef vnl_vector< double > VectorOfDoubleType;

  /** Set/Get the number of required largest principal components. The
   * filter produces the required number of principal components plus
   * one outputs. Output index 0 represents the mean image and the
   * remaining outputs the requested principal components. */
  virtual void SetNumberOfPrincipalComponentsRequired(unsigned int n);

  itkGetConstMacro(NumberOfPrincipalComponentsRequired, unsigned int);

  /** Set/Get the number of training images in the input. */
  virtual void SetNumberOfTrainingImages(unsigned int n);

  itkGetConstMacro(NumberOfTrainingImages, unsigned int);

  /** Get the eigen values */
  itkGetConstMacro(EigenValues, VectorOfDoubleType);

protected:
  ImagePCAShapeModelEstimator();
  ~ImagePCAShapeModelEstimator() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This filter must produce all of the outputs at once, as such it
   * must override the EnlargeOutputRequestedRegion method to enlarge the
   * output request region. */
  virtual void EnlargeOutputRequestedRegion(DataObject *) ITK_OVERRIDE;

  /** This filter requires all the input image at once, as such it
   * must override the GenerateInputRequestedRegion method. Additionally,
   * this filter assumes that the input images are at least the size as
   * the first input image. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Starts the image modelling process */
  void GenerateData() ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(ImagePCAShapeModelEstimator);

  /** Local variable typedefs */
  typedef std::vector< InputImageConstPointer >  InputImagePointerArray;
  typedef std::vector< InputImageConstIterator > InputImageIteratorArray;

  typedef typename TInputImage::SizeType ImageSizeType;

  /** Set up the vector to store the image  data. */
  typedef typename TInputImage::PixelType InputPixelType;

  /** Local functions */

  /** A function that generates the cluster centers (model) corresponding to the
   * estimates of the cluster centers (in the initial codebook).
   * If no codebook is provided, then use the number of classes to
   * determine the cluster centers or the Shape model. This is the
   * the base function to call the K-means classifier. */

  virtual void EstimateShapeModels() ITK_OVERRIDE;

  void EstimatePCAShapeModelParameters();

  void CalculateInnerProduct();

  /** Local storage variables */
  InputImageIteratorArray m_InputImageIteratorArray;

  VectorOfDoubleType m_Means;

  MatrixOfDoubleType m_InnerProduct;

  MatrixOfDoubleType m_EigenVectors;

  VectorOfDoubleType m_EigenValues;

  VectorOfDoubleType m_EigenVectorNormalizedEnergy;

  ImageSizeType m_InputImageSize;

  unsigned int m_NumberOfPixels;

  // The number of input images for PCA
  unsigned int m_NumberOfTrainingImages;

  // The number of output Principal Components
  unsigned int m_NumberOfPrincipalComponentsRequired;
}; // class ImagePCAShapeModelEstimator
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImagePCAShapeModelEstimator.hxx"
#endif

#endif
