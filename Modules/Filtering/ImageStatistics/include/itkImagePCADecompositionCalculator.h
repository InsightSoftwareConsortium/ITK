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
#ifndef itkImagePCADecompositionCalculator_h
#define itkImagePCADecompositionCalculator_h

#include "itkObject.h"
#include "itkImagePCAShapeModelEstimator.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class ImagePCADecompositionCalculator
 * \brief Decomposes an image into directions along basis components.
 *
 * This calculator computes the projection of an image into a subspace specified
 * by some basis set of images, and, optionally, a mean image (e.g a translation
 * to a new origin).
 * Typically, this basis/mean image will be the mean and principal components of
 * an image data set, as calculated by an ImagePCAShapeModelEstimator. The output
 * of the calculator is a vnl_vector containing the coefficients along each
 * dimension of the provided basis set.
 * To use this calculator, set the basis images with the SetBasisImage method, and
 * optionally set the mean image with the SetMeanImage method.
 * In the PCA case, the zeroth output of the ImagePCAShapeModelEstimator is the
 * mean image and subsequent outputs are the basis images.
 * SetBasisFromModel is a convenience method to set all of this information from
 * a given ImagePCAShapeModelEstimator instance.
 *
 * This class is templated over the input image type and the type of images
 * used to describe the basis.
 *
 * \warning This method assumes that the input image consists of scalar pixel
 * types.
 *
 * \warning All images (input, basis, and mean) must be the same size.
 *
 * \author Zachary Pincus
 *
 * \ingroup Operators
 * \ingroup ITKImageStatistics
 */
template< typename TInputImage,
          typename TBasisImage = Image< double, TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT ImagePCADecompositionCalculator:public Object
{
public:
  /** Standard class typedefs. */
  typedef ImagePCADecompositionCalculator Self;
  typedef Object                          Superclass;
  typedef SmartPointer< Self >            Pointer;
  typedef SmartPointer< const Self >      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImagePCADecompositionCalculator, Object);

  /** Type definitions for the input images. */
  typedef TInputImage InputImageType;
  typedef TBasisImage BasisImageType;

  /** Pointer types for the image. */
  typedef typename TInputImage::Pointer InputImagePointer;
  typedef typename TBasisImage::Pointer BasisImagePointer;

  /** Const Pointer type for the image. */
  typedef typename TInputImage::ConstPointer InputImageConstPointer;
  typedef typename TBasisImage::ConstPointer BasisImageConstPointer;

  /** Basis image pixel type: this is also the type of the optput vector */
  typedef typename TBasisImage::PixelType BasisPixelType;
  /** Input Image dimension */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Basis Image dimension */
  itkStaticConstMacro(BasisImageDimension, unsigned int,
                      TBasisImage::ImageDimension);

  /** Vector of basis image pointers. */
  typedef std::vector< BasisImagePointer > BasisImagePointerVector;

  /** Type definitions for internal vectors and matrices */
  typedef vnl_matrix< BasisPixelType > BasisMatrixType;
  typedef vnl_vector< BasisPixelType > BasisVectorType;

  /** Set and get the input image. */
  itkSetConstObjectMacro(Image, InputImageType);
  itkGetConstObjectMacro(Image, InputImageType);

  /** Set and get the mean image. */
  itkSetConstObjectMacro(MeanImage, BasisImageType);
  itkGetConstObjectMacro(MeanImage, BasisImageType);

  /** Set and get the basis images. */
  void SetBasisImages(const BasisImagePointerVector &);

  BasisImagePointerVector GetBasisImages() { return m_BasisImages; }

  /** Type definition of a compatible ImagePCAShapeModelEstimator */
  typedef typename ImagePCAShapeModelEstimator< TInputImage,
                                                TBasisImage >::Pointer ModelPointerType;
  /** Set the basis images from a ImagePCAShapeModelEstimator */
  void SetBasisFromModel(ModelPointerType model);

  /** Compute the PCA decomposition of the input image. */
  void Compute();

  /** Return the projection of the image. */
  itkGetConstMacro(Projection, BasisVectorType);

protected:
  ImagePCADecompositionCalculator();
  virtual ~ImagePCADecompositionCalculator() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void CalculateBasisMatrix();

  void CalculateRecenteredImageAsVector();

private:
  typedef typename BasisImageType::SizeType BasisSizeType;

  ITK_DISALLOW_COPY_AND_ASSIGN(ImagePCADecompositionCalculator);

  BasisVectorType         m_Projection;
  BasisVectorType         m_ImageAsVector;
  BasisImagePointerVector m_BasisImages;
  BasisImageConstPointer  m_MeanImage;
  BasisSizeType           m_Size;
  InputImageConstPointer  m_Image;
  BasisMatrixType         m_BasisMatrix;
  bool                    m_BasisMatrixCalculated;
  SizeValueType           m_NumPixels;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImagePCADecompositionCalculator.hxx"
#endif

#endif
