/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImagePCAShapeModelEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImagePCAShapeModelEstimator_h
#define _itkImagePCAShapeModelEstimator_h

#include <time.h>
#include <math.h>
#include <float.h>

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_math.h"
#include "vnl/algo/vnl_matrix_inverse.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkExceptionObject.h"

#include "itkImageShapeModelEstimatorBase.h"
#include "itkConceptChecking.h"
#include "itkImage.h"
#include <vnl/algo/vnl_generalized_eigensystem.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>

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
 * largest principal components plus 1 a(for the mean image).
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
 * \ingroup ImageFeatureExtraction */ 

template <class TInputImage, 
          class TOutputImage= Image<double, ::itk::GetImageDimension<TInputImage>::ImageDimension> >
class ITK_EXPORT ImagePCAShapeModelEstimator: 
    public ImageShapeModelEstimatorBase<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ImagePCAShapeModelEstimator   Self;
  typedef ImageShapeModelEstimatorBase<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImagePCAShapeModelEstimator, ImageShapeModelEstimatorBase);

  /** Type definition for the input image. */
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::Pointer         InputImagePointer;
  typedef typename TInputImage::ConstPointer    InputImageConstPointer;

  /** Type definition for the input image pixel type. */     
  typedef typename TInputImage::PixelType   InputImagePixelType;

  /** Type definition for the input image iterator type. */
  typedef 
    ImageRegionIterator<TInputImage> InputImageIterator;

  typedef 
    ImageRegionConstIterator<TInputImage> InputImageConstIterator;    

  /** Input Image dimension */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension); 

  /** Type definition for the output image */
  typedef TOutputImage                    OutputImageType;
  typedef typename TOutputImage::Pointer  OutputImagePointer;

  /** Type definition for the input image iterator type. */
  typedef 
    ImageRegionIterator<TOutputImage> OutputImageIterator;

  /** Type definition for a double matrix. */
  typedef vnl_matrix<double> MatrixOfDoubleType; 

  /** Type definition for an integer vector. */
  typedef vnl_matrix<int>    MatrixOfIntegerType;

  /** Type definition for a double vector. */
  typedef vnl_vector<double> VectorOfDoubleType;

  /** Set/Get the number of input training images. The filter produces the
   same number of output images. The Set method modifies this filter's
   NumberOfRequiredInputs and NumberOfRequiredOutputs and calls MakeOutput
   to create the extra outputs. */
  void SetNumberOfPrincipalComponentsRequired( unsigned int n );
  itkGetMacro( NumberOfPrincipalComponentsRequired, unsigned int );

  /** Set/Get the number of training images in the input */
  itkSetMacro(NumberOfTrainingImages, unsigned int);
  itkGetMacro(NumberOfTrainingImages, unsigned int);

  /** Get the eigen values */
  itkGetMacro(EigenValues, VectorOfDoubleType);  

  /** Execute the function (over-ride the base clase update function)*/
  //void Update();

protected: 
  ImagePCAShapeModelEstimator();
  ~ImagePCAShapeModelEstimator();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** This filter must produce all of the outputs at once, as such it
   * must override the EnlargeOutputRequestedRegion method to enlarge the 
   * output request region. */
  virtual void EnlargeOutputRequestedRegion( DataObject * );

  /** This filter requires all the input image at once, as such it
   * must override the GenerateInputRequestedRegion method. Additionally,
   * this filter assumes that the input images are at least the size as
   * the first input image. */
  virtual void GenerateInputRequestedRegion();

  /** Starts the image modelling process */
  void GenerateData() ;

private:

  ImagePCAShapeModelEstimator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Local variable typedefs */
  typedef 
    std::vector<InputImageConstPointer> InputImagePointerArray;
  typedef
    ImageRegionConstIterator< InputImageType > InputImageConstIterator;
  typedef
    std::vector< InputImageConstIterator > InputImageIteratorArray;

  typedef typename TInputImage::SizeType ImageSizeType;

  /** Set up the vector to store the image  data. */
  typedef typename TInputImage::PixelType InputPixelType;

  /** Local functions */

  /** A function that generates the cluster centers (model) corresponding to the 
   * estimates of the cluster centers (in the initial codebook).
   * If no codebook is provided, then use the number of classes to 
   * determine the cluster centers or the Shape model. This is the
   * the base function to call the K-means classifier. */

  virtual void EstimateShapeModels();

  void EstimatePCAShapeModelPrameters();

  void CalculateInnerProduct();

  /** Local storage variables */
  InputImageIteratorArray m_InputImageIteratorArray;

  VectorOfDoubleType    m_Means; 

  MatrixOfDoubleType    m_InnerProduct;

  MatrixOfDoubleType    m_EigenVectors;

  VectorOfDoubleType    m_EigenValues;

  VectorOfDoubleType    m_EigenVectorNormalizedEnergy;

  ImageSizeType         m_InputImageSize;

  unsigned int          m_NumberOfPixels;

  // The number of input images for PCA
  unsigned int          m_NumberOfTrainingImages;

  // The number of output Pricipal Components
  unsigned int          m_NumberOfPrincipalComponentsRequired;
  
}; // class ImagePCAShapeModelEstimator


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImagePCAShapeModelEstimator.txx"
#endif



#endif
