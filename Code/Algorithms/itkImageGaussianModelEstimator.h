/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageGaussianModelEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageGaussianModelEstimator_h
#define _itkImageGaussianModelEstimator_h

#include <math.h>
#include <float.h>

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_math.h"
#include "vnl/algo/vnl_matrix_inverse.h"


#include "itkImageRegionIterator.h"
#include "itkExceptionObject.h"

#include "itkImageModelEstimatorBase.h"

namespace itk
{

/** \class ImageGaussianModelEstimator
 * \brief Base class for ImageGaussianModelEstimator object
 *
 * itkImageGaussianModelEstimator generated the gaussian model for given
 * tissue types (or class types) in an input training set. 
 * training data set for segmentation. The training data set is typically 
 * provided as a set of labelled/classified data set by the user. A gaussian 
 * model is generated for each label present in the training data set.
 * from the training data set.  
 *
 * The user should ensure that both the input and training images
 * are of the same size. The input data consists of the raw data and the
 * training data has class labels associated with each pixel. However, only
 * a subset of the data need to be labelled. Unlabelled data could be 
 * represented by a non zero, non positive number. The training data are 
 * analysed for identifying the classes. Any non zero, non negative value is 
 * considered a valid label. It is important that the maximum value of the 
 * training label be equal to N, where N is the number of classes represented
 * by the maximum label value in the training data set. The pixels 
 * corresponding to each training label is parsed and the mean and covariance
 * is calculated for each class.
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
 * specified. In this case a Membership function that store Gaussian models
 * needs to be specified.
 *
 * The function EstimateModels() calculated the various models, creates the 
 * membership function objects and populates them.
 *
 * \ingroup ClassificationFilters 
 */
template <class TInputImage, 
          class TMembershipFunction,
          class TTrainingImage>
class ITK_EXPORT ImageGaussianModelEstimator: 
    public ImageModelEstimatorBase<TInputImage, TMembershipFunction>
{
public:
  /** Standard class typedefs. */
  typedef ImageGaussianModelEstimator   Self;
  typedef ImageModelEstimatorBase<TInputImage,TMembershipFunction> Superclass;

  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageGaussianModelEstimator, ImageModelEstimatorBase);

  /** Type definition for the input image. */
  typedef typename TInputImage::Pointer   InputImagePointer;
 
  /** Type definitions for the training image. */
  typedef typename TTrainingImage::Pointer TrainingImagePointer;
    
  /** Type definition for the vector associated with
   * input image pixel type. */     
  typedef typename TInputImage::PixelType InputImagePixelType;        

  /** Type definitions for the vector holding
   * training image pixel type. */
  typedef typename TTrainingImage::PixelType TrainingImagePixelType;

  /** Type definitions for the iterators for the input and training images. */
  typedef
  ImageRegionIterator< TInputImage >  InputImageIterator;
  typedef
  ImageRegionIterator< TTrainingImage > TrainingImageIterator;

  /** Type definitions for the membership function . */
  typedef typename TMembershipFunction::Pointer MembershipFunctionPointer ;

  /** Set the training image. */
  itkSetMacro(TrainingImage,TrainingImagePointer);

  /** Get the training image. */
  itkGetMacro(TrainingImage,TrainingImagePointer);



protected: 
  ImageGaussianModelEstimator();
  ~ImageGaussianModelEstimator();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Starts the image modelling process */
  void GenerateData() ;

private:
  ImageGaussianModelEstimator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typedef vnl_matrix<double> MatrixType; 
  typedef vnl_vector<double> VectorType;

  typedef typename TInputImage::SizeType InputImageSizeType;

  /** Dimension of the each individual pixel vector. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      InputImagePixelType::Dimension);
  typedef vnl_matrix_fixed<double,1,itkGetStaticConstMacro(VectorDimension)> ColumnVectorType;

  MatrixType            m_NumberOfSamples;
  MatrixType            m_Means;
  MatrixType            *m_Covariance;  

  TrainingImagePointer  m_TrainingImage;

  /** A function that generates the 
   * model based on the training input data
   * Achieves the goal of training the classifier. */
  virtual void EstimateModels();

  void EstimateGaussianModelPrameters();

}; // class ImageGaussianModelEstimator

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageGaussianModelEstimator.txx"
#endif



#endif
