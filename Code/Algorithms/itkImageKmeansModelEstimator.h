/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageKmeansModelEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageKmeansModelEstimator_h
#define _itkImageKmeansModelEstimator_h

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

#include "itkImageModelEstimatorBase.h"

#define  ONEBAND           1
#define  GLA_CONVERGED     1
#define  GLA_NOT_CONVERGED 2
#define  LBG_COMPLETED     3

namespace itk
{

/** \class ImageKmeansModelEstimator
 * \brief Base class for ImageKmeansModelEstimator object
 *
 * itkImageKmeansModelEstimator generated the gaussian model for given
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
 * anaysed for identifying the classes. Any non zero, non negative value is 
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
          class TMembershipFunction>
class ITK_EXPORT ImageKmeansModelEstimator: 
public ImageModelEstimatorBase<TInputImage, TMembershipFunction>
{
public:
  /** Standard class typedefs. */
  typedef ImageKmeansModelEstimator   Self;
  typedef ImageModelEstimatorBase<TInputImage, TMembershipFunction> Superclass;

  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageKmeansModelEstimator, ImageModelEstimatorBase);

  /** Type definition for the input image. */
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::Pointer         InputImagePointer;
  typedef typename TInputImage::ConstPointer    InputImageConstPointer;

  /** Type definition for the vector associated with
   * input image pixel type. */     
  typedef typename TInputImage::PixelType::VectorType    
    InputImageVectorType;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType     InputImagePixelType;

  /** Type definition for the input image iterator type. */
  typedef 
    ImageRegionIterator<TInputImage> InputImageIterator;

  typedef 
    ImageRegionConstIterator<TInputImage> InputImageConstIterator;     

  /** Type definitions for the membership function . */
  typedef typename TMembershipFunction::Pointer MembershipFunctionPointer ;

  /** Type definition for a double matrix. */
  typedef vnl_matrix<double> CodebookMatrixOfDoubleType; 

  /** Type definition for an integer vector. */
  typedef vnl_matrix<int>    CodebookMatrixOfIntegerType;

  /** Set the cluster centers. */
  void SetCodebook(CodebookMatrixOfDoubleType InCodebook);

  /** Get the cluster centers. */
  itkGetMacro(Codebook,CodebookMatrixOfDoubleType);

  /** Get the optimized codebook or the centroids of the clusters. */
  CodebookMatrixOfDoubleType GetOutCodebook()
    { return m_Codebook; }

  /** Set the threshold parameter. */
  itkSetMacro(Threshold,double);

  /** Get the threshold parameter. */
  itkGetMacro(Threshold,double);

  /** Set the offset add parameter. */
  itkSetMacro(OffsetAdd,double);

  /** Get the offset add parameter. */
  itkGetMacro(OffsetAdd,double);

  /** Set the offset multiplication parameter. */
  itkSetMacro(OffsetMultiply,double);

  /** Get the offset multiplication parameter. */
  itkGetMacro(OffsetMultiply,double);

  /** Set the maximum number of attempts to split a codeword. */
  itkSetMacro(MaxSplitAttempts,int);

  /** Get the manimum number of attempts to split a codeword. */
  itkGetMacro(MaxSplitAttempts,int);

  /** Return the codebook/cluster centers. */
  CodebookMatrixOfDoubleType GetKmeansResults()
    { return m_Centroid; }


  /** A function that generates the cluster centers (model) corresponding to the 
   * estimates of the cluster centers (in the initial codebook).
   * If no codebook is provided, then use the number of classes to 
   * determine the cluster centers or the Kmeans model. This is the
   * the base function to call the K-means classifier. */

  virtual void EstimateModels();

protected: 
  ImageKmeansModelEstimator();
  ~ImageKmeansModelEstimator();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Allocate memory for the output model. */
  void Allocate();

  /** Print out the results on the screen for visual feedback. */
  void PrintKmeansAlgorithmResults();
private:
  ImageKmeansModelEstimator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  void EstimateKmeansModelPrameters();

  typedef typename TInputImage::SizeType ImageSizeType;

  /** Set up the vector to store the image  data. */
  typedef typename TInputImage::PixelType::VectorType InputPixelVectorType;

  void Reallocate(int oldSize, int newSize);

  //Local functions
  int  WithCodebookUseGLA(); // GLA stands for the Generalized Lloyd Algorithm
  int  WithoutCodebookUseLBG(); //LBG stands for the Lindo Buzo Gray Algorithm

  void NearestNeighborSearchBasic(double *distortion);
  
  void SplitCodewords(int currentSize, 
                      int numDesired, 
                      int scale);
  
  void Perturb(double *oldCodeword, 
               int scale, 
               double *newCodeword);

  CodebookMatrixOfDoubleType  m_Codebook;

  // Buffer for K-means calcualtions
  CodebookMatrixOfDoubleType  m_Centroid;

  double              m_Threshold;
  double              m_OffsetAdd;
  double              m_OffsetMultiply;
  int                 m_MaxSplitAttempts;

  //unsigned long       m_NumberOfModels;
  bool                m_ValidInCodebook;
  double              m_DoubleMaximum;
  double              m_OutputDistortion;
  int                 m_OutputNumberOfEmptyCells;

  unsigned long       m_VectorDimension;
  unsigned long       m_NumberOfCodewords;
  unsigned long       m_CurrentNumberOfCodewords;
  
  CodebookMatrixOfIntegerType  m_CodewordHistogram;
  CodebookMatrixOfDoubleType  m_CodewordDistortion;

}; // class ImageKmeansModelEstimator


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageKmeansModelEstimator.txx"
#endif



#endif
