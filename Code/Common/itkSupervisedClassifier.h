/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSupervisedClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkSupervisedClassifier_h
#define _itkSupervisedClassifier_h

#include "itkClassifier.h"
namespace itk
{

/** \class SupervisedClassifier
 * \brief Base class for SupervisedClassifier object
 *
 * itkSupervisedClassifier is the base class for all the classes that require
 * training data set for segmentation. The training data set is typically 
 * provided as a set of labelled/classified data set by the user. A model
 * is generated from the training data set. The GetPixelProbability
 * method uses this model to get the the liklihood of a given pixel/voxel/data 
 * value to a the N classes. The GaussianSupervisedClassifier returns the
 * mahalanobis distances between the a pixel/voxel/data point to the N classes
 * as a measure of liklihood.
 *
 * This object is templated over the input image and the classified images.
 * For this class the classified image type is also referred a training image.
 * This object that the user ensures that both the input and training images
 * are of the same size. The input data consists of the raw data and the
 * training data has class labels associated with each pixel. However, only
 * a subset of the data need to be labelled. Unlabelled data could be 
 * represented by a non zero, non positive number. The training data are 
 * anaysed for identifying the classes. Any non zero, non negative value is 
 * considered a valid label. It is important that the maximum value of the 
 * training label be equal to N. The pixels corresponding to each training 
 * label is parsed and the statistics calculated for each class. In case of 
 * the Gaussian Supervised Classsifier, the mean and covariance of each class
 * is calculated. These statistics are used to generate the probability of a 
 * pixel belonging to the N classes. In case of the Gaussian classifier the 
 * mean and covariance is used to estimate the distance between a unclassified 
 * data and the N classes.
 * 
 * This object supports data handling of multiband images. The object
 * accepts the input image in vector format only, where each pixel is a 
 * vector and each element of the vector corresponds to an entry from
 * 1 particular band of a multiband dataset. A single band image is treated 
 * as a vector image with a single element for every vector. The classified
 * image is treated as a single band scalar image.
 *
 */

template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT SupervisedClassifier: 
public Classifier<TInputImage,TClassifiedImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SupervisedClassifier   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef Classifier<TInputImage,TClassifiedImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SupervisedClassifier,Classifier);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;
 
  /**
   * Type definition for the vector associated with
   * input image pixel type.
   */     
  typedef typename TInputImage::PixelType::VectorType    
    InputImageVectorType;

  /**
   * Type definitions for the training image.
   */
  typedef typename TClassifiedImage::Pointer TrainingImageType;

  /**
   * Type definitions for the training image pixel type.
   */
  typedef typename TClassifiedImage::Pointer ClassifiedImageType;      
        
  /**
   * Type definitions for the vector holding
   * training image pixel type.
   */
  typedef typename TClassifiedImage::PixelType 
    TrainingImagePixelType;

  /**
   * Set the Training Image 
   */
  void SetTrainingImage ( TrainingImageType image );

  /**
   * Get the Training Image
   */
  TrainingImageType GetTrainingImage()
  {
    return m_TrainingImage;
  }

  /**
   * A virtual function that generates the 
   * model based on the training input data
   * Achieves the goal of training the classifier.
   */
  virtual void TrainClassifier() {};

  /**
   * Define a virtual Classifier function to classify the whole image.
   */
  virtual void ClassifyImage(){};

  /**
   * A virtual Function that returns the
   * the probabilties of a given data item belonging
   * to a certain class
   */
  virtual double *GetPixelDistance( InputImageVectorType &inPixelVec )=0;
  
  /**
  * Prints out the results using STL cout function.
  */
  virtual void PrintResults(){};

protected: 
  /**
   * Constructor
   */
  SupervisedClassifier();

  /**
   * Destructor
   */
  ~SupervisedClassifier();

  /**
   * Copy constructor
   */
  SupervisedClassifier(const Self&) {}

  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

  /**
   * Allocate memory for the classified image
   */
  void Allocate();

private:
  typedef typename TInputImage::SizeType InputImageSizeType;
        
  TrainingImageType   m_TrainingImage;
  unsigned int        m_NumClasses;

}; // class SupervisedClassifier


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSupervisedClassifier.txx"
#endif



#endif
