/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsupervisedClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkUnsupervisedClassifier_h
#define _itkUnsupervisedClassifier_h

#include "itkObject.h"
#include "itkClassifier.h"
namespace itk
{

/** \class UnsupervisedClassifier
 * \brief Base class for UnupervisedClassifier object
 *
 * itkUnsupervisedClassifier is the base class for all the classes that do
 * not require training data set. This object clusters the input data
 * based on some criteria. For instance, in case of the Kmeans algorithm, it
 * the criteria used is based on Euclidean distance. The input data could be
 * a image/volume or any set of feature vector. It can be used in any 
 * place where clustering is needed but no prior information is availavble.
 * This can be used to generate a codebook for vector quatinzation 
 * application. All the user needs to provide is the number of classes. 
 * The interface to classifying images is provided through the virtual
 * functions with this object.
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
class ITK_EXPORT UnsupervisedClassifier 
: public Classifier<TInputImage,TClassifiedImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef UnsupervisedClassifier   Self;

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
  itkTypeMacro(UnsupervisedClassifier,Classifier);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definitions for the classified image.
   */
  typedef typename TInputImage::Pointer ClassifiedImageType;

  /**
   * Type definition for the vector associated with
   * input image pixel type.
   */     
  typedef typename TInputImage::PixelType::VectorType   
    InputImageVectorType;

  /**
   * Generate the cluster centers of the given data set
   * .
   */
  virtual void Cluster(){};

  /**
   * Classify input image
   */
  virtual void ClassifyImage() {};

  /**
   * Define a virtual Function that return the
   * the probabilties of a given data item belonging
   * to a certain class
   */      
  virtual double *GetPixelDistance(InputImageVectorType &inPixelVec)=0;

protected:
  /**
   * Constructor
   */
  UnsupervisedClassifier();

  /**
   * Destructor
   */
  ~UnsupervisedClassifier();

  /**
   * Copy constructor
   */
  UnsupervisedClassifier(const Self&) {}

  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

private:
  unsigned int        m_NumClasses;

}; // class UnsupervisedClassifier


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnsupervisedClassifier.txx"
#endif



#endif





























