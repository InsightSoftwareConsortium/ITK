/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkClassifier_h
#define _itkClassifier_h

#include "itkLightProcessObject.h"

namespace itk
{

/** \class Classifier
 * \brief Base class for classifier object
 * 
 * itkClassifier is the base class for the classifier objects. It provides
 * the basic function definitions that are inherent to a classifier objects.
 * It is templated over the type of input and the classified image. The  
 * second template parameter allows templating over the classified image 
 * type. The template name is a misnomer since this class allows general 
 * classification of features stored as vector data. The name "image" 
 * indicates that the basic data structure used for storing data/results 
 * are derived from the ITK image class. The classified image (output) of 
 * this object is treated as a single band image and is accessed through
 * Get/SetScalar functions.
 *
 * The basic functionality is given an image classify each pixel/voxel in
 * one of the N classes where N is specified by the user. The classifier 
 * class is divided in to two subclasses itkSupervisedClassifier and 
 * itkUnsupervisedClassifier provide for most of the functionalities a 
 * classification algorithm might fall. However, for some new classifier 
 * this class provides the interface via the public virtual functions 
 * (ApplyClassifier and GetPixelProbability).

 * This object supports data handling of multiband images. The object
 * accepts the input image in vector format only, where each pixel is a 
 * vector and each element of the vector corresponds to an entry from
 * 1 particular band of a multiband dataset. A single band image is treated 
 * as a vector image with a single element for every vector. The classified
 * image is treated as a single band scalar image.
 *
 * \ingroup ClassificationFilters 
 */
template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT Classifier : public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef Classifier   Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(Classifier,LightProcessObject);

  /** Type definition for the input image. */
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::Pointer         InputImagePointer;
  typedef typename TInputImage::ConstPointer    InputImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType InputPixelType;

  /** Type definition for the vector associated with
   * input image pixel type. */   
  typedef typename TInputImage::PixelType InputImageVectorType;

  /** Type definitions for the training image pixel type. */
  typedef typename TClassifiedImage::Pointer ClassifiedImageType;   
  
  /** Type definitions for the training image pixel type. */
  typedef typename TClassifiedImage::Pointer TrainingImageType;      
        
  /** Type definitions for the vector holding
   * training image pixel type. */
  typedef typename TClassifiedImage::PixelType 
    TrainingImagePixelType;

  /** Set the input image. */
  itkSetConstObjectMacro(InputImage,InputImageType);

  /** Get the input image. */
  itkGetConstObjectMacro(InputImage,InputImageType);

  /** Set the classified image. */
  itkSetMacro(ClassifiedImage,ClassifiedImageType);

  /** Get the classified image. */
  itkGetMacro(ClassifiedImage,ClassifiedImageType);

  /** Set the number of classes. */
  itkSetMacro(NumberOfClasses, unsigned int);

  /** Get the number of classes. */
  itkGetMacro(NumberOfClasses, unsigned int);

  /** Define a virtual function to train a classifier. This is to be
   * used when training data is available (i.e., with supervised
   * classifiers). */
  virtual void TrainClassifier(){};

  /** Define a virtual function to perform clustering of input data
   * using an unsupervised classifier (this does not require training
   * data). */
  virtual void Cluster(){};

  /** Define a virtual function to classify the whole image. This is the
   * function where the specific alorithm implementation are implemented.  */
  virtual void ClassifyImage(){};

  /** Define a virtual Function that return the distance of a given 
   * vector data belonging to the different classes. It returns an array 
   * of numbers representing the distances between a input data and 
   * the various possible classes. */
  virtual void GetPixelDistance(InputImageVectorType &inPixelVec,
    double * results )=0;

  /** Set a training image (for supervised classifier). */
  virtual void SetTrainingImage( TrainingImageType image ) {};

protected:
  Classifier();
  ~Classifier();
  void PrintSelf(std::ostream& os, Indent indent) const;

  unsigned int        m_NumberOfClasses;

  void GenerateData();

private:
  Classifier(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputImageConstPointer      m_InputImage;
  ClassifiedImageType         m_ClassifiedImage;

}; // class Classifier


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkClassifier.txx"
#endif



#endif
















