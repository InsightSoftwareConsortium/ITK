/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsupervisedClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * \deprecated Class has been replaced by a  more flexible classifier framework. 
 * \ingroup UnSupervisedClassificationFilters Deprecated
 */
template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT UnsupervisedClassifier 
: public Classifier<TInputImage,TClassifiedImage>
{
public:
  /** Standard class typedefs. */
  typedef UnsupervisedClassifier   Self;
  typedef Classifier<TInputImage,TClassifiedImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnsupervisedClassifier,Classifier);

  /** Type definition for the input image. */
  typedef          TInputImage            InputImageType;
  typedef typename TInputImage::Pointer   InputImagePointer;

  /** Type definitions for the classified image. */
  typedef          TInputImage          ClassifiedImageType;
  typedef typename TInputImage::Pointer ClassifiedImagePointer;

  /** Type definition for the vector associated with
   * input image pixel type. */     
  typedef typename TInputImage::PixelType::VectorType InputImageVectorType;

  /** Generate the cluster centers of the given data set. */
  virtual void Cluster() {}

  /** Classify the input image. */
  virtual void ClassifyImage() {};

  /** Define a virtual Function that return the
   * the probabilties of a given data item belonging
   * to a certain class. */      
  virtual void GetPixelDistance(InputImageVectorType &inPixelVec,
                                double * results )=0;

protected:
  UnsupervisedClassifier();
  ~UnsupervisedClassifier();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  UnsupervisedClassifier(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  unsigned int        m_NumClasses;

}; // class UnsupervisedClassifier


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnsupervisedClassifier.txx"
#endif



#endif





























