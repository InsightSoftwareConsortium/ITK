/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnsupervisedClassifier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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





























