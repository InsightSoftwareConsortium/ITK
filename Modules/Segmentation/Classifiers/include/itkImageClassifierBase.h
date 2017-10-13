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
#ifndef itkImageClassifierBase_h
#define itkImageClassifierBase_h

#include "itkClassifierBase.h"
#include "itkMacro.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/** \class ImageClassifierBase
 * \brief Base class for the ImageClassifierBase object.
 *
 * itkImageClassifierBase is the base class for algorithms
 * that take input data as images and preserve the image structure
 * while performing classification. In other words, the data is not
 * converted into a list, hence filters that require spatial information
 * of a pixel can use the subclasses under this tree. It provides
 * the basic function definitions that are inherent to an image classifier
 * objects.
 *
 * This is the Superclass for the image classifier tree of the classifier
 * framework. This is the class for all the classification objects available
 * through the classifier framework in the ITK toolkit that hold the input
 * image and the classified image data.
 *
 * It is templated over the type of input image, classified image. The second
 * template parameter allows templating over the classified image type. The
 * name "image" indicates that the basic data structure used for storing
 * data/results are derived from the ITK image class.
 *
 * This object supports data handling of multiband images. The object
 * accepts the input image in vector format only, where each pixel is a
 * vector and each element of the vector corresponds to an entry from
 * 1 particular band of a multiband dataset. A single band image is treated
 * as a vector image with a single element for every vector. The classified
 * image is treated as a single band scalar image.
 *
 * This class stores the input and output data as its private members.
 * Before you call the Classify method to start the classification process,
 * you should plug in all necessary parts as described in the superclass
 * documentation.
 *
 * The core computation is carried out here. The function requires that
 * the number of classes be set to a non zero value and the membership
 * functions be populated. In addition, the number of classes should be equal
 * to the number of membership functions.
 *
 * \ingroup ImageClassificationFilters
 * \ingroup ITKClassifiers
 */

template< typename TInputImage,
          typename TClassifiedImage >
class ITK_TEMPLATE_EXPORT ImageClassifierBase:
  public ClassifierBase< TInputImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageClassifierBase           Self;
  typedef ClassifierBase< TInputImage > Superclass;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageClassifierBase, ClassifierBase);

  /** Type definition for the input image. */
  typedef TInputImage                        InputImageType;
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** Type definitions for the classified image pixel type. */
  typedef typename TClassifiedImage::Pointer ClassifiedImagePointer;

  /** Type definitions from the Superclass */

  /**Set the decision rule */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;

  /** Typedefs for membership funciton */
  typedef typename Superclass::MembershipFunctionType MembershipFunctionType;

  typedef typename Superclass::MembershipFunctionPointer
  MembershipFunctionPointer;

  typedef typename Superclass::MembershipFunctionPointerVector
  MembershipFunctionPointerVector;

  /** Type alias for decision rule */
  typedef typename Superclass::DecisionRuleType DecisionRuleType;

  /** Get/Set the input image. */
  itkSetConstObjectMacro(InputImage, InputImageType);
  itkGetConstObjectMacro(InputImage, InputImageType);

  /** Set the classified image. */
  itkSetMacro(ClassifiedImage, ClassifiedImagePointer);

  /** Get the classified image. */
  itkGetConstMacro(ClassifiedImage, ClassifiedImagePointer);

  /** Type definition for the vector associated with
    * input image pixel type. */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Type definitions for the vector holding
   * training image pixel type. */
  typedef typename TClassifiedImage::PixelType ClassifiedImagePixelType;

  /** Type definition for the input image/training iterator */
  typedef
  ImageRegionConstIterator< TInputImage >    InputImageConstIterator;
  typedef
  ImageRegionIterator< TClassifiedImage >    ClassifiedImageIterator;

  /** Method to get the membership of a given pixel to the different classes */
  std::vector< double >
  GetPixelMembershipValue(const InputImagePixelType inputImagePixel);

protected:
  ImageClassifierBase();
  ~ImageClassifierBase() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Allocate memory for the classified image. */
  void Allocate();

  /** Starts the classification process */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageClassifierBase);

  typedef typename TInputImage::SizeType InputImageSizeType;

  InputImageConstPointer m_InputImage;
  ClassifiedImagePointer m_ClassifiedImage;

  /** Define a virtual Classifier function to classify the whole image. */
  virtual void Classify();
}; // class ImageClassifierBase
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageClassifierBase.hxx"
#endif

#endif
