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
#ifndef itkImageDuplicator_h
#define itkImageDuplicator_h

#include "itkObject.h"
#include "itkImage.h"

namespace itk
{
/** \class ImageDuplicator
 * \brief A helper class which creates an image which is perfect copy of the input image.
 *
 * This class is NOT a filter. Although it has an API similar to a filter, this class
 * is not intended to be used in a pipeline. Instead, the typical use will be like
 * it is illustrated in the following code:
 *
 * \code
 *     medianFilter->Update();
 *     ImageType::Pointer image = medianFilter->GetOutput();
 *     typedef itk::ImageDuplicator< ImageType > DuplicatorType;
 *     DuplicatorType::Pointer duplicator = DuplicatorType::New();
 *     duplicator->SetInputImage(image);
 *     duplicator->Update();
 *     ImageType::Pointer clonedImage = duplicator->GetModifiableOutput();
 * \endcode
 *
 * Note that the Update() method must be called explicitly in the filter
 * that provides the input to the ImageDuplicator object. This is needed
 * because the ImageDuplicator is not a pipeline filter.
 *
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{SimpleOperations/ImageDuplicator,Duplicate an image}
 * \endwiki
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT ImageDuplicator:public Object
{
public:
  /** Standard class typedefs. */
  typedef ImageDuplicator            Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageDuplicator, Object);

  /** Type definitions for the input image. */
  typedef TInputImage                        ImageType;
  typedef typename TInputImage::Pointer      ImagePointer;
  typedef typename TInputImage::ConstPointer ImageConstPointer;
  typedef typename TInputImage::PixelType    PixelType;
  typedef typename TInputImage::IndexType    IndexType;

  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  itkSetConstObjectMacro(InputImage, ImageType);

  /** Get/Set the input image. */
  itkGetModifiableObjectMacro(Output, ImageType);

  /** Compute of the input image. */
  void Update();

protected:
  ImageDuplicator();
  virtual ~ImageDuplicator() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageDuplicator);

  ImageConstPointer m_InputImage;
  ImagePointer      m_Output;
  ModifiedTimeType  m_InternalImageTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageDuplicator.hxx"
#endif

#endif /* itkImageDuplicator_h */
