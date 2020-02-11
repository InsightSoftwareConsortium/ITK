/*=========================================================================
 *
 *  Copyright NumFOCUS
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
   \code
       medianFilter->Update();
       ImageType::Pointer image = medianFilter->GetOutput();
       using DuplicatorType = itk::ImageDuplicator< ImageType >;
       DuplicatorType::Pointer duplicator = DuplicatorType::New();
       duplicator->SetInputImage(image);
       duplicator->Update();
       ImageType::Pointer clonedImage = duplicator->GetOutput();
   \endcode
 *
 * Note that the Update() method must be called explicitly in the filter
 * that provides the input to the ImageDuplicator object. This is needed
 * because the ImageDuplicator is not a pipeline filter.
 *
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/DuplicateAnImage,Duplicate An Image}
 * \endsphinx
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT ImageDuplicator : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageDuplicator);

  /** Standard class type aliases. */
  using Self = ImageDuplicator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageDuplicator, Object);

  /** Type definitions for the input image. */
  using ImageType = TInputImage;
  using ImagePointer = typename TInputImage::Pointer;
  using ImageConstPointer = typename TInputImage::ConstPointer;
  using PixelType = typename TInputImage::PixelType;
  using IndexType = typename TInputImage::IndexType;

  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  itkSetConstObjectMacro(InputImage, ImageType);

  /**
   * Provide an interface to match that
   * of other ProcessObjects
   * for this source generation object
   * by returning a non-const pointer
   * for the generated Object.
   */
  // NOTE:  The m_DuplicateImage is only
  //       exposed via the Source generation interface
  //       by the GetOutput() method that mimics
  //       a process object.
  virtual const ImageType *
  GetOutput() const
  {
    return this->m_DuplicateImage.GetPointer();
  }
  virtual ImageType *
  GetOutput()
  {
    return this->m_DuplicateImage.GetPointer();
  }

#if !defined(ITK_LEGACY_REMOVE)
  // This interface was exposed in ITKv4 when the itkGetModifiableObjectMacro was used
  virtual ImageType *
  GetModifiableOutput()
  {
    return this->m_DuplicateImage.GetPointer();
  }
#endif

  /** Compute of the input image. */
  void
  Update();

protected:
  ImageDuplicator();
  ~ImageDuplicator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ImageConstPointer m_InputImage;
  ImagePointer      m_DuplicateImage;
  ModifiedTimeType  m_InternalImageTime;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageDuplicator.hxx"
#endif

#endif /* itkImageDuplicator_h */
