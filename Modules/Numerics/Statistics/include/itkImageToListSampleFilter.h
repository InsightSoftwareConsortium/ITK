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
#ifndef itkImageToListSampleFilter_h
#define itkImageToListSampleFilter_h

#include "itkListSample.h"
#include "itkPixelTraits.h"
#include "itkProcessObject.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class ImageToListSampleFilter
 *  \brief The class takes an image as input and generates a list sample as
 *  output.
 *
 *  There are differences between this class and ImageToListSampleAdaptor. This
 *  class is not an adaptor. It creates a new list sample and does not
 *  provide a pseudo interface to the actual image to make it look like a
 *  list sample.
 *
 *  The class optionally allows you to specify a mask image as an input. The
 *  list sample (if a mask is specified) is constructed from pixels that are
 *  within the mask
 *
 * \todo
 * In future allow the filter to take a Spatial object as input so a
 * generic spatial object like an ellipse etc can be used as a mask.
 * Sure the ImageMaskSpatialObject
 * can represent image masks too, so why not make SpatialObjects the default. I
 * think the ImageMaskSpatialObject is slow in terms of inefficient iteration
 * through the image.
 *
 * \sa ImageToListSampleAdaptor
 * \ingroup ITKStatistics
 */
template< typename TImage, typename TMaskImage = TImage >
class ITK_TEMPLATE_EXPORT ImageToListSampleFilter:
  public ProcessObject
{
public:
  /** Standard class typedefs */
  typedef ImageToListSampleFilter    Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToListSampleFilter, ProcessObject);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image typedefs */
  typedef TImage                           ImageType;
  typedef typename ImageType::Pointer      ImagePointer;
  typedef typename ImageType::ConstPointer ImageConstPointer;
  typedef typename ImageType::PixelType    PixelType;
  typedef typename MeasurementVectorPixelTraits<
    PixelType >::MeasurementVectorType MeasurementVectorType;

  /** Mask Image typedefs */
  typedef TMaskImage                           MaskImageType;
  typedef typename MaskImageType::Pointer      MaskImagePointer;
  typedef typename MaskImageType::ConstPointer MaskImageConstPointer;
  typedef typename MaskImageType::PixelType    MaskPixelType;

  /** Type of the output list sample */
  typedef ListSample< MeasurementVectorType > ListSampleType;

  /** return the number of components of the input image */
  unsigned int GetMeasurementVectorSize() const;

  /** Method to set/get the image */
  using Superclass::SetInput;
  void SetInput(const ImageType *image);

  const ImageType * GetInput() const;

  /** Method to set/get the mask */
  void SetMaskImage(const MaskImageType *image);

  const MaskImageType * GetMaskImage() const;

  /** Method to get the list sample, the generated output. Note that this does
   * not invoke Update(). You should have called update on this class to get
   * any meaningful output. */
  const ListSampleType * GetOutput() const;

  /** Set the pixel value treated as on in the mask. If a mask has been
   * specified, only pixels with this value will be added to the list sample, if
   * no mask has been specified all pixels will be added as measurement vectors
   * to the list sample. */
  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

protected:
  ImageToListSampleFilter();
  virtual ~ImageToListSampleFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Standard itk::ProcessObject subclass method. */
  typedef DataObject::Pointer                           DataObjectPointer;
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  /** This method causes the filter to generate its output. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** This method ensures that a mask image if specified has requested regions
   * that at least contain the input image's buffered region. */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToListSampleFilter);

  MaskPixelType m_MaskValue;
};  // end of class ImageToListSampleFilter
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToListSampleFilter.hxx"
#endif

#endif
