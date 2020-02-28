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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkLabelMapFilter_h
#define itkLabelMapFilter_h

#include "itkImageToImageFilter.h"
#include <mutex>

namespace itk
{
/**
 *\class LabelMapFilter
 * \brief Base class for filters that take an image as input and overwrite that image as the output
 *
 * LabelMapFilter is the base class for all process objects whose
 * are using a LabelMapFilter as input. It manage several threads,
 * and run a method ThreadedGenerateData() for each object in the LabelMapFilter.
 * With that class, the developer doesn't need to take care of iterating over all the objects in
 * the image, or to manage by hand the threads.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \sa LabelMapToBinaryImageFilter, LabelMapToLabelImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup LabeledImageFilters
 * \ingroup ITKLabelMap
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT LabelMapFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelMapFilter);

  /** Standard class type aliases. */
  using Self = LabelMapFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LabelMapFilter, ImageToImageFilter);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Some convenient type alias. */
  using InputImageType = typename Superclass::InputImageType;
  using InputImagePointer = typename Superclass::InputImagePointer;
  using InputImageConstPointer = typename Superclass::InputImageConstPointer;
  using InputImageRegionType = typename Superclass::InputImageRegionType;
  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using LabelObjectType = typename InputImageType::LabelObjectType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** LabelMapFilter requires the entire input to be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** LabelMapFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

protected:
  LabelMapFilter();
  ~LabelMapFilter() override = default;

  void
  BeforeThreadedGenerateData() override;

  void
  AfterThreadedGenerateData() override;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  // derived classes call this as inherited so we must delegate to DynamicThreadedGenerateData
  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType) override
  {
    Self::DynamicThreadedGenerateData(outputRegionForThread);
  }

  virtual void
  ThreadedProcessLabelObject(LabelObjectType * labelObject);

  /**
   * Return the label collection image to use. This method may be overloaded
   * if the label collection image to use is not the input image.
   */
  virtual InputImageType *
  GetLabelMap()
  {
    return static_cast<InputImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(0)));
  }

  std::mutex m_LabelObjectContainerLock;

private:
  typename InputImageType::Iterator m_LabelObjectIterator;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLabelMapFilter.hxx"
#endif

#endif
