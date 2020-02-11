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
#ifndef itkConnectedComponentImageFilter_h
#define itkConnectedComponentImageFilter_h

#include "itkScanlineFilterCommon.h"

namespace itk
{
/**
 * \class ConnectedComponentImageFilter
 * \brief Label the objects in a binary image
 *
 * ConnectedComponentImageFilter labels the objects in a binary image
 * (non-zero pixels are considered to be objects, zero-valued pixels
 * are considered to be background).
 * Each distinct object is assigned a unique label. The filter experiments
 * with some improvements to the existing implementation, and is based on
 * run length encoding along raster lines.
 * If the output background value is set to zero (the default), the final
 * object labels start with 1 and are consecutive. If the output background
 * is set to a non-zero value (by calling the SetBackgroundValue() routine of the filter),
 * the final labels start at 0, and remain consecutive except for skipping the background
 * value as needed. Objects that are reached earlier by a raster order scan have a lower
 * label. This is different to the behaviour of the original connected
 * component image filter which did not produce consecutive labels or
 * impose any particular ordering.
 *
 * After the filter is executed, ObjectCount holds the number of connected components.
 *
 * \sa ImageToImageFilter
 *
 * \ingroup SingleThreaded
 * \ingroup ITKConnectedComponents
 *
 * \sphinx
 * \sphinxexample{Segmentation/ConnectedComponents/LabelConnectComponentsInBinaryImage,Label Connect Components In
 * Binary Image} \sphinxexample{Segmentation/ConnectedComponents/ExtraLargestConnectComponentFromBinaryImage,Extra
 * Largest Connect Component From Binary Image} \endsphinx
 */

template <typename TInputImage, typename TOutputImage, typename TMaskImage = TInputImage>
class ITK_TEMPLATE_EXPORT ConnectedComponentImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
  , protected ScanlineFilterCommon<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConnectedComponentImageFilter);

  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = ConnectedComponentImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Superclass::Register;
  using Superclass::UnRegister;

  /**
   * Types from the Superclass
   */
  using InputImagePointer = typename Superclass::InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using MaskPixelType = typename TMaskImage::PixelType;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using MaskImageType = TMaskImage;
  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using OffsetType = typename TInputImage::OffsetType;

  using OutputImageType = TOutputImage;
  using RegionType = typename TOutputImage::RegionType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputOffsetType = typename TOutputImage::OffsetType;
  using OutputImagePixelType = typename TOutputImage::PixelType;

  using ListType = std::list<IndexType>;
  using MaskImagePointer = typename MaskImageType::Pointer;

  /**
   * Smart pointer type alias support
   */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(ConnectedComponentImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /** Type used as identifier of the different component labels. */
  using LabelType = IdentifierType;

  // only set after completion
  itkGetConstReferenceMacro(ObjectCount, LabelType);

  itkConceptMacro(OutputImagePixelTypeIsInteger, (Concept::IsInteger<OutputImagePixelType>));

  itkSetInputMacro(MaskImage, MaskImageType);
  itkGetInputMacro(MaskImage, MaskImageType);

  /**
   * Set the pixel intensity to be used for background (non-object)
   * regions of the image in the output. Note that this does NOT set
   * the background value to be used in the input image.
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

protected:
  ConnectedComponentImageFilter();

  ~ConnectedComponentImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  void
  DynamicThreadedGenerateData(const RegionType &) override;

  void
  ThreadedWriteOutput(const RegionType &);

  /** ConnectedComponentImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** ConnectedComponentImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  using ScanlineFunctions = ScanlineFilterCommon<TInputImage, TOutputImage>;

  using InternalLabelType = typename ScanlineFunctions::InternalLabelType;
  using OutSizeType = typename ScanlineFunctions::OutSizeType;
  using RunLength = typename ScanlineFunctions::RunLength;
  using LineEncodingType = typename ScanlineFunctions::LineEncodingType;
  using LineEncodingIterator = typename ScanlineFunctions::LineEncodingIterator;
  using LineEncodingConstIterator = typename ScanlineFunctions::LineEncodingConstIterator;
  using OffsetVectorType = typename ScanlineFunctions::OffsetVectorType;
  using OffsetVectorConstIterator = typename ScanlineFunctions::OffsetVectorConstIterator;
  using LineMapType = typename ScanlineFunctions::LineMapType;
  using UnionFindType = typename ScanlineFunctions::UnionFindType;
  using ConsecutiveVectorType = typename ScanlineFunctions::ConsecutiveVectorType;
  using WorkUnitData = typename ScanlineFunctions::WorkUnitData;

private:
  OutputPixelType m_BackgroundValue = NumericTraits<OutputPixelType>::ZeroValue();
  LabelType       m_ObjectCount = 0;

  typename TInputImage::ConstPointer m_Input;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  if !defined(ITK_WRAPPING_PARSER)
#    include "itkConnectedComponentImageFilter.hxx"
#  endif
#endif

#endif
