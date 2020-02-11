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
#ifndef itkBinaryImageToLabelMapFilter_h
#define itkBinaryImageToLabelMapFilter_h

#include "itkScanlineFilterCommon.h"
#include "itkLabelMap.h"
#include "itkLabelObject.h"

namespace itk
{
/**
 * \class BinaryImageToLabelMapFilter
 * \brief Label the connected components in a binary image and produce a
 * collection of label objects.
 *
 * BinaryImageToLabelMapFilter labels the objects in a binary image.
 * Each distinct object is assigned a unique label.
 * The final object labels start with 1 and are consecutive. Objects
 * that are reached earlier by a raster order scan have a lower
 * label.
 *
 * The GetOutput() function of this class returns an itk::LabelMap.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ConnectedComponentImageFilter, LabelImageToLabelMapFilter, LabelMap, LabelObject
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/LabelBinaryRegionsInImage,Label Binary Regions In Image}
 * \endsphinx
 */

template <typename TInputImage,
          typename TOutputImage = LabelMap<LabelObject<SizeValueType, TInputImage::ImageDimension>>>
class ITK_TEMPLATE_EXPORT BinaryImageToLabelMapFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
  , protected ScanlineFilterCommon<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLabelMapFilter);

  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = BinaryImageToLabelMapFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass::Register;
  using Superclass::UnRegister;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(BinaryImageToLabelMapFilter, ImageToImageFilter);

  /**
   * Types from the Superclass
   */
  using InputImagePointer = typename Superclass::InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using SizeValueType = typename TInputImage::SizeValueType;
  using OffsetValueType = typename TInputImage::OffsetValueType;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using OffsetType = typename TInputImage::OffsetType;

  using OutputImageType = TOutputImage;
  using RegionType = typename TOutputImage::RegionType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputOffsetType = typename TOutputImage::OffsetType;
  using OutputImagePixelType = typename TOutputImage::PixelType;

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  // only set after completion
  itkGetConstReferenceMacro(NumberOfObjects, SizeValueType);

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<OutputPixelType>::NonpositiveMin().
   */
  itkSetMacro(OutputBackgroundValue, OutputPixelType);
  itkGetConstMacro(OutputBackgroundValue, OutputPixelType);

  /**
   * Set/Get the value to be consider "foreground" in the input image.
   * Defaults to NumericTraits<InputPixelType>::max().
   */
  itkSetMacro(InputForegroundValue, InputPixelType);
  itkGetConstMacro(InputForegroundValue, InputPixelType);

protected:
  BinaryImageToLabelMapFilter();
  ~BinaryImageToLabelMapFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  DynamicThreadedGenerateData(const RegionType & outputRegionForThread) override;

  void
  GenerateData() override;

  /** BinaryImageToLabelMapFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** BinaryImageToLabelMapFilter will produce all of the output.
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
  OutputPixelType m_OutputBackgroundValue;
  InputPixelType  m_InputForegroundValue;
  SizeValueType   m_NumberOfObjects;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  if !defined(ITK_WRAPPING_PARSER)
#    include "itkBinaryImageToLabelMapFilter.hxx"
#  endif
#endif

#endif
