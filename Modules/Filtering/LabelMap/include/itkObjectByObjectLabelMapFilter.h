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
#ifndef itkObjectByObjectLabelMapFilter_h
#define itkObjectByObjectLabelMapFilter_h

#include "itkLabelMapFilter.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"
#include "itkLabelSelectionLabelMapFilter.h"
#include "itkAutoCropLabelMapFilter.h"
#include "itkPadLabelMapFilter.h"


namespace itk
{

/**
 *\class ObjectByObjectLabelMapFilter
 * \brief ObjectByObjectLabelMapFilter applies an image pipeline to all the objects of a label map and produce a new
 * label map
 *
 * The image pipeline can simply produce a modified object or produce several objects
 * from the single input object. Several options are provided to handle the different
 * cases.
 *
 * KeepLabel, which defaults to true, makes the filter try to keep as much as possible
 * the labels of the original objects. If an image pipeline produce several objects
 * the label of the input object is assigned to the first output object. The other
 * output objects get another label not present in the input image.
 * When KeepLabel is set to false, all the objects are relabeled in the order of
 * apparition during the filter process.
 *
 * BinaryInternalOutput can be set to true if the image pipeline produce binary
 * output image. In that case, the objects produced are identified with a connected
 * component algorithm before being reinserted in the output label map.
 * InternalForegroundValue can be set to a specific value which represent the foreground
 * value in the binary image.
 *
 * PadSize and ConstrainPaddingToImage can be used to extend the size of the image to
 * process passed to the image pipeline. This is useful if the image pipeline is known
 * to be able to enlarge the object. The padding can be constrained to the input label map
 * region by setting ConstrainPaddingToImage to true - this parameter can make a difference
 * for the algorithm with a different behavior on the border of the image.
 * By default, the image is padded by 1 pixel and constrained to the image region.
 *
 * \note: When applying a single filter, input and output filters are the same; while applying a pipeline,
 * input and output filters are different, may not even be of the same type. It is the responsibility of the
 * user to connect the pipeline properly outside of this filter.
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/176
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKLabelMap
 *
 * \sphinx
 * \sphinxexample{Filtering/LabelMap/ApplyMorphologicalClosingOnAllLabelObjects,Apply Morphological Closing On All Label
 * Objects} \endsphinx
 */
template <typename TInputImage,
          typename TOutputImage = TInputImage,
          typename TInputFilter = ImageToImageFilter<Image<unsigned char, TInputImage::ImageDimension>,
                                                     Image<unsigned char, TOutputImage::ImageDimension>>,
          class TOutputFilter = typename TInputFilter::Superclass,
          class TInternalInputImage = typename TInputFilter::InputImageType,
          class TInternalOutputImage = typename TOutputFilter::OutputImageType>
class ITK_TEMPLATE_EXPORT ObjectByObjectLabelMapFilter : public LabelMapFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ObjectByObjectLabelMapFilter);

  /** Standard class type aliases. */
  using Self = ObjectByObjectLabelMapFilter;
  using Superclass = LabelMapFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using SizeType = typename OutputImageType::SizeType;
  using LabelMapType = OutputImageType;
  using LabelObjectType = typename LabelMapType::LabelObjectType;

  using InputFilterType = TInputFilter;
  using OutputFilterType = TOutputFilter;

  using InternalInputImageType = TInternalInputImage;
  using InternalRegionType = typename InternalInputImageType::RegionType;
  using InternalSizeType = typename InternalInputImageType::SizeType;
  using InternalIndexType = typename InternalInputImageType::IndexType;
  using InternalOffsetType = typename InternalInputImageType::OffsetType;
  using InternalInputPixelType = typename InternalInputImageType::PixelType;

  using InternalOutputImageType = TInternalOutputImage;
  using InternalOutputPixelType = typename InternalOutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ObjectByObjectLabelMapFilter, LabelMapFilter);

  void
  SetFilter(InputFilterType * filter);
  InputFilterType *
  GetFilter()
  {
    return this->m_InputFilter;
  }

  const InputFilterType *
  GetFilter() const
  {
    return this->m_InputFilter;
  }

  void
  SetInputFilter(InputFilterType * filter);
  itkGetModifiableObjectMacro(InputFilter, InputFilterType);

  void
  SetOutputFilter(OutputFilterType * filter);
  itkGetModifiableObjectMacro(OutputFilter, OutputFilterType);

  /** If KeepLabels is set to true, the filter will do its best to reuse the labels
   * of the input objects in the output ones. However, this is possible only if the
   * internal pipeline produce a single object - the other labels will be taken as
   * they come.
   * If KeepLabels is false, no care is made of the input labels, and a new label is produced
   * for all the objects using LabelMap::PushLabelObject().
   */
  itkSetMacro(KeepLabels, bool);
  itkGetMacro(KeepLabels, bool);
  itkBooleanMacro(KeepLabels);

  /** If PadSize is not zero, the image produce for each object will be padded.
   * The default value is 1 on all the dimensions.
   */
  itkSetMacro(PadSize, SizeType);
  itkGetMacro(PadSize, SizeType);

  /** Padding by PadSize will be constrained to the input image region if
   * ConstrainPaddingToImage is true, and won't be constrained if it is set to false.
   * Default value is true.
   */
  itkSetMacro(ConstrainPaddingToImage, bool);
  itkGetMacro(ConstrainPaddingToImage, bool);
  itkBooleanMacro(ConstrainPaddingToImage);

  /** Set/Get whether the internal image produced by OutputFilter should be interpreted
   * as a binary image in which the filter have to search for connected components. If
   * set to false, the filter consider the image as a label image.
   * Default is false.
   */
  itkSetMacro(BinaryInternalOutput, bool);
  itkGetMacro(BinaryInternalOutput, bool);
  itkBooleanMacro(BinaryInternalOutput);

  /** The foreground value used internally to represent the object in the image passed to
   * InputFilter, and to read the data produced by OutputFilter, if BinaryInternalOutput
   * is true
   */
  itkSetMacro(InternalForegroundValue, InternalOutputPixelType);
  itkGetMacro(InternalForegroundValue, InternalOutputPixelType);

  /** The label of the object currently processed by the filter. This is intended to be
   * used with the IterationEvent sent before the processing of each object. It contains
   * a relevant value only during the filter update.
   */
  itkGetMacro(Label, InputImagePixelType);

protected:
  ObjectByObjectLabelMapFilter();
  ~ObjectByObjectLabelMapFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  bool     m_ConstrainPaddingToImage;
  SizeType m_PadSize;
  bool     m_BinaryInternalOutput;

  bool m_KeepLabels;

  InternalOutputPixelType m_InternalForegroundValue;

  using SelectType = itk::LabelSelectionLabelMapFilter<LabelMapType>;
  typename SelectType::Pointer m_Select;

  using CropType = itk::AutoCropLabelMapFilter<LabelMapType>;
  typename CropType::Pointer m_Crop;

  using PadType = itk::PadLabelMapFilter<LabelMapType>;
  typename PadType::Pointer m_Pad;

  using LM2BIType = itk::LabelMapToBinaryImageFilter<LabelMapType, InternalInputImageType>;
  typename LM2BIType::Pointer m_LM2BI;

  using LI2LMType = itk::LabelImageToLabelMapFilter<InternalOutputImageType, LabelMapType>;
  typename LI2LMType::Pointer m_LI2LM;

  using BI2LMType = itk::BinaryImageToLabelMapFilter<InternalOutputImageType, LabelMapType>;
  typename BI2LMType::Pointer m_BI2LM;

  typename InputFilterType::Pointer  m_InputFilter;
  typename OutputFilterType::Pointer m_OutputFilter;


  InputImagePixelType m_Label;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkObjectByObjectLabelMapFilter.hxx"
#endif

#endif
