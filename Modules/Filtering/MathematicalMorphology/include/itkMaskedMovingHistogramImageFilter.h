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
#ifndef itkMaskedMovingHistogramImageFilter_h
#define itkMaskedMovingHistogramImageFilter_h

#include "itkMovingHistogramImageFilterBase.h"
#include "itkLexicographicCompare.h"
#include <list>
#include <map>
#include <set>

namespace itk
{
/**
 * \class MaskedMovingHistogramImageFilter
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 * \author Richard Beare
 * \author Gaetan Lehmann
 * \ingroup ITKMathematicalMorphology
 */

template <typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram>
class ITK_TEMPLATE_EXPORT MaskedMovingHistogramImageFilter
  : public MovingHistogramImageFilterBase<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskedMovingHistogramImageFilter);

  /** Standard class type aliases. */
  using Self = MaskedMovingHistogramImageFilter;
  using Superclass = MovingHistogramImageFilterBase<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskedMovingHistogramImageFilter, MovingHistogramImageFilter);

  /** Image related type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using MaskImageType = TMaskImage;
  using RegionType = typename TInputImage::RegionType;
  using SizeType = typename TInputImage::SizeType;
  using IndexType = typename TInputImage::IndexType;
  using PixelType = typename TInputImage::PixelType;
  using OffsetType = typename TInputImage::OffsetType;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using MaskPixelType = typename MaskImageType::PixelType;
  using HistogramType = THistogram;

  /** Set the marker image */
  void
  SetMaskImage(const MaskImageType * input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<TMaskImage *>(input));
  }

  /** Get the marker image */
  MaskImageType *
  GetMaskImage()
  {
    return static_cast<MaskImageType *>(const_cast<DataObject *>(this->ProcessObject::GetInput(1)));
  }

  /** Set the input image */
  void
  SetInput1(const InputImageType * input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void
  SetInput2(const MaskImageType * input)
  {
    this->SetMaskImage(input);
  }

  /** Image related type alias. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** Kernel (structuring element) iterator. */
  using KernelIteratorType = typename KernelType::ConstIterator;

  /** n-dimensional Kernel radius. */
  using RadiusType = typename KernelType::SizeType;

  using OffsetListType = typename std::list<OffsetType>;

  using OffsetMapType = typename std::map<OffsetType, OffsetListType, Functor::LexicographicCompare>;

  /** Get the modified mask image */
  MaskImageType *
  GetOutputMask();

  void
  AllocateOutputs() override;

  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

  itkSetMacro(FillValue, OutputPixelType);
  itkGetConstMacro(FillValue, OutputPixelType);

  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  itkSetMacro(BackgroundMaskValue, MaskPixelType);
  itkGetConstMacro(BackgroundMaskValue, MaskPixelType);

  void
  SetGenerateOutputMask(bool);

  itkGetConstMacro(GenerateOutputMask, bool);
  itkBooleanMacro(GenerateOutputMask);

  /** ConfigurewHistogram can be used to configure the histogram. The default version just do nothing. */
  virtual void
  ConfigureHistogram(THistogram &)
  {}

protected:
  MaskedMovingHistogramImageFilter();
  ~MaskedMovingHistogramImageFilter() override = default;

  /** Multi-thread version GenerateData. */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  pushHistogram(HistogramType &        histogram,
                const OffsetListType * addedList,
                const OffsetListType * removedList,
                const RegionType &     inputRegion,
                const RegionType &     kernRegion,
                const InputImageType * inputImage,
                const MaskImageType *  maskImage,
                const IndexType        currentIdx);

private:
  bool m_GenerateOutputMask;

  OutputPixelType m_FillValue;

  MaskPixelType m_MaskValue;

  MaskPixelType m_BackgroundMaskValue;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMaskedMovingHistogramImageFilter.hxx"
#endif

#endif
