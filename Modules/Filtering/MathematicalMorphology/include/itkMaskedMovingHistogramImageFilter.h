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
#ifndef itkMaskedMovingHistogramImageFilter_h
#define itkMaskedMovingHistogramImageFilter_h

#include "itkMovingHistogramImageFilterBase.h"
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

template< typename TInputImage, typename TMaskImage, typename TOutputImage, typename TKernel, typename THistogram >
class ITK_TEMPLATE_EXPORT MaskedMovingHistogramImageFilter:
  public MovingHistogramImageFilterBase< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef MaskedMovingHistogramImageFilter                                     Self;
  typedef MovingHistogramImageFilterBase< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                                 Pointer;
  typedef SmartPointer< const Self >                                           ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskedMovingHistogramImageFilter, MovingHistogramImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                                InputImageType;
  typedef TOutputImage                               OutputImageType;
  typedef TMaskImage                                 MaskImageType;
  typedef typename TInputImage::RegionType           RegionType;
  typedef typename TInputImage::SizeType             SizeType;
  typedef typename TInputImage::IndexType            IndexType;
  typedef typename TInputImage::PixelType            PixelType;
  typedef typename TInputImage::OffsetType           OffsetType;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename TOutputImage::PixelType           OutputPixelType;
  typedef typename TInputImage::PixelType            InputPixelType;
  typedef typename MaskImageType::PixelType          MaskPixelType;
  typedef THistogram                                 HistogramType;

  /** Set the marker image */
  void SetMaskImage(const MaskImageType *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< TMaskImage * >( input ) );
  }

  /** Get the marker image */
  MaskImageType * GetMaskImage()
  {
    return static_cast< MaskImageType * >( const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
  }

  /** Set the input image */
  void SetInput1(const InputImageType *input)
  {
    this->SetInput(input);
  }

  /** Set the marker image */
  void SetInput2(const MaskImageType *input)
  {
    this->SetMaskImage(input);
  }

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** Kernel (structuring element) iterator. */
  typedef typename KernelType::ConstIterator KernelIteratorType;

  /** n-dimensional Kernel radius. */
  typedef typename KernelType::SizeType RadiusType;

  typedef typename std::list< OffsetType > OffsetListType;

  typedef typename std::map< OffsetType, OffsetListType,
                             typename Functor::OffsetLexicographicCompare< itkGetStaticConstMacro(ImageDimension) > >
  OffsetMapType;

  /** Get the modified mask image */
  MaskImageType * GetOutputMask();

  void AllocateOutputs() ITK_OVERRIDE;

  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObject::Pointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  itkSetMacro(FillValue, OutputPixelType);
  itkGetConstMacro(FillValue, OutputPixelType);

  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  itkSetMacro(BackgroundMaskValue, MaskPixelType);
  itkGetConstMacro(BackgroundMaskValue, MaskPixelType);

  void SetGenerateOutputMask(bool);

  itkGetConstMacro(GenerateOutputMask, bool);
  itkBooleanMacro(GenerateOutputMask);

  /** ConfigurewHistogram can be used to configure the histogram. The default version just do nothing. */
  virtual void ConfigureHistogram(THistogram &) {}

protected:
  MaskedMovingHistogramImageFilter();
  ~MaskedMovingHistogramImageFilter() ITK_OVERRIDE {}

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void pushHistogram(HistogramType & histogram,
                     const OffsetListType *addedList,
                     const OffsetListType *removedList,
                     const RegionType & inputRegion,
                     const RegionType & kernRegion,
                     const InputImageType *inputImage,
                     const MaskImageType *maskImage,
                     const IndexType currentIdx);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskedMovingHistogramImageFilter);

  bool m_GenerateOutputMask;

  OutputPixelType m_FillValue;

  MaskPixelType m_MaskValue;

  MaskPixelType m_BackgroundMaskValue;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaskedMovingHistogramImageFilter.hxx"
#endif

#endif
