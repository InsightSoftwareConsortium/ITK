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
#ifndef itkLabelContourImageFilter_h
#define itkLabelContourImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImage.h"
#include "itkConceptChecking.h"
#include <vector>

namespace itk
{
/**
 * \class LabelContourImageFilter
 * \brief Labels the pixels on the border of the objects in a labeled image.
 *
 * LabelContourImageFilter takes a labeled image as input, where the pixels in the
 * objects are the pixels with a value different of the BackgroundValue. Only the pixels
 * on the contours of the objects are kept. The pixels not on the border are changed
 * to BackgroundValue. The labels of the object are the same in the input and in the
 * output image.
 *
 * The connectivity can be changed to minimum or maximum connectivity with
 * SetFullyConnected(). Full connectivity produces thicker contours.
 *
 * https://hdl.handle.net/1926/1352
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa BinaryContourImageFilter
 *
 * \ingroup ITKImageLabel
 *
 * \wiki
 * \wikiexample{ImageSegmentation/LabelContourImageFilter,Label the contours of connected components}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT LabelContourImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LabelContourImageFilter);

  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = LabelContourImageFilter;
  using Superclass = InPlaceImageFilter< TInputImage, TOutputImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(LabelContourImageFilter, InPlaceImageFilter);

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING

  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;

  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  itkConceptMacro( SameDimension,
    ( Concept::SameDimension< Self::InputImageDimension,
                              Self::OutputImageDimension > ) );

#endif

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputIndexType = typename InputImageType::IndexType;
  using InputSizeType = typename InputImageType::SizeType;
  using InputOffsetType = typename InputImageType::OffsetType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OffsetValueType = typename InputImageType::OffsetValueType;
  using InputPixelType = typename InputImageType::PixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputRegionType = typename OutputImageType::RegionType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using OutputOffsetType = typename OutputImageType::OffsetType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.
   * \note For objects that are 1 pixel wide, use FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /**
   * Set/Get the background value used to identify the objects and mark the
   * pixels not on the border of the objects.
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

protected:

  LabelContourImageFilter();
  ~LabelContourImageFilter() override {}

  void PrintSelf(std::ostream & os, Indent indent) const override;

  SizeValueType IndexToLinearIndex(OutputIndexType index);

  void GenerateData() override;

  void BeforeThreadedGenerateData() override;

  void AfterThreadedGenerateData() override;

  void DynamicThreadedGenerateData(const OutputRegionType& outputRegionForThread) override;

  void ThreadedIntegrateData(const OutputRegionType& outputRegionForThread);


  /** LabelContourImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() override;

  /** LabelContourImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) ) override;

private:
  /** types to support the run length encoding of lines */
  struct RunLength
  {
    /** run length information - may be a more type safe way of doing this */
    SizeValueType length;

    /** Index of the start of the run */
    InputIndexType where;

    InputImagePixelType label;
  };

  using LineEncodingType = std::vector< RunLength >;
  using LineEncodingIterator = typename LineEncodingType::iterator;
  using LineEncodingConstIterator = typename LineEncodingType::const_iterator;

  using OffsetVectorType = std::vector< OffsetValueType >;
  using OffsetVectorConstIterator = typename OffsetVectorType::const_iterator;

  // the map storing lines
  using LineMapType = std::vector< LineEncodingType >;

  LineMapType           m_LineMap;
  OutputImagePixelType  m_BackgroundValue;
  ThreadIdType          m_NumberOfWorkUnits;
  bool                  m_FullyConnected;

  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B) const;

  void CompareLines(TOutputImage *output, LineEncodingType & current, const LineEncodingType & Neighbour);

  void SetupLineOffsets(OffsetVectorType & LineOffsets);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelContourImageFilter.hxx"
#endif

#endif
