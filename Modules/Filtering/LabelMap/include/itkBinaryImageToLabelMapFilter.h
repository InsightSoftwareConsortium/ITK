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
#ifndef itkBinaryImageToLabelMapFilter_h
#define itkBinaryImageToLabelMapFilter_h

#include "itkImageToImageFilter.h"
#include <deque>
#include <map>
#include <mutex>
#include <vector>
#include "itkLabelMap.h"
#include "itkLabelObject.h"
#include "itkMultiThreaderBase.h"

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
 * \wiki
 * \wikiexample{ImageProcessing/BinaryImageToLabelMapFilter,Label binary regions in an image}
 * \endwiki
 */

template< typename TInputImage,
          typename TOutputImage =
            LabelMap< LabelObject< SizeValueType, TInputImage::ImageDimension > > >
class ITK_TEMPLATE_EXPORT BinaryImageToLabelMapFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLabelMapFilter);

  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = BinaryImageToLabelMapFilter;
  using Superclass = ImageToImageFilter< TInputImage, TOutputImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

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

#ifdef ITK_USE_CONCEPT_CHECKING
  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< Self::InputImageDimension,
                                             Self::OutputImageDimension > ) );
#endif

protected:
  BinaryImageToLabelMapFilter();
  ~BinaryImageToLabelMapFilter() override {}
  void PrintSelf(std::ostream & os, Indent indent) const override;

  using InternalLabelType = SizeValueType;

  void DynamicThreadedGenerateData( const RegionType & outputRegionForThread ) override;
  void ComputeEquivalence( const SizeValueType workUnitResultsIndex );
  void MergeLabels( const SizeValueType workUnitResultsIndex );

  void GenerateData() override;

  /** BinaryImageToLabelMapFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() override;

  /** BinaryImageToLabelMapFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) override;

private:
  using OutSizeType = typename TOutputImage::RegionType::SizeType;

  // types to support the run length encoding of lines
  struct runLength
  {
    // run length information - may be a more type safe way of doing this
    SizeValueType length;
    typename InputImageType::IndexType where; // Index of the start of the run
    InternalLabelType label;                  // the initial label of the run
  };

  using lineEncoding = std::vector< runLength >;

  // the map storing lines
  using LineMapType = std::vector< lineEncoding >;

  using OffsetVectorType = std::vector< OffsetValueType >;
  OffsetVectorType m_LineOffsets;

  // the types to support union-find operations
  using UnionFindType = std::vector< InternalLabelType >;
  UnionFindType m_UnionFind;

  using ConsecutiveVectorType = std::vector< OutputPixelType >;
  ConsecutiveVectorType m_Consecutive;

  InternalLabelType LookupSet(const InternalLabelType label);

  void LinkLabels(const InternalLabelType lab1, const InternalLabelType lab2);

  SizeValueType CreateConsecutive();

  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B);

  void CompareLines(lineEncoding & current, const lineEncoding & Neighbour);

  void SetupLineOffsets();

  OutputPixelType m_OutputBackgroundValue;
  InputPixelType  m_InputForegroundValue;

  SizeValueType m_NumberOfObjects;

  std::mutex m_Mutex;

  bool m_FullyConnected;

  struct WorkUnitData
  {
    SizeValueType numberOfLabels;
    SizeValueType firstLineIdForThread;
    SizeValueType firstLineIdToJoin;
    SizeValueType numberOfLineIdsToJoin;
  };

  std::deque< WorkUnitData > m_WorkUnitResults;

#if !defined( ITK_WRAPPING_PARSER )
  LineMapType m_LineMap;
#endif
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#if !defined( ITK_WRAPPING_PARSER )
#include "itkBinaryImageToLabelMapFilter.hxx"
#endif
#endif

#endif
