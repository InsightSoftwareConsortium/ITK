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
#ifndef itkBinaryContourImageFilter_h
#define itkBinaryContourImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkConceptChecking.h"
#include <vector>

namespace itk
{
/**
 * \class BinaryContourImageFilter
 * \brief Labels the pixels on the border of the objects in a binary image.
 *
 * BinaryContourImageFilter takes a binary image as input, where the pixels
 * in the objects are the pixels with a value equal to ForegroundValue.
 * Only the pixels on the contours of the objects are kept. The pixels not
 * on the border are changed to BackgroundValue.
 *
 * The connectivity can be changed to minimum or maximum connectivity with
 * SetFullyConnected(). Full connectivity produces thicker contours.
 *
 * https://hdl.handle.net/1926/1352
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa LabelContourImageFilter BinaryErodeImageFilter SimpleContourExtractorImageFilter
 * \ingroup ITKImageLabel
 *
 * \wiki
 * \wikiexample{EdgesAndGradients/BinaryContourImageFilter,Extract the boundaries of connected regions in a binary image}
 * \wikiexample{EdgesAndGradients/BinaryBoundaries,Extract the inner and outer boundaries of blobs in a binary image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BinaryContourImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryContourImageFilter);

  /**
   * Standard "Self" & Superclass typedef.
   */
  using Self = BinaryContourImageFilter;
  using Superclass = InPlaceImageFilter< TInputImage, TOutputImage >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(BinaryContourImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Image type alias support
   */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;
  using OffsetType = typename InputImageType::OffsetType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputInternalPixelType = typename InputImageType::InternalPixelType;

  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using RegionType = typename OutputImageType::RegionType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputSizeType = typename OutputImageType::SizeType;
  using OutputOffsetType = typename OutputImageType::OffsetType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using OutputInternalPixelType = typename OutputImageType::InternalPixelType;

  static constexpr unsigned int ImageDimension = OutputImageType::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  static constexpr unsigned int InputImageDimension = InputImageType::ImageDimension;

  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< Self::ImageDimension,
                                             Self::OutputImageDimension > ) );
#endif

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /**
   * Set/Get the background value used to mark the pixels not on the border of the
   * objects.
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get the foreground value used to identify the objects in the input and
   * output images.
   */
  itkSetMacro(ForegroundValue, InputImagePixelType);
  itkGetConstMacro(ForegroundValue, InputImagePixelType);

protected:

  BinaryContourImageFilter();
  ~BinaryContourImageFilter() override {}

  void PrintSelf(std::ostream & os, Indent indent) const override;

  SizeValueType IndexToLinearIndex(IndexType index);

  void GenerateData() override;

  void BeforeThreadedGenerateData() override;

  void AfterThreadedGenerateData() override;

  void DynamicThreadedGenerateData(const RegionType& outputRegionForThread) override;

  void ThreadedIntegrateData(const RegionType& outputRegionForThread);


  /** BinaryContourImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() override;

  /** BinaryContourImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) ) override;

private:
  // types to support the run length encoding of lines
  struct runLength
  {
    runLength( const OffsetValueType& iLength, const IndexType& iWhere ) :
      m_Length( iLength ), m_Where( iWhere ) {}

    // run length information - may be a more type safe way of doing this
    OffsetValueType m_Length;

    // Index of the start of the run
    IndexType       m_Where;
  };

  using LineEncodingType = std::vector< runLength >;
  using LineEncodingIterator = typename LineEncodingType::iterator;
  using LineEncodingConstIterator = typename LineEncodingType::const_iterator;

  // the map storing lines
  using LineMapType = std::vector< LineEncodingType >;

  using OffsetVec = std::vector< OffsetValueType >;

  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B);

  void CompareLines(LineEncodingType & current,
                    const LineEncodingType & Neighbour);

  void SetupLineOffsets(OffsetVec & LineOffsets);

  LineMapType   m_ForegroundLineMap;
  LineMapType   m_BackgroundLineMap;
  ThreadIdType  m_NumberOfWorkUnits;

  InputImagePixelType  m_ForegroundValue;
  OutputImagePixelType m_BackgroundValue;
  bool                 m_FullyConnected;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryContourImageFilter.hxx"
#endif

#endif
