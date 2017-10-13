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
#include "itkBarrier.h"
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
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef BinaryContourImageFilter                        Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(BinaryContourImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Image typedef support
   */
  typedef TInputImage                                 InputImageType;
  typedef typename InputImageType::Pointer            InputImagePointer;
  typedef typename InputImageType::ConstPointer       InputImageConstPointer;
  typedef typename InputImageType::IndexType          IndexType;
  typedef typename InputImageType::SizeType           SizeType;
  typedef typename InputImageType::OffsetType         OffsetType;
  typedef typename InputImageType::PixelType          InputImagePixelType;
  typedef typename InputImageType::InternalPixelType  InputInternalPixelType;

  typedef TOutputImage                                OutputImageType;
  typedef typename OutputImageType::Pointer           OutputImagePointer;
  typedef typename OutputImageType::RegionType        RegionType;
  typedef typename OutputImageType::IndexType         OutputIndexType;
  typedef typename OutputImageType::SizeType          OutputSizeType;
  typedef typename OutputImageType::OffsetType        OutputOffsetType;
  typedef typename OutputImageType::PixelType         OutputImagePixelType;
  typedef typename OutputImageType::InternalPixelType OutputInternalPixelType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      OutputImageType::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< itkGetStaticConstMacro(ImageDimension),
                                             itkGetStaticConstMacro(OutputImageDimension) > ) );
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
  virtual ~BinaryContourImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Standard pipeline methods.
   */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void AfterThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedGenerateData(const RegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** BinaryContourImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** BinaryContourImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) ) ITK_OVERRIDE;

private:
  BinaryContourImageFilter(const Self &); //Purposefully not implemented
  void operator = ( const Self &);        //Purposefully not implemented

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

  typedef std::vector< runLength >                  LineEncodingType;
  typedef typename LineEncodingType::iterator       LineEncodingIterator;
  typedef typename LineEncodingType::const_iterator LineEncodingConstIterator;

  // the map storing lines
  typedef std::vector< LineEncodingType > LineMapType;

  typedef std::vector< OffsetValueType > OffsetVec;

  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B);

  void CompareLines(LineEncodingType & current,
                    const LineEncodingType & Neighbour);

  void SetupLineOffsets(OffsetVec & LineOffsets);

  void Wait();

  Barrier::Pointer m_Barrier;

  LineMapType   m_ForegroundLineMap;
  LineMapType   m_BackgroundLineMap;
  ThreadIdType  m_NumberOfThreads;

  InputImagePixelType  m_ForegroundValue;
  OutputImagePixelType m_BackgroundValue;
  bool                 m_FullyConnected;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryContourImageFilter.hxx"
#endif

#endif
