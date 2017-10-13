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
#include "itkBarrier.h"

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
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef LabelContourImageFilter                         Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(LabelContourImageFilter, InPlaceImageFilter);

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING

  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  itkConceptMacro( SameDimension,
    ( Concept::SameDimension< itkGetStaticConstMacro(InputImageDimension),
                              itkGetStaticConstMacro(OutputImageDimension) > ) );

#endif

  /**
   * Image typedef support
   */
  typedef TInputImage                                 InputImageType;
  typedef typename InputImageType::Pointer            InputImagePointer;
  typedef typename InputImageType::IndexType          InputIndexType;
  typedef typename InputImageType::SizeType           InputSizeType;
  typedef typename InputImageType::OffsetType         InputOffsetType;
  typedef typename InputImageType::PixelType          InputImagePixelType;
  typedef typename InputImageType::SizeValueType      SizeValueType;
  typedef typename InputImageType::OffsetValueType    OffsetValueType;
  typedef typename InputImageType::PixelType          InputPixelType;

  typedef TOutputImage                          OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputRegionType;
  typedef typename OutputImageType::IndexType   OutputIndexType;
  typedef typename OutputImageType::SizeType    OutputSizeType;
  typedef typename OutputImageType::OffsetType  OutputOffsetType;
  typedef typename OutputImageType::PixelType   OutputImagePixelType;

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
  virtual ~LabelContourImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Standard pipeline methods.
   */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void AfterThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedGenerateData(const OutputRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** LabelContourImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** LabelContourImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) ) ITK_OVERRIDE;

private:

  LabelContourImageFilter(const Self &);
  void operator = ( const Self& );

  /** types to support the run length encoding of lines */
  struct RunLength
  {
    /** run length information - may be a more type safe way of doing this */
    SizeValueType length;

    /** Index of the start of the run */
    InputIndexType where;

    InputImagePixelType label;
  };

  typedef std::vector< RunLength >                  LineEncodingType;
  typedef typename LineEncodingType::iterator       LineEncodingIterator;
  typedef typename LineEncodingType::const_iterator LineEncodingConstIterator;

  typedef std::vector< OffsetValueType >            OffsetVectorType;
  typedef typename OffsetVectorType::const_iterator OffsetVectorConstIterator;

  // the map storing lines
  typedef std::vector< LineEncodingType > LineMapType;

  LineMapType           m_LineMap;
  OutputImagePixelType  m_BackgroundValue;
  ThreadIdType          m_NumberOfThreads;
  bool                  m_FullyConnected;

  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B) const;

  void CompareLines(TOutputImage *output, LineEncodingType & current, const LineEncodingType & Neighbour);

  void SetupLineOffsets(OffsetVectorType & LineOffsets);

  void Wait();

  typename Barrier::Pointer m_Barrier;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelContourImageFilter.hxx"
#endif

#endif
