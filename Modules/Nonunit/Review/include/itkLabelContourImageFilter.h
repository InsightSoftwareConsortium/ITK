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
#ifndef __itkLabelContourImageFilter_h
#define __itkLabelContourImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkImage.h"
#include "itkConceptChecking.h"
#include <vector>
#include <map>
#include "itkProgressReporter.h"
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
 * http://hdl.handle.net/1926/1352
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa BinaryContourImageFilter
 */

template< class TInputImage, class TOutputImage >
class ITK_EXPORT LabelContourImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef LabelContourImageFilter                         Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Types from the Superclass
   */
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType          InputPixelType;
  typedef typename TInputImage::InternalPixelType  InputInternalPixelType;

  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /**
   * Image typedef support
   */
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::IndexType       IndexType;
  typedef typename TInputImage::SizeType        SizeType;
  typedef typename TInputImage::OffsetType      OffsetType;
  typedef typename TInputImage::PixelType       InputImagePixelType;
  typedef typename TInputImage::SizeValueType   SizeValueType;
  typedef typename TInputImage::OffsetValueType OffsetValueType;

  typedef TOutputImage                      OutputImageType;
  typedef typename TOutputImage::RegionType RegionType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::SizeType   OutputSizeType;
  typedef typename TOutputImage::OffsetType OutputOffsetType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;

  typedef std::list< IndexType > ListType;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(LabelContourImageFilter, ImageToImageFilter);

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

  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageDimension),
                                             itkGetStaticConstMacro(OutputImageDimension) > ) );

  /**
   * Set/Get the background value used to identify the objects and mark the
   * pixels not on the border of the objects.
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);
protected:
  LabelContourImageFilter()
  {
    m_FullyConnected = false;
    m_BackgroundValue = NumericTraits< OutputImagePixelType >::Zero;
    m_NumberOfThreads = 0;
    this->SetInPlace(false);
  }

  virtual ~LabelContourImageFilter() {}
  LabelContourImageFilter(const Self &) {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /**
   * Standard pipeline methods.
   */
  void BeforeThreadedGenerateData();

  void AfterThreadedGenerateData();

  void ThreadedGenerateData(const RegionType & outputRegionForThread, int threadId);

  /** LabelContourImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** LabelContourImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) );
private:
  OutputImagePixelType m_BackgroundValue;
  bool                 m_FullyConnected;

  // some additional types
  typedef typename TOutputImage::RegionType::SizeType OutSizeType;

  // types to support the run length encoding of lines
  class runLength
  {
public:
    // run length information - may be a more type safe way of doing this
    SizeValueType length;
    typename InputImageType::IndexType where; // Index of the start of the run
    InputImagePixelType label;
  };

  typedef std::vector< runLength > lineEncoding;

  // the map storing lines
  typedef std::vector< lineEncoding > LineMapType;

  typedef std::vector< OffsetValueType > OffsetVec;

  // the types to support union-find operations
  typedef std::vector< SizeValueType > UnionFindType;

  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B);

  void CompareLines(lineEncoding & current, const lineEncoding & Neighbour);

  void SetupLineOffsets(OffsetVec & LineOffsets);

  void Wait()
  {
    if ( m_NumberOfThreads > 1 )
      {
      m_Barrier->Wait();
      }
  }

  typename Barrier::Pointer m_Barrier;

  LineMapType m_LineMap;
  int         m_NumberOfThreads;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelContourImageFilter.txx"
#endif

#endif
