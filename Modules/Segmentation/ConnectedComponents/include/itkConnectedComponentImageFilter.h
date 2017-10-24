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
#ifndef itkConnectedComponentImageFilter_h
#define itkConnectedComponentImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include <vector>
#include <map>
#include "itkProgressReporter.h"
#include "itkBarrier.h"

namespace itk
{
/**
 * \class ConnectedComponentImageFilter
 * \brief Label the objects in a binary image
 *
 * ConnectedComponentImageFilter labels the objects in a binary image
 * (non-zero pixels are considered to be objects, zero-valued pixels
 * are considered to be background).
 * Each distinct object is assigned a unique label. The filter experiments
 * with some improvements to the existing implementation, and is based on
 * run length encoding along raster lines.
 * The final object labels start with 1 and are consecutive. Objects
 * that are reached earlier by a raster order scan have a lower
 * label. This is different to the behaviour of the original connected
 * component image filter which did not produce consecutive labels or
 * impose any particular ordering.
 *
 * After the filter is executed, ObjectCount holds the number of connected components.
 *
 * \sa ImageToImageFilter
 *
 * \ingroup SingleThreaded
 * \ingroup ITKConnectedComponents
 *
 * \wiki
 * \wikiexample{ImageProcessing/ConnectedComponentImageFilter,Label connected components in a binary image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage, typename TMaskImage = TInputImage >
class ITK_TEMPLATE_EXPORT ConnectedComponentImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef ConnectedComponentImageFilter                   Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

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
  typedef typename TMaskImage::PixelType           MaskPixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /**
   * Image typedef support
   */
  typedef TInputImage                      InputImageType;
  typedef TMaskImage                       MaskImageType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef TOutputImage                      OutputImageType;
  typedef typename TOutputImage::RegionType RegionType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::SizeType   OutputSizeType;
  typedef typename TOutputImage::OffsetType OutputOffsetType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;

  typedef std::list< IndexType >          ListType;
  typedef typename MaskImageType::Pointer MaskImagePointer;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(ConnectedComponentImageFilter, ImageToImageFilter);

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

  /** Type used as identifier of the different component labels. */
  typedef IdentifierType   LabelType;

  // only set after completion
  itkGetConstReferenceMacro(ObjectCount, LabelType);

  // Concept checking -- input and output dimensions must be the same
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageDimension),
                                             itkGetStaticConstMacro(OutputImageDimension) > ) );
  itkConceptMacro( OutputImagePixelTypeIsInteger, ( Concept::IsInteger< OutputImagePixelType > ) );

  itkSetInputMacro(MaskImage, MaskImageType);
  itkGetInputMacro(MaskImage, MaskImageType);

  /**
   * Set the pixel intensity to be used for background (non-object)
   * regions of the image in the output. Note that this does NOT set
   * the background value to be used in the input image.
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

protected:
  ConnectedComponentImageFilter()
  {
    m_FullyConnected = false;
    m_ObjectCount = 0;
    m_BackgroundValue = NumericTraits< OutputImagePixelType >::ZeroValue();

    // implicit
    // #0 "Primary" required

    //  #1 "MaskImage" optional
    Self::AddOptionalInputName("MaskImage",1);
  }

  virtual ~ConnectedComponentImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Standard pipeline methods.
   */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void AfterThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  /** ConnectedComponentImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** ConnectedComponentImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject * itkNotUsed(output) ) ITK_OVERRIDE;

  bool m_FullyConnected;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConnectedComponentImageFilter);

  LabelType            m_ObjectCount;
  OutputImagePixelType m_BackgroundValue;

  // some additional types
  typedef typename TOutputImage::RegionType::SizeType OutSizeType;

  // types to support the run length encoding of lines
  class runLength
  {
public:
    // run length information - may be a more type safe way of doing this
    typename TInputImage::OffsetValueType   length;
    typename TInputImage::IndexType         where;   // Index of the start of the run
    LabelType                               label;   // the initial label of the run
  };

  typedef std::vector< runLength > lineEncoding;

  // the map storing lines
  typedef std::vector< lineEncoding > LineMapType;

  typedef std::vector< typename TInputImage::OffsetValueType > OffsetVec;

  // the types to support union-find operations
  typedef std::vector< LabelType > UnionFindType;
  UnionFindType m_UnionFind;
  UnionFindType m_Consecutive;

  // functions to support union-find operations
  void InitUnion( SizeValueType size )
  {
    m_UnionFind = UnionFindType(size + 1);
  }

  void InsertSet(const LabelType label);

  SizeValueType LookupSet(const LabelType label);

  void LinkLabels(const LabelType lab1, const LabelType lab2);

  SizeValueType CreateConsecutive();

  //////////////////
  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B);

  void CompareLines(lineEncoding & current, const lineEncoding & Neighbour);

  void FillOutput(const LineMapType & LineMap,
                  ProgressReporter & progress);

  void SetupLineOffsets(OffsetVec & LineOffsets);

  void Wait()
  {
    // use m_NumberOfLabels.size() to get the number of thread used
    if ( m_NumberOfLabels.size() > 1 )
      {
      m_Barrier->Wait();
      }
  }

  typename std::vector< IdentifierType > m_NumberOfLabels;
  typename std::vector< IdentifierType > m_FirstLineIdToJoin;

  typename Barrier::Pointer m_Barrier;

  typename TInputImage::ConstPointer m_Input;
#if !defined( ITK_WRAPPING_PARSER )
  LineMapType m_LineMap;
#endif
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#if !defined( ITK_WRAPPING_PARSER )
#include "itkConnectedComponentImageFilter.hxx"
#endif
#endif

#endif
