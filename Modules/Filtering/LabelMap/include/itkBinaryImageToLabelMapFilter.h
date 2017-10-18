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
#include <list>
#include <map>
#include <vector>
#include "itkProgressReporter.h"
#include "itkBarrier.h"
#include "itkLabelMap.h"
#include "itkLabelObject.h"
#include "itkImageRegionSplitterDirection.h"

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
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef BinaryImageToLabelMapFilter                     Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

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
  typedef typename Superclass::InputImagePointer InputImagePointer;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType      OutputPixelType;
  typedef typename TInputImage::PixelType       InputPixelType;
  typedef typename TInputImage::SizeValueType   SizeValueType;
  typedef typename TInputImage::OffsetValueType OffsetValueType;
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);

  /**
   * Image typedef support
   */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef TOutputImage                      OutputImageType;
  typedef typename TOutputImage::RegionType RegionType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::SizeType   OutputSizeType;
  typedef typename TOutputImage::OffsetType OutputOffsetType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;

  typedef std::list< IndexType > ListType;

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
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageDimension),
                                             itkGetStaticConstMacro(OutputImageDimension) > ) );
#endif

protected:
  BinaryImageToLabelMapFilter();
  virtual ~BinaryImageToLabelMapFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef SizeValueType InternalLabelType;

  /**
   * Standard pipeline method.
   */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void AfterThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedGenerateData(const RegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  /** BinaryImageToLabelMapFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** BinaryImageToLabelMapFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  /** Provide an ImageRegionSplitter that does not split along the first
   * dimension -- we assume the data is complete along this dimension when
   * threading. */
  virtual const ImageRegionSplitterBase* GetImageRegionSplitter() const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLabelMapFilter);

  // some additional types
  typedef typename TOutputImage::RegionType::SizeType OutSizeType;

  // types to support the run length encoding of lines
  class runLength
  {
  public:
    // run length information - may be a more type safe way of doing this
    SizeValueType length;
    typename InputImageType::IndexType where; // Index of the start of the run
    InternalLabelType label;                  // the initial label of the run
  };

  typedef std::vector< runLength > lineEncoding;

  // the map storing lines
  typedef std::vector< lineEncoding > LineMapType;

  typedef std::vector< OffsetValueType > OffsetVectorType;

  // the types to support union-find operations
  typedef std::vector< InternalLabelType > UnionFindType;
  UnionFindType m_UnionFind;

  typedef std::vector< OutputPixelType > ConsecutiveVectorType;
  ConsecutiveVectorType m_Consecutive;

  // functions to support union-find operations
  void InitUnion(const InternalLabelType size);

  void InsertSet(const InternalLabelType label);

  InternalLabelType LookupSet(const InternalLabelType label);

  void LinkLabels(const InternalLabelType lab1, const InternalLabelType lab2);

  SizeValueType CreateConsecutive();

  //////////////////
  bool CheckNeighbors(const OutputIndexType & A,
                      const OutputIndexType & B);

  void CompareLines(lineEncoding & current, const lineEncoding & Neighbour);

  void FillOutput(const LineMapType & LineMap,
                  ProgressReporter & progress);

  void SetupLineOffsets(OffsetVectorType & LineOffsets);

  void Wait();

  OutputPixelType m_OutputBackgroundValue;
  InputPixelType  m_InputForegroundValue;

  SizeValueType m_NumberOfObjects;

  bool m_FullyConnected;

  std::vector< SizeValueType >   m_NumberOfLabels;
  std::vector< SizeValueType >   m_FirstLineIdToJoin;

  typename Barrier::Pointer m_Barrier;

  ImageRegionSplitterDirection::Pointer m_ImageRegionSplitter;

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
