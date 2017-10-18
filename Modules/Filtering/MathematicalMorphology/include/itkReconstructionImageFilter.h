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
#ifndef itkReconstructionImageFilter_h
#define itkReconstructionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include <queue>

//#define BASIC
#define COPY

#ifdef COPY
#include "itkNeighborhoodAlgorithm.h"
#endif

namespace itk
{
/** \class ReconstructionImageFilter
 * \brief Performs a grayscale geodesic reconstruction -- for
 * performance comparison with GrayscaleGeodesicDilateImageFilter.
 *
 * This filter uses Luc Vincent's algorithm, which employs raster and
 * antiraster propagation steps followed by a FIFO based propagation
 * step. "Morphological grayscale reconstruction in image analysis -
 * applications and efficient algorithms" -- IEEE Transactions on
 * Image processing, Vol 2, No 2, pp 176-201, April 1993
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa MorphologicalReonstructionErosionImageFilter MorphologicalReonstructionDilationImageFilter
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage, typename TCompare >
class ITK_TEMPLATE_EXPORT ReconstructionImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ReconstructionImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef typename InputImageType::SizeType      ISizeType;
  typedef TInputImage                            MarkerImageType;
  typedef typename MarkerImageType::Pointer      MarkerImagePointer;
  typedef typename MarkerImageType::ConstPointer MarkerImageConstPointer;
  typedef typename MarkerImageType::RegionType   MarkerImageRegionType;
  typedef typename MarkerImageType::PixelType    MarkerImagePixelType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef typename InputImageType::IndexType     InputImageIndexType;
  typedef TInputImage                            MaskImageType;
  typedef typename MaskImageType::Pointer        MaskImagePointer;
  typedef typename MaskImageType::ConstPointer   MaskImageConstPointer;
  typedef typename MaskImageType::RegionType     MaskImageRegionType;
  typedef typename MaskImageType::PixelType      MaskImagePixelType;
  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;
  typedef typename OutputImageType::IndexType    OutputImageIndexType;

  /** ImageDimension constants */
  /** ImageDimension constants */
  itkStaticConstMacro(MarkerImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(MaskImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ReconstructionImageFilter,
               ImageToImageFilter);

  /** Set/Get the marker image. Traditionally, the marker image must
   * be pixelwise less than or equal to the mask image (for dilation),
   * however this filter implicitly applies a mask to force the
   * constraint to hold. The marker image the
   * image that is dilated by this filter. */
  void SetMarkerImage(const MarkerImageType *);

  const MarkerImageType * GetMarkerImage();

  /** Set/Get the mask image. The mask image is used to "mask" the
   * dilated marker image. The mask operation is a pixelwise
   * minimum. */
  void SetMaskImage(const MaskImageType *);

  const MaskImageType * GetMaskImage();

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
   * Perform a padding of the image internally to increase the performance
   * of the filter. UseInternalCopy can be set to false to reduce the memory
   * usage.
   */
  itkSetMacro(UseInternalCopy, bool);
  itkGetConstReferenceMacro(UseInternalCopy, bool);
  itkBooleanMacro(UseInternalCopy);

protected:
  ReconstructionImageFilter();
  ~ReconstructionImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** ValuedRegionalExtremaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** ValuedRegionalExtremaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

  /**
   * the value of the border - used in boundary condition.
   */
  typename TInputImage::PixelType m_MarkerValue;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ReconstructionImageFilter);

  bool m_FullyConnected;
  bool m_UseInternalCopy;

  typedef typename itk::NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< OutputImageType > FaceCalculatorType;

  typedef typename FaceCalculatorType::FaceListType           FaceListType;
  typedef typename FaceCalculatorType::FaceListType::iterator FaceListTypeIt;

  typedef ImageRegionConstIterator< InputImageType > InputIteratorType;
  typedef ImageRegionIterator< OutputImageType >     OutputIteratorType;

  typedef typename OutputImageType::IndexType               OutIndexType;
  typedef typename InputImageType::IndexType                InIndexType;
  typedef ConstShapedNeighborhoodIterator< InputImageType > CNInputIterator;
  typedef ShapedNeighborhoodIterator< OutputImageType >     NOutputIterator;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReconstructionImageFilter.hxx"
#endif

#endif
