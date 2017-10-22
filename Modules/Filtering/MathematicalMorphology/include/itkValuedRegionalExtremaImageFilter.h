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
#ifndef itkValuedRegionalExtremaImageFilter_h
#define itkValuedRegionalExtremaImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"
#include <stack>

namespace itk
{
/** \class ValuedRegionalExtremaImageFilter
 *
 * \brief Uses a flooding algorithm to set all voxels that are not a
 * regional extrema to the max or min of the pixel type.
 *
 * This is the class used by ValuedRegionalMinimaImageFilter and
 * ValuedRegionalMaximaImageFilter. There is no suppression of regional
 * minima based on dynamics, as available in HMinimaImageFilter. This
 * flooding algorithm is a very simple one, but I'm not sure where it
 * came from - I certainly didn't invent it.
 *
 * Let's consider the case of regional minima.
 * The basic algorithm is:
 *    Boundary conditions are such that the image is logically
 *    surrounded by a border that is either maximal or minimal for the
 *    pixel type. An optimized version could explicitly set the border
 *    to avoid the need for boundary checks. For regional minima the
 *    boundary is set to the maximal value for the pixel type.
 *
 *    Pixels are visited in raster order. The neighbors of each pixel
 *    are examined. If any neighbor is greater than the centre, then
 *    the centre pixel cannot be a regional minima. The centre pixel
 *    is part of a flat region (consisting of at least one pixel) that
 *    is therefore not a regional minima either. This region is set to
 *    the maximum value for the pixel type using a flooding algorithm.
 *
 *    There are some minor complications that prevent pixels being
 *    examined more than once -- basically check that the output value
 *    is less than the maximum for the pixel type.
 *
 * The implementation uses the functor model from itkMaximumImageFilter.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Finding regional extrema - methods and performance"
 * by Beare R., Lehmann G.
 * https://hdl.handle.net/1926/153
 * http://www.insight-journal.org/browse/publication/65
 *
 * \author Richard Beare. Department of Medicine, Monash University,
 * Melbourne, Australia.
 *
 * \sa ValuedRegionalMinimaImageFilter, ValuedRegionalMaximaImageFilter,
 * \sa HMinimaImageFilter
 *
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage,
          typename TFunction1, typename TFunction2 >
class ITK_TEMPLATE_EXPORT ValuedRegionalExtremaImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ValuedRegionalExtremaImageFilter                Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef TOutputImage                           OutputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef typename InputImageType::SizeType      ISizeType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ValuedRegionalExtremaImageFilter, ImageToImageFilter);

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
   * Set/Get the value used to mark all pixels which are not extrema.
   */
  itkSetMacro(MarkerValue, typename TInputImage::PixelType);
  itkGetConstReferenceMacro(MarkerValue, typename TInputImage::PixelType);

  /**
   * Get whether the image is flat or not.
   */
  itkGetConstMacro(Flat, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasPixelTraitsCheck,
                   ( Concept::HasPixelTraits< InputImagePixelType > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputImagePixelType > ) );
  // End concept checking
#endif

protected:
  ValuedRegionalExtremaImageFilter();
  ~ValuedRegionalExtremaImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** ValuedRegionalExtremaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** ValuedRegionalExtremaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ValuedRegionalExtremaImageFilter);

  typename TInputImage::PixelType m_MarkerValue;

  bool m_FullyConnected;
  bool m_Flat;

  typedef typename OutputImageType::IndexType               OutIndexType;
  typedef typename InputImageType::IndexType                InIndexType;
  typedef ConstShapedNeighborhoodIterator< InputImageType > ConstInputIterator;
  typedef ShapedNeighborhoodIterator< OutputImageType >     NOutputIterator;
  typedef std::stack< OutIndexType >                        IndexStack;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkValuedRegionalExtremaImageFilter.hxx"
#endif

#endif
