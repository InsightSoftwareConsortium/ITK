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
#ifndef itkBinaryPruningImageFilter_h
#define itkBinaryPruningImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNeighborhoodIterator.h"

namespace itk
{
/** \class BinaryPruningImageFilter
 *
 * \brief This filter removes "spurs" of less than a certain
 * length in the input image.
 *
 * This class is parametrized over the type of the input image
 * and the type of the output image.
 *
 * The input is assumed to be a binary image.
 *
 * This filter is a sequential pruning algorithm and known to be computational time
 * dependable of the image size.  The algorithm is the N-dimensional version
 * of that given for two dimensions in:
 *
 * Rafael C. Gonzales and Richard E. Woods.
 * Digital Image Processing.
 * Addison Wesley, 491-494, (1993).
 *
 * \sa MorphologyImageFilter
 * \sa BinaryErodeImageFilter
 * \sa BinaryDilateImageFilter
 * \sa BinaryThinningImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BinaryPruningImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BinaryPruningImageFilter                        Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryPruningImageFilter, ImageToImageFilter);

  /** Type for input image. */
  typedef   TInputImage InputImageType;

  /** Type for output image: Skelenton of the object.  */
  typedef   TOutputImage OutputImageType;

  /** Type for the region of the input image. */
  typedef typename InputImageType::RegionType RegionType;

  /** Type for the index of the input image. */
  typedef typename RegionType::IndexType IndexType;

  /** Type for the index of the input image. */
  typedef typename InputImageType::PixelType PixelType;

  /** Type for the size of the input image. */
  typedef typename RegionType::SizeType SizeType;

  /** Pointer Type for input image. */
  typedef typename InputImageType::ConstPointer InputImagePointer;

  /** Pointer Type for the output image. */
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Neighborhood iterator type */
  typedef NeighborhoodIterator< TInputImage > NeighborhoodIteratorType;

  /** Get Skelenton by thinning image. */
  OutputImageType * GetPruning();

  /** Set/Get the iteration value */
  itkSetMacro(Iteration, unsigned int);
  itkGetConstMacro(Iteration, unsigned int);

  /** ImageDimension enumeration   */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( SameTypeCheck,
                   ( Concept::SameType< PixelType, typename TOutputImage::PixelType > ) );
  itkConceptMacro( AdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< PixelType > ) );
  itkConceptMacro( IntConvertibleToPixelTypeCheck,
                   ( Concept::Convertible< int, PixelType > ) );
  itkConceptMacro( PixelLessThanIntCheck,
                   ( Concept::LessThanComparable< PixelType, int > ) );
  // End concept checking
#endif

protected:
  BinaryPruningImageFilter();
  virtual ~BinaryPruningImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Compute thinning Image. */
  void GenerateData() ITK_OVERRIDE;

  /** Prepare data. */
  void PrepareData();

  /**  Compute thinning Image. */
  void ComputePruneImage();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryPruningImageFilter);

  unsigned int m_Iteration;
}; // end of BinaryThinningImageFilter class
} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryPruningImageFilter.hxx"
#endif

#endif
