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
#ifndef itkBinaryThinningImageFilter_h
#define itkBinaryThinningImageFilter_h

#include "itkNeighborhoodIterator.h"
#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
/** \class BinaryThinningImageFilter
 *
 * \brief This filter computes one-pixel-wide edges of the input image.
 *
 * This class is parametrized over the type of the input image
 * and the type of the output image.
 *
 * The input is assumed to be a binary image.  If the foreground pixels
 * of the input image do not have a value of 1, they are rescaled to 1
 * internally to simplify the computation.
 *
 * The filter will produce a skeleton of the object.  The output
 * background values are 0, and the foreground values are 1.
 *
 * This filter is a sequential thinning algorithm and known to be computational time
 * dependable on the image size.  The algorithm corresponds with the 2D
 * implementation described in:
 *
 * Rafael C. Gonzales and Richard E. Woods.
 * Digital Image Processing.
 * Addison Wesley, 491-494, (1993).
 *
 * To do: Make this filter ND.
 *
 * \sa MorphologyImageFilter
 * \ingroup ImageEnhancement MathematicalMorphologyImageFilters
 * \ingroup ITKBinaryMathematicalMorphology
 *
 * \wiki
 * \wikiexample{ImageProcessing/BinaryThinningImageFilter,Skeletonize/thin an image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT BinaryThinningImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BinaryThinningImageFilter                       Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThinningImageFilter, ImageToImageFilter);

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

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;

  /** Neighborhood iterator type */
  typedef NeighborhoodIterator< TInputImage > NeighborhoodIteratorType;

  /** Get Skelenton by thinning image. */
  OutputImageType * GetThinning();

  /** ImageDimension enumeration   */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< PixelType > ) );
  itkConceptMacro( InputConvertibleToIntCheck,
                   ( Concept::Convertible< PixelType, int > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, PixelType > ) );
  itkConceptMacro( SameTypeCheck,
                   ( Concept::SameType< PixelType, typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  BinaryThinningImageFilter();
  virtual ~BinaryThinningImageFilter() ITK_OVERRIDE {}

  /** Compute thinning Image. */
  void GenerateData() ITK_OVERRIDE;

  /** Prepare data. */
  void PrepareData();

  /**  Compute thinning Image. */
  void ComputeThinImage();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryThinningImageFilter);
};                                         // end of BinaryThinningImageFilter
                                           // class
} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryThinningImageFilter.hxx"
#endif

#endif
