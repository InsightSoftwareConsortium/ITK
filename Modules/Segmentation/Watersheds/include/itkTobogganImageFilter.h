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
#ifndef itkTobogganImageFilter_h
#define itkTobogganImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/** \class TobogganImageFilter
 * \brief toboggan image segmentation
 * The Toboggan segmentation takes a gradient magnitude image
 * as input and produces an (over-)segmentation of the image based
 * on connecting each pixel to a local minimum of gradient.  It is
 * roughly equivalent to a watershed segmentation of the lowest level.
 *
 * The output is a 4 connected labeled map of the image.
 * \ingroup Segmentation
 * \ingroup ITKWatersheds
 */

template< typename TInputImage >
class ITK_TEMPLATE_EXPORT TobogganImageFilter:
  public ImageToImageFilter<
    TInputImage,
    Image< IdentifierType, TInputImage::ImageDimension > >
{
public:
  /** Standard "Self" typedef.   */
  typedef TobogganImageFilter Self;

  /** The type of input image.   */
  typedef TInputImage InputImageType;

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** The type of output image.   */
  typedef Image< IdentifierType, itkGetStaticConstMacro(NDimensions) > OutputImageType;

  /** Output image pixel type. */
  typedef typename OutputImageType::PixelType OutputImagePixelType;

  /** Input image pixel type. */
  typedef typename InputImageType::PixelType InputImagePixelType;

  /** Dimension of the input and output images. */
  enum { ImageDimension = InputImageType::ImageDimension };

  /** Other convenient typedefs   */
  typedef typename InputImageType::RegionType   RegionType;
  typedef typename InputImageType::SizeType     SizeType;
  typedef typename InputImageType::IndexType    IndexType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename OutputImageType::Pointer     OutputImagePointer;

  /** Standard super class typedef support. */
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;

  /** Typedef support for the input image scalar value type. */
  typedef typename InputImageType::PixelType ScalarType;

  /** Smart pointer typedef support  */
  typedef SmartPointer< Self > Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(TobogganImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard process object method.  This filter is not multithreaded. */
  void GenerateData() ITK_OVERRIDE;

  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  void EnlargeOutputRequestedRegion(DataObject *) ITK_OVERRIDE;

  /** Neighborhood iterator type */
  typedef ConstNeighborhoodIterator< TInputImage >
  NeighborhoodIteratorType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( LessThanComparableCheck,
                   ( Concept::LessThanComparable< InputImagePixelType > ) );
  itkConceptMacro( OStreamWritableCheck,
                   ( Concept::OStreamWritable< InputImagePixelType > ) );
  // End concept checking
#endif

protected:
  TobogganImageFilter();
  ~TobogganImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TobogganImageFilter);
};                                   // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTobogganImageFilter.hxx"
#endif

#endif
