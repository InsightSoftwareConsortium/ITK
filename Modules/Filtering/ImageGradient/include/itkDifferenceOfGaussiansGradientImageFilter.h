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
#ifndef itkDifferenceOfGaussiansGradientImageFilter_h
#define itkDifferenceOfGaussiansGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class DifferenceOfGaussiansGradientImageFilter
 * \brief Performs difference-of-gaussians gradient detection
 *
 * \ingroup ImageEnhancement
 * \ingroup GradientFilters
 *
 * \ingroup ITKImageGradient
 */
template< typename TInputImage, typename TDataType >
class ITK_TEMPLATE_EXPORT DifferenceOfGaussiansGradientImageFilter:
  public ImageToImageFilter< TInputImage,
                             Image< CovariantVector< TDataType, TInputImage::ImageDimension >,
                                    TInputImage::ImageDimension > >
{
public:
  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef DifferenceOfGaussiansGradientImageFilter Self;

  /** Output image typedef. The output image is always an n-dimensional
   * image of n-dimensional vectors of doubles. */
  typedef Image< CovariantVector< TDataType, itkGetStaticConstMacro(NDimensions) >, itkGetStaticConstMacro(NDimensions) >
  TOutputImage;

  /** Standard class typedefs. */
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DifferenceOfGaussiansGradientImageFilter, ImageToImageFilter);

  /** Image size typedef. */
  typedef Size< itkGetStaticConstMacro(NDimensions) > SizeType;

  /** Image index typedef. */
  typedef typename TInputImage::IndexType IndexType;

  /** Image pixel value typedef. */
  typedef typename TInputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TInputImage::RegionType OutputImageRegionType;

  /** Set/Get the member variables. */
  itkGetConstMacro(Width, unsigned int);
  itkSetMacro(Width, unsigned int);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( DataTypeHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< TDataType > ) );
  // End concept checking
#endif

protected:
  DifferenceOfGaussiansGradientImageFilter();
  virtual ~DifferenceOfGaussiansGradientImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Method for evaluating the implicit function over the image. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DifferenceOfGaussiansGradientImageFilter);

  unsigned int m_Width;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDifferenceOfGaussiansGradientImageFilter.hxx"
#endif

#endif
