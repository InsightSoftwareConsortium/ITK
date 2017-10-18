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
#ifndef itkInvertIntensityImageFilter_h
#define itkInvertIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
/**
 * \class InvertIntensityTransform
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename  TOutput >
class ITK_TEMPLATE_EXPORT InvertIntensityTransform
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  InvertIntensityTransform() { m_Maximum = NumericTraits< TInput >::max(); }
  ~InvertIntensityTransform() {}

  void SetMaximum(TOutput max) { m_Maximum = max; }

  bool operator!=(const InvertIntensityTransform & other) const
  {
    if ( m_Maximum != other.m_Maximum )
      {
      return true;
      }
    return false;
  }

  bool operator==(const InvertIntensityTransform & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & x) const
  {
    TOutput result = static_cast< TOutput >( m_Maximum - x );

    return result;
  }

private:
  TInput m_Maximum;
};
}  // end namespace functor

/** \class InvertIntensityImageFilter
 * \brief Invert the intensity of an image.
 *
 * InvertIntensityImageFilter inverts intensity of pixels by
 * subtracting pixel value to a maximum value. The maximum value can
 * be set with SetMaximum and defaults the maximum of input pixel
 * type. This filter can be used to invert, for example, a binary
 * image, a distance map, etc.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa IntensityWindowingImageFilter ShiftScaleImageFilter
 * \ingroup IntensityImageFilters  MultiThreaded
 *
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/InvertIntensityImageFilter,Invert an image}
 * \endwiki
 */
template< typename  TInputImage, typename  TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT InvertIntensityImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::InvertIntensityTransform<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef InvertIntensityImageFilter Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::InvertIntensityTransform<
                                     typename TInputImage::PixelType,
                                     typename TOutputImage::PixelType > > Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename TOutputImage::PixelType                   OutputPixelType;
  typedef typename TInputImage::PixelType                    InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(InvertIntensityImageFilter,
               UnaryFunctorImageFilter);

  /** Set/Get the maximum intensity value for the inversion. */
  itkSetMacro(Maximum, InputPixelType);
  itkGetConstReferenceMacro(Maximum, InputPixelType);

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  // End concept checking
#endif

protected:
  InvertIntensityImageFilter();
  virtual ~InvertIntensityImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(InvertIntensityImageFilter);

  InputPixelType m_Maximum;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInvertIntensityImageFilter.hxx"
#endif

#endif
