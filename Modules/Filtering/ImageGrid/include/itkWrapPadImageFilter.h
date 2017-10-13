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
#ifndef itkWrapPadImageFilter_h
#define itkWrapPadImageFilter_h

#include "itkPadImageFilter.h"

#include "itkPeriodicBoundaryCondition.h"

namespace itk
{
/** \class WrapPadImageFilter
 * \brief Increase the image size by padding with replicants of the
 * input image value.
 *
 * WrapPadImageFilter changes the image bounds of an image.  Added
 * pixels are filled in with a wrapped replica of the input image.
 * For instance, if the output image needs a pixel that is <b>two pixels
 * to the left of the LargestPossibleRegion</b> of the input image, the
 * value assigned will be from the pixel <b>two pixels inside the right
 * boundary of the LargestPossibleRegion</b>. The image bounds of the
 * output must be specified.
 *
 * \image html WrapPadImageFilter.png "Visual explanation of padding regions."
 *
 * This filter is implemented as a multithreaded filter.  It provides a
 * ThreadedGenerateData() method for its implementation.
 *
 * \ingroup GeometricTransform
 * \sa MirrorPadImageFilter, ConstantPadImageFilter
 * \ingroup ITKImageGrid
 *
 * \wiki
 * \wikiexample{Images/WrapPadImageFilter,Pad an image by wrapping}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT WrapPadImageFilter:
  public PadImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef WrapPadImageFilter                          Self;
  typedef PadImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(WrapPadImageFilter, PadImageFilter);

  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::OutputImagePointer     OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  /** Typedef to describe the output image region type. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType  InputImageRegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType  InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename Superclass::OutputImageIndexType OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType  InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType  OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType   InputImageSizeType;

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputImagePixelType, OutputImagePixelType > ) );
  // End concept checking
#endif

protected:
  WrapPadImageFilter();
  ~WrapPadImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WrapPadImageFilter);

  PeriodicBoundaryCondition< TInputImage, TOutputImage > m_InternalBoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWrapPadImageFilter.hxx"
#endif

#endif
