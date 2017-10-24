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
#ifndef itkZeroFluxNeumannPadImageFilter_h
#define itkZeroFluxNeumannPadImageFilter_h

#include "itkPadImageFilter.h"

#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{

/** \class ZeroFluxNeumannPadImageFilter
 * \brief Increase the image size by padding according to the
 * zero-flux Neumann boundary condition.
 *
 * A filter which extends the image size and fill the missing pixels
 * according to a Neumann boundary condition where first,
 * upwind derivatives on the boundary are zero.  This is a useful condition
 * in solving some classes of differential equations.
 *
 * For example, invoking this filter on an image with a corner like:
 * \code
 *               * * * * * * *
 *               * * * * * * *
 *               * * 1 2 3 4 5  (where * denotes pixels that lie
 *               * * 3 3 5 5 6          outside of the image boundary)
 *               * * 4 4 6 7 8
 * \endcode
 * returns the following padded image:
 * \code
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               1 1 1 2 3 4 5
 *               3 3 3 3 5 5 6   (note the corner values)
 *               4 4 4 4 6 7 8
 * \endcode
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup GeometricTransform
 * \ingroup ITKImageGrid
 * \sa WrapPadImageFilter, MirrorPadImageFilter, ConstantPadImageFilter, ZeroFluxNeumannBoundaryCondition
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ZeroFluxNeumannPadImageFilter :
    public PadImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ZeroFluxNeumannPadImageFilter              Self;
  typedef PadImageFilter<TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroFluxNeumannPadImageFilter, PadImageFilter);

  /** Typedef to describe the output image region type. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename Superclass::InputImageRegionType  InputImageRegionType;
  typedef typename Superclass::InputImageRegionType  RegionType;

  /** Typedef to describe the type of pixel. */
  typedef typename Superclass::OutputImagePixelType  OutputImagePixelType;
  typedef typename Superclass::InputImagePixelType   InputImagePixelType;

  /** Typedef to describe the output and input image index and size types. */
  typedef typename Superclass::OutputImageIndexType  OutputImageIndexType;
  typedef typename Superclass::InputImageIndexType   InputImageIndexType;
  typedef typename Superclass::OutputImageSizeType   OutputImageSizeType;
  typedef typename Superclass::InputImageSizeType    InputImageSizeType;
  typedef typename Superclass::InputImageIndexType   IndexType;
  typedef typename Superclass::InputImageSizeType    SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck,
    (Concept::EqualityComparable< OutputImagePixelType >));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible< InputImagePixelType, OutputImagePixelType >));
  itkConceptMacro(SameDimensionCheck,
    (Concept::SameDimension< ImageDimension, OutputImageDimension >));
  itkConceptMacro(OutputOStreamWritableCheck,
    (Concept::OStreamWritable< OutputImagePixelType >));
  // End concept checking
#endif

protected:
  ZeroFluxNeumannPadImageFilter();
  ~ZeroFluxNeumannPadImageFilter() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ZeroFluxNeumannPadImageFilter);

  ZeroFluxNeumannBoundaryCondition< TInputImage, TOutputImage > m_InternalBoundaryCondition;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkZeroFluxNeumannPadImageFilter.hxx"
#endif

#endif
