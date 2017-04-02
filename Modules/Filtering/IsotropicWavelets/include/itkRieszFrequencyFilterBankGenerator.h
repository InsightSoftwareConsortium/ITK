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
#ifndef itkRieszFrequencyFilterBankGenerator_h
#define itkRieszFrequencyFilterBankGenerator_h

#include <itkImageRegionIterator.h>
#include <itkGenerateImageSource.h>
#include <complex>
#include "itkRieszFrequencyFunction.h"
#include <itkFrequencyImageRegionIteratorWithIndex.h>

namespace itk
{
/** \class RieszFrequencyFilterBankGenerator
 * Generate a filter bank of M components.
 * M = p(N,d); where N = Order of the RieszTransform, and d = ImageDimension.
 * M := p(N,d) = \frac{(N+d-1)!}{(d-1)! N!}
 *
 * TODO: OLD DELETE ME
 * \brief Generate filter bank of RieszFrequencyFunction.
 * RieszFrequencyFunction returns a complex value,
 * but the output of this generator is real , representing the imaginary part
 * of that complex number (the real part of a Riesz Transform is zero).
 * It is conceptually equivalent to a ImageFilter, but because it is usually used along Wavelets, the
 * FilterBankGenerator interface has been chosen. Check RieszFrecuencyFunction for the spatial function implementation.
 *
 * The output is one image per input ImageDimension, corresponding to each direction.
 * For example: Rx = GetOutput(0), Ry = GetOutput(1), Rz = GetOutput(2) for 3D.
 *
 * \sa RieszFrequencyFunction
 *
 * \ingroup IsotropicWavelets
 */
template <typename TOutputImage,
          typename TRieszFunction = itk::RieszFrequencyFunction<std::complex<double>, TOutputImage::ImageDimension>,
          typename TFrequencyRegionIterator = FrequencyImageRegionIteratorWithIndex<TOutputImage>>
class RieszFrequencyFilterBankGenerator : public itk::GenerateImageSource<TOutputImage>
{
public:
  /** Standard typedefs */
  typedef RieszFrequencyFilterBankGenerator      Self;
  typedef itk::GenerateImageSource<TOutputImage> Superclass;
  typedef itk::SmartPointer<Self>                Pointer;
  typedef itk::SmartPointer<const Self>          ConstPointer;

  /** Type macro */
  itkNewMacro(Self);

  /** Creation through object factory macro */
  itkTypeMacro(RieszFrequencyFilterBankGenerator, GenerateImageSourceFilter);

  /** Inherit types from Superclass. */
  typedef typename Superclass::OutputImageType    OutputImageType;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  /** Basic typedefs */
  typedef TFrequencyRegionIterator             OutputRegionIterator;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  /** RieszFunction types */
  typedef TRieszFunction                                RieszFunctionType;
  typedef typename RieszFunctionType::FunctionValueType FunctionValueType;

  /** Dimension */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Get Outputs *****/
  /** Return vector of images from all directions */
  std::vector<OutputImagePointer>
  GetOutputs();

  // #ifdef ITK_USE_CONCEPT_CHECKING
  //   itkConceptMacro( OutputPixelTypeIsFloatCheck,
  //                    ( Concept::IsFloatingPoint< typename OutputImageType::PixelType > ) );
  // #endif

protected:
  RieszFrequencyFilterBankGenerator();
  virtual ~RieszFrequencyFilterBankGenerator() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate data */
  virtual void
  GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RieszFrequencyFilterBankGenerator);
}; // end of class
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszFrequencyFilterBankGenerator.hxx"
#endif

#endif
