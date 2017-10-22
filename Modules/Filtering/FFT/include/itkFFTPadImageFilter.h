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

#ifndef itkFFTPadImageFilter_h
#define itkFFTPadImageFilter_h

#include "itkPadImageFilterBase.h"
#include "itkConceptChecking.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{

/** \class FFTPadImageFilter
 * \brief Pad an image to make it suitable for an FFT transformation
 *
 * FFT filters usually requires a specific image size. The size is decomposed
 * in several prime factors, and the filter only supports prime factors up to
 * a maximum value.
 * This filter automatically finds the greatest prime factor required by the
 * available implementation and pads the input appropriately.
 *
 * This code was adapted from the Insight Journal contribution:
 *
 * "FFT Based Convolution"
 * by Gaetan Lehmann
 * https://hdl.handle.net/10380/3154
 *
 * \author Gaetan Lehmann
 *
 * \ingroup ITKFFT
 *
 * \sa FFTShiftImageFilter
 */
template<typename TInputImage, typename TOutputImage=TInputImage>
class ITK_TEMPLATE_EXPORT FFTPadImageFilter :
    public PadImageFilterBase<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef FFTPadImageFilter                             Self;
  typedef PadImageFilterBase<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                              InputImageType;
  typedef TOutputImage                             OutputImageType;
  typedef typename InputImageType::PixelType       InputImagePixelType;
  typedef typename OutputImageType::PixelType      OutputImagePixelType;
  typedef typename InputImageType::RegionType      RegionType;
  typedef typename InputImageType::IndexType       IndexType;
  typedef typename InputImageType::SizeType        SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FFTPadImageFilter, PadImageFilterBase);

  /**
   * Set/Get the greatest prime factor allowed on the size of the padded image.
   * The filter increase the size of the image to reach a size with the greatest
   * prime factor smaller or equal to the specified value. The default value is
   * 13, which is the greatest prime number for which the FFT are precomputed
   * in FFTW, and thus gives very good performance.
   * A greatest prime factor of 2 produce a size which is a power of 2, and thus
   * is suitable for vnl base fft filters.
   * A greatest prime factor of 1 or less - typically 0 - disable the extra padding.
   */
  itkGetConstMacro(SizeGreatestPrimeFactor, SizeValueType);
  itkSetMacro(SizeGreatestPrimeFactor, SizeValueType);

  /** Typedef to describe the boundary condition. */
  typedef ImageBoundaryCondition< TInputImage >           BoundaryConditionType;
  typedef BoundaryConditionType *                         BoundaryConditionPointerType;
  typedef ZeroFluxNeumannBoundaryCondition< TInputImage > DefaultBoundaryConditionType;

protected:
  FFTPadImageFilter();
  ~FFTPadImageFilter() ITK_OVERRIDE {};
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;


private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTPadImageFilter);

  SizeValueType m_SizeGreatestPrimeFactor;

  DefaultBoundaryConditionType m_DefaultBoundaryCondition;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFFTPadImageFilter.hxx"
#endif

#endif
