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

#ifndef itkFFTPadPositiveIndexImageFilter_h
#define itkFFTPadPositiveIndexImageFilter_h

#include "itkFFTPadImageFilter.h"
#include "itkImageBoundaryCondition.h"
#include "itkChangeInformationImageFilter.h"

namespace itk
{
/** \class FFTPadPositiveIndexImageFilter
 * \brief Pad an image to make it suitable for an FFT transformation.
 * The difference with @sa FFTPadImageFilter is that the padded image
 * has no negative indices, that can be problematic for @sa NeighborhoodIterator.
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
 * \author Pablo Hernandez-Cerdan
 *
 * \ingroup IsotropicWavelets
 *
 * \sa FFTPadImageFilter
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class FFTPadPositiveIndexImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef FFTPadPositiveIndexImageFilter                Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                         InputImageType;
  typedef TOutputImage                        OutputImageType;
  typedef typename InputImageType::PixelType  InputImagePixelType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;
  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::IndexType  IndexType;
  typedef typename InputImageType::SizeType   SizeType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  typedef ImageBoundaryCondition<TInputImage, TOutputImage> BoundaryConditionType;
  typedef BoundaryConditionType *                           BoundaryConditionPointerType;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FFTPadPositiveIndexImageFilter, ImageToImageFilter);

  typedef itk::ChangeInformationImageFilter<OutputImageType>      ChangeInfoFilterType;
  typedef itk::FFTPadImageFilter<InputImageType, OutputImageType> FFTPadFilterType;
  itkGetConstMacro(SizeGreatestPrimeFactor, SizeValueType);
  itkSetMacro(SizeGreatestPrimeFactor, SizeValueType);
  /** Set/get the boundary condition. */
  itkSetMacro(BoundaryCondition, BoundaryConditionPointerType);
  itkGetConstMacro(BoundaryCondition, BoundaryConditionPointerType);

protected:
  FFTPadPositiveIndexImageFilter();
  ~FFTPadPositiveIndexImageFilter() {};
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

  virtual void
  GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTPadPositiveIndexImageFilter);
  typename FFTPadFilterType::Pointer     m_FFTPadFilter;
  typename ChangeInfoFilterType::Pointer m_ChangeInfoFilter;
  SizeValueType                          m_SizeGreatestPrimeFactor;
  BoundaryConditionPointerType           m_BoundaryCondition;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTPadPositiveIndexImageFilter.hxx"
#endif

#endif
