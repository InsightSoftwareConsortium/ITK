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
#include "itkConstantBoundaryCondition.h"
#include "itkPeriodicBoundaryCondition.h"
#include "itkChangeInformationImageFilter.h"

namespace itk
{
/** \class FFTPadPositiveIndexImageFilter
 * \brief Pad an image to make it suitable for an FFT transformation.
 * The difference with @sa FFTPadImageFilter is that the padded image
 * has no negative indices, that can be problematic for @sa NeighborhoodIterator.
 *
 * FFT filters usually require a specific image size. The size is decomposed
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
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTPadPositiveIndexImageFilter);

  /** Standard class type alias. */
  using Self = FFTPadPositiveIndexImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using RegionType = typename InputImageType::RegionType;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  using BoundaryConditionType = ImageBoundaryCondition<TInputImage, TOutputImage>;
  using BoundaryConditionPointerType = BoundaryConditionType *;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FFTPadPositiveIndexImageFilter, ImageToImageFilter);

  using ChangeInfoFilterType = itk::ChangeInformationImageFilter<OutputImageType>;
  using FFTPadFilterType = itk::FFTPadImageFilter<InputImageType, OutputImageType>;
  itkGetConstMacro(SizeGreatestPrimeFactor, SizeValueType);
  itkSetMacro(SizeGreatestPrimeFactor, SizeValueType);
  /** Set/get the boundary condition. */
  itkGetConstMacro(BoundaryCondition, BoundaryConditionPointerType);
  virtual void
  SetBoundaryCondition(const BoundaryConditionPointerType boundaryCondition)
  {
    if (this->m_BoundaryCondition != boundaryCondition)
    {
      this->m_BoundaryCondition = boundaryCondition;
      this->m_FFTPadFilter->SetBoundaryCondition(this->m_BoundaryCondition);
      this->m_FFTPadFilter->Modified();
      this->Modified();
    }
  }
  itkGetConstMacro(HalfPadSize, SizeType);
  void
  SetBoundaryConditionToConstant(const OutputImagePixelType & boundaryValue)
  {
    using BoundaryCondition = itk::ConstantBoundaryCondition<InputImageType, OutputImageType>;
    static BoundaryCondition boundaryCondition;
    boundaryCondition.SetConstant(boundaryValue);
    this->SetBoundaryCondition(&boundaryCondition);
  }
  void
  SetBoundaryConditionToPeriodic()
  {
    using BoundaryCondition = itk::PeriodicBoundaryCondition<InputImageType, OutputImageType>;
    static BoundaryCondition boundaryCondition;
    this->SetBoundaryCondition(&boundaryCondition);
  }

protected:
  FFTPadPositiveIndexImageFilter();
  ~FFTPadPositiveIndexImageFilter() override {};
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  GenerateOutputInformation() override;

  void
  GenerateData() override;

private:
  typename FFTPadFilterType::Pointer     m_FFTPadFilter;
  typename ChangeInfoFilterType::Pointer m_ChangeInfoFilter;
  SizeValueType                          m_SizeGreatestPrimeFactor;
  BoundaryConditionPointerType           m_BoundaryCondition;
  SizeType                               m_HalfPadSize;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTPadPositiveIndexImageFilter.hxx"
#endif

#endif
