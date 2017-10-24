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
#ifndef itkGPUDiscreteGaussianImageFilter_h
#define itkGPUDiscreteGaussianImageFilter_h

#include "itkGPUImage.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUNeighborhoodOperatorImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"

namespace itk
{
/**
 * \class GPUDiscreteGaussianImageFilter
 * \brief Blurs an image by separable convolution with discrete gaussian kernels.
 * This filter performs Gaussian blurring by separable convolution of an image
 * and a discrete Gaussian operator (kernel). GPUNeighborhoodOperatorImageFilter
 * is used to compute 1D directional discrete Gaussian filtering for each axis.
 *
 * The variance or standard deviation (sigma) will be evaluated as pixel units
 * if SetUseImageSpacing is off (false) or as physical units if
 * SetUseImageSpacing is on (true, default). The variance can be set
 * independently in each dimension.
 *
 * When the Gaussian kernel is small, this filter tends to run faster than
 * itk::RecursiveGaussianImageFilter.
 *
 * \ingroup ITKGPUSmoothing
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT GPUDiscreteGaussianImageFilter :
  public GPUImageToImageFilter< TInputImage, TOutputImage, DiscreteGaussianImageFilter< TInputImage, TOutputImage > >
{
public:
  /** Standard class typedefs. */
  typedef GPUDiscreteGaussianImageFilter                                    Self;
  typedef DiscreteGaussianImageFilter< TInputImage, TOutputImage >          CPUSuperclass;
  typedef GPUImageToImageFilter< TInputImage, TOutputImage, CPUSuperclass > GPUSuperclass;
  typedef SmartPointer< Self >                                              Pointer;
  typedef SmartPointer< const Self >                                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUDiscreteGaussianImageFilter, GPUImageToImageFilter);

  /** Image type information. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.   */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType         OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType          InputPixelType;
  typedef typename TInputImage::InternalPixelType  InputInternalPixelType;

  /** Pixel value type for Vector pixel types **/
  typedef typename NumericTraits<InputPixelType>::ValueType  InputPixelValueType;
  typedef typename NumericTraits<OutputPixelType>::ValueType OutputPixelValueType;

  typedef OutputPixelType
                                                                         RealOutputPixelType;
  typedef GPUImage< OutputPixelType,
                    ImageDimension >                                     RealOutputImageType;
  typedef typename NumericTraits<RealOutputPixelType>::ValueType
                                                                         RealOutputPixelValueType;
  typedef GPUNeighborhoodOperatorImageFilter< InputImageType, RealOutputImageType,
                                              RealOutputPixelValueType > FirstFilterType;
  typedef GPUNeighborhoodOperatorImageFilter< RealOutputImageType, RealOutputImageType,
                                              RealOutputPixelValueType > IntermediateFilterType;
  typedef GPUNeighborhoodOperatorImageFilter< RealOutputImageType, OutputImageType,
                                              RealOutputPixelValueType > LastFilterType;
  typedef GPUNeighborhoodOperatorImageFilter< InputImageType, OutputImageType,
                                              RealOutputPixelValueType > SingleFilterType;

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

protected:
  GPUDiscreteGaussianImageFilter();
  virtual ~GPUDiscreteGaussianImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Standard GPU pipeline method. */
  void GPUGenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUDiscreteGaussianImageFilter);

  /** Intermediate 1D Gaussian filters */
  typename FirstFilterType::Pointer                       m_FirstFilter;
  typename LastFilterType::Pointer                        m_LastFilter;
  std::vector< typename IntermediateFilterType::Pointer > m_IntermediateFilters;
  typename SingleFilterType::Pointer                      m_SingleFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUDiscreteGaussianImageFilter.hxx"
#endif

#endif
