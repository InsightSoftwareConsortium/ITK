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
#ifndef itkConvolutionImageFilterBase_h
#define itkConvolutionImageFilterBase_h

#include "itkImageToImageFilter.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk
{
/** \class ConvolutionImageFilterBase
 * \brief Abstract base class for the convolution image filters.
 *
 * \ingroup ITKConvolution
 * \sa ConvolutionImageFilter FFTConvolutionImageFilter
 */
template< typename TInputImage,
          typename TKernelImage = TInputImage,
          typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT ConvolutionImageFilterBase :
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  typedef ConvolutionImageFilterBase                      Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(ConvolutionImageFilterBase, ImageToImageFilter);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef TInputImage                           InputImageType;
  typedef TOutputImage                          OutputImageType;
  typedef TKernelImage                          KernelImageType;
  typedef typename InputImageType::PixelType    InputPixelType;
  typedef typename OutputImageType::PixelType   OutputPixelType;
  typedef typename KernelImageType::PixelType   KernelPixelType;
  typedef typename InputImageType::IndexType    InputIndexType;
  typedef typename OutputImageType::IndexType   OutputIndexType;
  typedef typename KernelImageType::IndexType   KernelIndexType;
  typedef typename InputImageType::SizeType     InputSizeType;
  typedef typename OutputImageType::SizeType    OutputSizeType;
  typedef typename KernelImageType::SizeType    KernelSizeType;
  typedef typename InputSizeType::SizeValueType SizeValueType;
  typedef typename InputImageType::RegionType   InputRegionType;
  typedef typename OutputImageType::RegionType  OutputRegionType;
  typedef typename KernelImageType::RegionType  KernelRegionType;

  /** Typedef to describe the boundary condition. */
  typedef ImageBoundaryCondition< TInputImage >           BoundaryConditionType;
  typedef BoundaryConditionType *                         BoundaryConditionPointerType;
  typedef ZeroFluxNeumannBoundaryCondition< TInputImage > DefaultBoundaryConditionType;

  /** Set/get the boundary condition. */
  itkSetMacro(BoundaryCondition, BoundaryConditionPointerType);
  itkGetConstMacro(BoundaryCondition, BoundaryConditionPointerType);

  /** Set/get the image kernel. */
  itkSetInputMacro(KernelImage, KernelImageType);
  itkGetInputMacro(KernelImage, KernelImageType);

  /** Normalize the output image by the sum of the kernel
   * components. Defaults to off. */
  itkSetMacro(Normalize, bool);
  itkGetConstMacro(Normalize, bool);
  itkBooleanMacro(Normalize);

  typedef enum
  {
    SAME = 0,
    VALID
  } OutputRegionModeType;

  /** Sets the output region mode. If set to SAME, the output region
   * will be the same as the input region, and regions of the image
   * near the boundaries will contain contributions from outside the
   * input image as determined by the boundary condition set in
   * SetBoundaryCondition(). If set to VALID, the output region
   * consists of pixels computed only from pixels in the input image
   * (no extrapolated contributions from the boundary condition are
   * needed). The output is therefore smaller than the input
   * region. Default output region mode is SAME. */
  itkSetEnumMacro(OutputRegionMode, OutputRegionModeType);
  itkGetEnumMacro(OutputRegionMode, OutputRegionModeType);
  virtual void SetOutputRegionModeToSame();
  virtual void SetOutputRegionModeToValid();

protected:
  ConvolutionImageFilterBase();
  ~ConvolutionImageFilterBase() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** The largest possible output region may differ from the largest
   * possible input region. */
  void GenerateOutputInformation() ITK_OVERRIDE;

  /** Get the valid region of the convolution. */
  OutputRegionType GetValidRegion() const;

  /** Default superclass implementation ensures that input images
   * occupy same physical space. This is not needed for this filter. */
  virtual void VerifyInputInformation() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConvolutionImageFilterBase);

  bool m_Normalize;

  DefaultBoundaryConditionType m_DefaultBoundaryCondition;
  BoundaryConditionPointerType m_BoundaryCondition;

  OutputRegionModeType m_OutputRegionMode;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvolutionImageFilterBase.hxx"
#endif

#endif
