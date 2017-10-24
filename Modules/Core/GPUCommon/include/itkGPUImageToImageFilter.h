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
#ifndef itkGPUImageToImageFilter_h
#define itkGPUImageToImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkGPUKernelManager.h"

namespace itk
{
/** \class GPUImageToImageFilter
 *
 * \brief class to abstract the behaviour of the GPU filters.
 *
 * GPUImageToImageFilter is the GPU version of ImageToImageFilter.
 * This class can accept both CPU and GPU image as input and output,
 * and apply filter accordingly. If GPU is available for use, then
 * GPUGenerateData() is called. Otherwise, GenerateData() in the
 * parent class (i.e., ImageToImageFilter) will be called.
 *
 * \ingroup ITKGPUCommon
 */
template< typename TInputImage, typename TOutputImage, typename TParentImageFilter =
            ImageToImageFilter< TInputImage, TOutputImage > >
class ITK_TEMPLATE_EXPORT GPUImageToImageFilter : public TParentImageFilter
{
public:
  /** Standard class typedefs. */
  typedef GPUImageToImageFilter      Self;
  typedef TParentImageFilter         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUImageToImageFilter, TParentImageFilter);

  /** Superclass typedefs. */
  typedef typename Superclass::DataObjectIdentifierType DataObjectIdentifierType;
  typedef typename Superclass::OutputImageRegionType    OutputImageRegionType;
  typedef typename Superclass::OutputImagePixelType     OutputImagePixelType;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  // macro to set if GPU is used
  itkSetMacro(GPUEnabled, bool);
  itkGetConstMacro(GPUEnabled, bool);
  itkBooleanMacro(GPUEnabled);

  void GenerateData() ITK_OVERRIDE;
  virtual void GraftOutput(typename itk::GPUTraits< TOutputImage >::Type *output);
  virtual void GraftOutput(const DataObjectIdentifierType & key, typename itk::GPUTraits< TOutputImage >::Type *output);

protected:
  virtual void GraftOutput(DataObject *output) ITK_OVERRIDE;
  virtual void GraftOutput(const DataObjectIdentifierType & key, DataObject *output) ITK_OVERRIDE;
  GPUImageToImageFilter();
  ~GPUImageToImageFilter() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GPUGenerateData() {
  }

  // GPU kernel manager
  typename GPUKernelManager::Pointer m_GPUKernelManager;

  // GPU kernel handle - kernel should be defined in specific filter (not in the
  // base class)
  //int m_KernelHandle;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUImageToImageFilter);

  bool m_GPUEnabled;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUImageToImageFilter.hxx"
#endif

#endif
