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

#ifndef itkGPUInPlaceImageFilter_h
#define itkGPUInPlaceImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkGPUImageToImageFilter.h"

namespace itk
{
/** \class GPUInPlaceImageFilter
 * \brief Base class for GPU filters that take an image as input and overwrite that image as the output
 *
 * This class is the base class for GPU inplace filter. The template parameter for parent class type
 * must be InPlaceImageFilter type so that the GPU superclass of this class can be correctly defined
 * (NOTE: TParentImageFilter::Superclass is used to define GPUImageToImageFilter class).
 *
 * \ingroup ITKGPUCommon
 */
template< typename TInputImage, typename TOutputImage = TInputImage, typename TParentImageFilter =
            InPlaceImageFilter< TInputImage, TOutputImage > >
class ITK_TEMPLATE_EXPORT GPUInPlaceImageFilter : public GPUImageToImageFilter< TInputImage, TOutputImage, TParentImageFilter >
{
public:
  /** Standard class typedefs. */
  typedef GPUInPlaceImageFilter                                                  Self;
  typedef GPUImageToImageFilter< TInputImage, TOutputImage, TParentImageFilter > GPUSuperclass;
  typedef TParentImageFilter                                                     CPUSuperclass;
  typedef SmartPointer< Self >                                                   Pointer;
  typedef SmartPointer< const Self >                                             ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUInPlaceImageFilter, GPUImageToImageFilter);

  /** Superclass typedefs. */
  typedef typename GPUSuperclass::OutputImageType       OutputImageType;
  typedef typename GPUSuperclass::OutputImagePointer    OutputImagePointer;
  typedef typename GPUSuperclass::OutputImageRegionType OutputImageRegionType;
  typedef typename GPUSuperclass::OutputImagePixelType  OutputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

protected:
  GPUInPlaceImageFilter();
  ~GPUInPlaceImageFilter() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** The GenerateData method normally allocates the buffers for all
   * of the outputs of a filter. Since InPlaceImageFilter's can use an
   * overwritten version of the input for its output, the output
   * buffer should not be allocated. When possible, we graft the input
   * to the filter to the output.  If an InPlaceFilter has multiple
   * outputs, then it would need to override this method to graft one
   * of its outputs and allocate the remaining. If a filter is
   * threaded (i.e. it provides an implementation of
   * ThreadedGenerateData()), this method is called automatically. If
   * an InPlaceFilter is not threaded (i.e. it provides an
   * implementation of GenerateData()), then this method (or
   * equivalent) must be called in GenerateData(). */
  virtual void AllocateOutputs() ITK_OVERRIDE;

  /** InPlaceImageFilter may transfer ownership of the input bulk data
   * to the output object.  Once the output object owns the bulk data
   * (done in AllocateOutputs()), the input object must release its
   * hold on the bulk data.  ProcessObject::ReleaseInputs() only
   * releases the input bulk data when the user has set the
   * ReleaseDataFlag.  InPlaceImageFilter::ReleaseInputs() also
   * releases the input that it has overwritten.
   *
   * \sa ProcessObject::ReleaseInputs() */
  virtual void ReleaseInputs() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUInPlaceImageFilter);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUInPlaceImageFilter.hxx"
#endif

#endif
