/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkGPUDenseFiniteDifferenceImageFilter_h
#define itkGPUDenseFiniteDifferenceImageFilter_h

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkGPUFiniteDifferenceImageFilter.h"

namespace itk
{
/** Create a helper GPU Kernel class for GPUDenseFiniteDifferenceImageFilter */
itkGPUKernelClassMacro(GPUDenseFiniteDifferenceImageFilterKernel);

/**
 * \class GPUDenseFiniteDifferenceImageFilter
 * This is the GPU version of DenseFiniteDifferenceImageFilter class.
 * Currently only single-threaded, single GPU version is implemented.
 * See documentation for FiniteDifferenceImageFilter for an overview
 * of the iterative finite difference algorithm:
 *
 * \par
 * \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 * \par
 * This class defines an update buffer for \f$ \Delta \f$ and the methods
 * GPUCalculateChange() and GPUApplyUpdate(), which are GPU version of
 * CalculateChange() and ApplyUpdate().
 *
 * Use m_UpdateBuffer defined in CPU superclass (DenseFiniteDifferenceImageFilter).
 *
 * \par How to use this class
 * This filter can be used as a base class for GPU implementation of
 * DenseFiniteDifferenceImageFilter.
 *
 * \ingroup ITKGPUFiniteDifference
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TParentImageFilter = DenseFiniteDifferenceImageFilter<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT GPUDenseFiniteDifferenceImageFilter
  : public GPUFiniteDifferenceImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUDenseFiniteDifferenceImageFilter);

  /** Standard class type aliases */
  using Self = GPUDenseFiniteDifferenceImageFilter;
  using GPUSuperclass = GPUFiniteDifferenceImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using CPUSuperclass = TParentImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUDenseFiniteDifferenceImageFilter, GPUFiniteDifferenceImageFilter);

  /** Convenient type alias */
  using InputImageType = typename GPUSuperclass::InputImageType;
  using OutputImageType = typename GPUSuperclass::OutputImageType;
  using FiniteDifferenceFunctionType = typename GPUSuperclass::FiniteDifferenceFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = GPUSuperclass::ImageDimension;

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  using PixelType = typename GPUSuperclass::PixelType;

  /** The value type of a time step.  Inherited from the superclass. */
  using TimeStepType = typename GPUSuperclass::TimeStepType;

  /** The container type for the update buffer. */
  using UpdateBufferType = OutputImageType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputTimesDoubleCheck, (Concept::MultiplyOperator<PixelType, double>));
  itkConceptMacro(OutputAdditiveOperatorsCheck, (Concept::AdditiveOperators<PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck, (Concept::Convertible<typename TInputImage::PixelType, PixelType>));
  // End concept checking
#endif

  /** Get OpenCL Kernel source as a string, creates a GetOpenCLSource method */
  itkGetOpenCLSourceFromKernelMacro(GPUDenseFiniteDifferenceImageFilterKernel);

protected:
  GPUDenseFiniteDifferenceImageFilter();
  ~GPUDenseFiniteDifferenceImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method applies changes from the m_UpdateBuffer to the output using
   * the GPU.  "dt" is the time step to use for the update of each pixel. */
  void
  ApplyUpdate(const TimeStepType & dt) override;

  void
  GPUApplyUpdate(const TimeStepType & dt) override;

  /** This method populates an update buffer with changes for each pixel in the
   * output using the GPU. Returns value is a time step to be used for the update. */
  TimeStepType
  GPUCalculateChange() override;

  /** A simple method to copy the data from the input to the output.  ( Supports
   * "read-only" image adaptors in the case where the input image type converts
   * to a different output image type. )  */
  void
  CopyInputToOutput() override;

  /** Method to allow subclasses to get direct access to the update
   * buffer */
  UpdateBufferType *
  GetUpdateBuffer() override
  {
    return CPUSuperclass::GetUpdateBuffer();
  }

  /** This method allocates storage in m_UpdateBuffer.  It is called from
   * Superclass::GenerateData(). */
  void
  AllocateUpdateBuffer() override;

  /* GPU kernel handle for GPUApplyUpdate */
  int m_ApplyUpdateGPUKernelHandle;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUDenseFiniteDifferenceImageFilter.hxx"
#endif

#endif
