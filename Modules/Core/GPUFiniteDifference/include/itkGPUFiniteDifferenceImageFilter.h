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
#ifndef itkGPUFiniteDifferenceImageFilter_h
#define itkGPUFiniteDifferenceImageFilter_h

#include "itkGPUInPlaceImageFilter.h"
#include "itkGPUFiniteDifferenceFunction.h"
#include "itkFiniteDifferenceImageFilter.h"
#include "itkTimeProbe.h"
#include "itkGPUFiniteDifferenceFilterEnum.h"

namespace itk
{

/**
 * \class GPUFiniteDifferenceImageFilter
 *
 * \brief Base class for GPU Finite Difference Image Filters.
 *
 * \ingroup ITKGPUFiniteDifference
 * \sa GPUDenseFiniteDifferenceImageFilter */
template <typename TInputImage,
          typename TOutputImage,
          typename TParentImageFilter = FiniteDifferenceImageFilter<TInputImage, TOutputImage>>
class ITK_TEMPLATE_EXPORT GPUFiniteDifferenceImageFilter
  : public GPUInPlaceImageFilter<TInputImage, TOutputImage, TParentImageFilter>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUFiniteDifferenceImageFilter);

  /** Standard class type aliases. */
  using Self = GPUFiniteDifferenceImageFilter;
  using GPUSuperclass = GPUInPlaceImageFilter<TInputImage, TOutputImage, TParentImageFilter>;
  using CPUSuperclass = TParentImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUFiniteDifferenceImageFilter, GPUInPlaceImageFilter);

  /** Input and output image types. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = OutputImageType::ImageDimension;

  /** The pixel type of the output image will be used in computations. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using PixelType = OutputPixelType;

  /** Extract value type in case the pixel is of vector type */
  using OutputPixelValueType = typename NumericTraits<OutputPixelType>::ValueType;
  using InputPixelValueType = typename NumericTraits<InputPixelType>::ValueType;

  /** The value type of the time step.  This is distinct from PixelType
   * because PixelType may often be a vector value, while the TimeStep is
   * a scalar value. */
  using FiniteDifferenceFunctionType = typename GPUFiniteDifferenceFunction<TOutputImage>::DifferenceFunctionType;
  using TimeStepType = typename FiniteDifferenceFunctionType::TimeStepType;
  using RadiusType = typename FiniteDifferenceFunctionType::RadiusType;
  using NeighborhoodScalesType = typename FiniteDifferenceFunctionType::NeighborhoodScalesType;

  /** This method returns a pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  const typename FiniteDifferenceFunctionType::Pointer &
  GetDifferenceFunction() const override
  {
    return this->m_DifferenceFunction;
  }

  /** This method sets the pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  void
  SetDifferenceFunction(FiniteDifferenceFunctionType * differenceFunction) override
  {
    itkDebugMacro("setting m_DifferenceFunction to " << differenceFunction);
    if (this->m_DifferenceFunction != differenceFunction)
    {
      this->m_DifferenceFunction = differenceFunction;
      this->Modified();
    }
  }

#if !defined(ITK_LEGACY_REMOVE)
  /** Enables backwards compatibility for enum values */
  using FilterStateType = GPUFiniteDifferenceFilterEnum;
  // We need to expose the enum values at the class level for backwards compatibility
  static constexpr GPUFiniteDifferenceFilterEnum UNINITIALIZED = GPUFiniteDifferenceFilterEnum::UNINITIALIZED;
  static constexpr GPUFiniteDifferenceFilterEnum INITIALIZED = GPUFiniteDifferenceFilterEnum::INITIALIZED;
#endif

  /** Set the state of the filter to INITIALIZED */
  void
  SetStateToInitialized()
  {
    this->SetState(GPUFiniteDifferenceFilterEnum::INITIALIZED);
  }

  /** Set the state of the filter to UNINITIALIZED */
  void
  SetStateToUninitialized()
  {
    this->SetState(GPUFiniteDifferenceFilterEnum::UNINITIALIZED);
  }

  /** Set/Get the state of the filter. */
#if !defined(ITK_WRAPPING_PARSER)
  itkSetMacro(State, GPUFiniteDifferenceFilterEnum);
  itkGetConstReferenceMacro(State, GPUFiniteDifferenceFilterEnum);
#endif

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputPixelIsFloatingPointCheck, (Concept::IsFloatingPoint<OutputPixelValueType>));
  // End concept checking
#endif

  /** Methods to get timers */
  itkGetConstReferenceMacro(InitTime, TimeProbe);
  itkGetConstReferenceMacro(ComputeUpdateTime, TimeProbe);
  itkGetConstReferenceMacro(ApplyUpdateTime, TimeProbe);
  itkGetConstReferenceMacro(SmoothFieldTime, TimeProbe);

protected:
  GPUFiniteDifferenceImageFilter();
  ~GPUFiniteDifferenceImageFilter() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** This method allocates a temporary update container in the subclass. */
  void
  AllocateUpdateBuffer() override = 0;

  /** This method is defined by a subclass to apply changes to the output
   * from an update buffer and a time step value "dt".
   * \param dt Time step value. */
  virtual void
  GPUApplyUpdate(const TimeStepType & dt) = 0;

  /** This method is defined by a subclass to populate an update buffer
   * with changes for the pixels in the output.  It returns a time
   * step value to be used for the update.
   * \returns A time step to use in updating the output with the changes
   * calculated from this method. */
  virtual TimeStepType
  GPUCalculateChange() = 0;

  /** This method can be defined in subclasses as needed to copy the input
   * to the output. See DenseFiniteDifferenceImageFilter for an
   * implementation. */
  void
  CopyInputToOutput() override = 0;

  /** This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm. */
  void
  GPUGenerateData() override;

  /** FiniteDifferenceImageFilter needs a larger input requested region than
   * the output requested region.  As such, we need to provide
   * an implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   *
   * \par
   * The filter will ask for a padded region to perform its neighborhood
   * calculations.  If no such region is available, the boundaries will be
   * handled as described in the FiniteDifferenceFunction defined by the
   * subclass.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving.  Defined by a subclass. */
  bool
  Halt() override;

  /** This method is similar to Halt(), and its default implementation in this
   * class is simply to call Halt(). However, this method takes as a parameter
   * a void pointer to the MultiThreaderBase::WorkUnitInfo structure. If you
   * override this method instead of overriding Halt, you will be able to get
   * the current thread ID and handle the Halt method accordingly. This is useful
   * if you are doing a lot of processing in Halt that you don't want parallelized.
   * Notice that ThreadedHalt is only called by the multithreaded filters, so you
   * still should implement Halt, just in case a non-threaded filter is used.
   */
  bool
  ThreadedHalt(void * itkNotUsed(threadInfo)) override
  {
    return this->Halt();
  }

  /** This method is optionally defined by a subclass and is called before
   * the loop of iterations of calculate_change & update. It does the global
   * initialization, i.e. in the SparseFieldLevelSetImageFilter, initialize
   * the list of layers.
   */
  void
  Initialize() override
  {}

  /** This method is optionally defined by a subclass and is called immediately
   * prior to each iterative CalculateChange-ApplyUpdate cycle.  It can be
   * used to set global variables needed for the next iteration (ie. average
   * gradient magnitude of the image in anisotropic diffusion functions), or
   * otherwise prepare for the next iteration.
   */
  void
  InitializeIteration() override
  {
    m_DifferenceFunction->InitializeIteration();
  }

  /** Virtual method for resolving a single time step from a set of time steps
   * returned from processing threads.
   * \return Time step (dt) for the iteration update based on a list
   * of time steps generated from the threaded calculated change method (one
   * for each region processed).
   *
   * \param timeStepList The set of time changes compiled from all the threaded calls
   * to ThreadedGenerateData.
   * \param valid The set of flags indicating which of "list" elements are
   *  valid
   *
   * The default is to return the minimum value in the list. */
  TimeStepType
  ResolveTimeStep(const std::vector<TimeStepType> & timeStepList, const std::vector<bool> & valid) const override;

  /** This method is called after the solution has been generated to allow
   * subclasses to apply some further processing to the output. */
  void
  PostProcessOutput() override
  {}

  /** The maximum number of iterations this filter will run */

  // unsigned int m_NumberOfIterations;

  /** A counter for keeping track of the number of elapsed
      iterations during filtering. */
  // unsigned int m_ElapsedIterations;

  /** Indicates whether the filter automatically resets to UNINITIALIZED state
      after completing, or whether filter must be manually reset */
  bool m_ManualReinitialization;

  double m_RMSChange;
  double m_MaximumRMSError;

  /** Timers for statistics */
  TimeProbe m_InitTime, m_ComputeUpdateTime, m_ApplyUpdateTime, m_SmoothFieldTime;

private:
  /** Initialize the values of the Function coefficients. This function will
   * also take care of checking whether the image spacing should be taken into
   * account or not. */
  void
  InitializeFunctionCoefficients();

  /** The function that will be used in calculating updates for each pixel. */
  typename FiniteDifferenceFunctionType::Pointer m_DifferenceFunction;

  /** Control whether derivatives use spacing of the input image in
      its calculation. */
  bool m_UseImageSpacing;

  /** State that the filter is in, i.e. UNINITIALIZED or INITIALIZED */
  GPUFiniteDifferenceFilterEnum m_State;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUFiniteDifferenceImageFilter.hxx"
#endif

#endif
