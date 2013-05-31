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
#ifndef __itkGPUFiniteDifferenceImageFilter_h
#define __itkGPUFiniteDifferenceImageFilter_h

#include "itkGPUInPlaceImageFilter.h"
#include "itkGPUFiniteDifferenceFunction.h"
#include "itkFiniteDifferenceImageFilter.h"
#include "itkTimeProbe.h"

namespace itk
{
/**
 * \class GPUFiniteDifferenceImageFilter
 *
 * \brief Base class for GPU Finite Difference Image Filters.
 *
 * \ingroup ITKGPUFiniteDifference
 * \sa GPUDenseFiniteDifferenceImageFilter */
template< class TInputImage, class TOutputImage, class TParentImageFilter =
            FiniteDifferenceImageFilter< TInputImage, TOutputImage > >
class GPUFiniteDifferenceImageFilter :
  public GPUInPlaceImageFilter< TInputImage, TOutputImage, TParentImageFilter >
{
public:
  /** Standard class typedefs. */
  typedef GPUFiniteDifferenceImageFilter                                         Self;
  typedef GPUInPlaceImageFilter< TInputImage, TOutputImage, TParentImageFilter > GPUSuperclass;
  typedef TParentImageFilter                                                     CPUSuperclass;
  typedef SmartPointer< Self >                                                   Pointer;
  typedef SmartPointer< const Self >                                             ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(GPUFiniteDifferenceImageFilter, GPUInPlaceImageFilter);

  /** Input and output image types. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int, OutputImageType::ImageDimension);

  /** The pixel type of the output image will be used in computations. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef OutputPixelType                  PixelType;

  /** Extract value type in case the pixel is of vector type */
  typedef typename NumericTraits< OutputPixelType >::ValueType OutputPixelValueType;
  typedef typename NumericTraits< InputPixelType >::ValueType  InputPixelValueType;

  /** The value type of the time step.  This is distinct from PixelType
   * because PixelType may often be a vector value, while the TimeStep is
   * a scalar value. */
  typedef typename GPUFiniteDifferenceFunction< TOutputImage >::DifferenceFunctionType FiniteDifferenceFunctionType;
  //typedef typename GPUFiniteDifferenceFunction< TOutputImage >
  // FiniteDifferenceFunctionType;
  typedef typename FiniteDifferenceFunctionType::TimeStepType           TimeStepType;
  typedef typename FiniteDifferenceFunctionType::RadiusType             RadiusType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodScalesType NeighborhoodScalesType;

  /** This method returns a pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  itkGetConstReferenceObjectMacro(DifferenceFunction, FiniteDifferenceFunctionType);
  //itkGetMacro(DifferenceFunction, FiniteDifferenceFunctionType);

  /** This method sets the pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  itkSetObjectMacro(DifferenceFunction, FiniteDifferenceFunctionType);

  typedef enum { UNINITIALIZED = 0, INITIALIZED = 1 } FilterStateType;

  /** Use the image spacing information in calculations. Use this option if you
   *  want derivatives in physical space. Default is UseImageSpacingOff. */
  itkSetMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set/Get the maximum error allowed in the solution.  This may not be
      defined for all solvers and its meaning may change with the application. */
  itkSetMacro(MaximumRMSError, double);
  itkGetConstReferenceMacro(MaximumRMSError, double);

  /** Set/Get the root mean squared change of the previous iteration. May not
      be used by all solvers. */
  itkSetMacro(RMSChange, double);
  itkGetConstReferenceMacro(RMSChange, double);

  /** Set the state of the filter to INITIALIZED */
  void SetStateToInitialized()
  {
    this->SetState(INITIALIZED);
  }

  /** Set the state of the filter to UNINITIALIZED */
  void SetStateToUninitialized()
  {
    this->SetState(UNINITIALIZED);
  }

  /** Set/Get the state of the filter. */
#if !defined( CABLE_CONFIGURATION )
  itkSetMacro(State, FilterStateType);
  itkGetConstReferenceMacro(State, FilterStateType);
#endif

  /** Require the filter to be manually reinitialized (by calling
      SetStateToUninitialized() */
  itkSetMacro(ManualReinitialization, bool);
  itkGetConstReferenceMacro(ManualReinitialization, bool);
  itkBooleanMacro(ManualReinitialization);

#ifdef ITK_USE_STRICT_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( OutputPixelIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputPixelValueType > ) );
  /** End concept checking */
#endif

  /** Methods to get timers */
  itkGetConstReferenceMacro(InitTime,           TimeProbe);
  itkGetConstReferenceMacro(ComputeUpdateTime,  TimeProbe);
  itkGetConstReferenceMacro(ApplyUpdateTime,    TimeProbe);
  itkGetConstReferenceMacro(SmoothFieldTime,    TimeProbe);

protected:
  GPUFiniteDifferenceImageFilter();
  ~GPUFiniteDifferenceImageFilter();

  void PrintSelf(std::ostream & os, Indent indent) const;

  /** This method allocates a temporary update container in the subclass. */
  virtual void AllocateUpdateBuffer() = 0;

  /** This method is defined by a subclass to apply changes to the output
   * from an update buffer and a time step value "dt".
   * \param dt Time step value. */
  virtual void GPUApplyUpdate(const TimeStepType& dt) = 0;

  /** This method is defined by a subclass to populate an update buffer
   * with changes for the pixels in the output.  It returns a time
   * step value to be used for the update.
   * \returns A time step to use in updating the output with the changes
   * calculated from this method. */
  virtual TimeStepType GPUCalculateChange() = 0;

  /** This method can be defined in subclasses as needed to copy the input
   * to the output. See DenseFiniteDifferenceImageFilter for an
   * implementation. */
  virtual void CopyInputToOutput() = 0;

  /** This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm. */
  virtual void GPUGenerateData();

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
  virtual void GenerateInputRequestedRegion();

  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving.  Defined by a subclass. */
  virtual bool Halt();

  /** This method is similar to Halt(), and its default implementation in this
   * class is simply to call Halt(). However, this method takes as a parameter
   * a void pointer to the MultiThreader::ThreadInfoStruct structure. If you
   * override this method instead of overriding Halt, you will be able to get
   * the current thread ID and handle the Halt method accordingly. This is useful
   * if you are doing a lot of processing in Halt that you don't want parallelized.
   * Notice that ThreadedHalt is only called by the multithreaded filters, so you
   * still should implement Halt, just in case a non-threaded filter is used.
   */
  virtual bool ThreadedHalt( void *itkNotUsed(threadInfo) ) {
    return this->Halt();
  }

  /** This method is optionally defined by a subclass and is called before
   * the loop of iterations of calculate_change & upate. It does the global
   * initialization, i.e. in the SparseFieldLevelSetImageFilter, initialize
   * the list of layers.
   * */
  virtual void Initialize() {
  }

  /** This method is optionally defined by a subclass and is called immediately
   * prior to each iterative CalculateChange-ApplyUpdate cycle.  It can be
   * used to set global variables needed for the next iteration (ie. average
   * gradient magnitude of the image in anisotropic diffusion functions), or
   * otherwise prepare for the next iteration.
   * */
  virtual void InitializeIteration()
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
virtual TimeStepType ResolveTimeStep(const std::vector<TimeStepType >& timeStepList,
                                     const std::vector< bool >& valid) const;

  /** This method is called after the solution has been generated to allow
   * subclasses to apply some further processing to the output. */
  virtual void PostProcessOutput() {
  }

  /** Set the number of elapsed iterations of the filter. */
  itkSetMacro(ElapsedIterations, IdentifierType);

  /** The maximum number of iterations this filter will run */

  //unsigned int m_NumberOfIterations;

  /** A counter for keeping track of the number of elapsed
      iterations during filtering. */
  //unsigned int m_ElapsedIterations;

  /** Indicates whether the filter automatically resets to UNINITIALIZED state
      after completing, or whether filter must be manually reset */
  bool m_ManualReinitialization;

  double m_RMSChange;
  double m_MaximumRMSError;

  /** Timers for statistics */
  TimeProbe m_InitTime, m_ComputeUpdateTime, m_ApplyUpdateTime, m_SmoothFieldTime;

private:

  GPUFiniteDifferenceImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  /** Initialize the values of the Function coefficients. This function will
   * also take care of checking whether the image spacing should be taken into
   * account or not. */
  void InitializeFunctionCoefficients();

  /** The function that will be used in calculating updates for each pixel. */
  typename FiniteDifferenceFunctionType::Pointer m_DifferenceFunction;

  /** Control whether derivatives use spacing of the input image in
      its calculation. */
  bool m_UseImageSpacing;

  /** State that the filter is in, i.e. UNINITIALIZED or INITIALIZED */
  FilterStateType m_State;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGPUFiniteDifferenceImageFilter.hxx"
#endif

#endif
