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
#ifndef itkDenseFiniteDifferenceImageFilter_h
#define itkDenseFiniteDifferenceImageFilter_h

#include "itkFiniteDifferenceImageFilter.h"
#include "itkMultiThreader.h"

namespace itk
{
/**
 * \class DenseFiniteDifferenceImageFilter
 *
 * This filter implements a layer of the finite difference solver hierarchy that
 * performs "dense" iteration, ie. iteration over all pixels in the input and
 * output at each change calculation and update step. Dense iteration is in
 * contrast to a "sparse" iteration over a subset of the pixels.  See
 * documentation for FiniteDifferenceImageFilter for an overview of the
 * iterative finite difference algorithm:
 *
 * \par
 * \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 * \par
 * The generic code for performing iterations and updates at each time
 * step is inherited from the parent class.  This class defines an update
 * buffer for \f$ \Delta \f$ and the methods CalculateChange() and
 * ApplyUpdate(). These methods are designed to automatically thread their
 * execution.  \f$ \Delta \f$ is defined as an image of identical size and type
 * as the output image.
 *
 * \par
 * As we descend through each layer in the hierarchy, we know more and more
 * about the specific application of our filter.  At this level, we
 * have committed to iteration over each pixel in an image. We take advantage
 * of that knowledge to multithread the iteration and update methods.
 *
 * \par Inputs and Outputs
 * This is an image to image filter.  The specific types of the images are not
 * fixed at this level in the hierarchy.
 *
 * \par How to use this class
 * This filter is only one layer in a branch the finite difference solver
 * hierarchy.  It does not define the function used in the CalculateChange() and
 * it does not define the stopping criteria (Halt method).  To use this class,
 * subclass it to a specific instance that supplies a function and Halt()
 * method.
 *
 * \ingroup ImageFilters
 * \sa FiniteDifferenceImageFilter
 * \ingroup ITKFiniteDifference
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT DenseFiniteDifferenceImageFilter:
  public FiniteDifferenceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef DenseFiniteDifferenceImageFilter Self;
  typedef FiniteDifferenceImageFilter<
    TInputImage, TOutputImage >                 Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(DenseFiniteDifferenceImageFilter, ImageToImageFilter);

  /** Convenient typedefs */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename Superclass::PixelType PixelType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** The container type for the update buffer. */
  typedef OutputImageType UpdateBufferType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputTimesDoubleCheck,
                   ( Concept::MultiplyOperator< PixelType, double > ) );
  itkConceptMacro( OutputAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< PixelType > ) );
  itkConceptMacro( OutputAdditiveAndAssignOperatorsCheck,
                   ( Concept::AdditiveAndAssignOperators< PixelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, PixelType > ) );
  // End concept checking
#endif

protected:
  DenseFiniteDifferenceImageFilter()
  { m_UpdateBuffer = UpdateBufferType::New(); }
  ~DenseFiniteDifferenceImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** A simple method to copy the data from the input to the output.  ( Supports
   * "read-only" image adaptors in the case where the input image type converts
   * to a different output image type. )  */
  virtual void CopyInputToOutput() ITK_OVERRIDE;

  /** This method applies changes from the m_UpdateBuffer to the output using
   * the ThreadedApplyUpdate() method and a multithreading mechanism.  "dt" is
   * the time step to use for the update of each pixel. */
  virtual void ApplyUpdate(const TimeStepType& dt) ITK_OVERRIDE;

  /** Method to allow subclasses to get direct access to the update
   * buffer */
  virtual UpdateBufferType * GetUpdateBuffer()
  { return m_UpdateBuffer; }

  /** This method populates an update buffer with changes for each pixel in the
   * output using the ThreadedCalculateChange() method and a multithreading
   * mechanism. Returns value is a time step to be used for the update. */
  virtual TimeStepType CalculateChange() ITK_OVERRIDE;

  /** This method allocates storage in m_UpdateBuffer.  It is called from
   * Superclass::GenerateData(). */
  virtual void AllocateUpdateBuffer() ITK_OVERRIDE;

  /** The type of region used for multithreading */
  typedef typename UpdateBufferType::RegionType ThreadRegionType;

  /**  Does the actual work of updating the output from the UpdateContainer over
   *  an output region supplied by the multithreading mechanism.
   *  \sa ApplyUpdate
   *  \sa ApplyUpdateThreaderCallback */
  virtual
  void ThreadedApplyUpdate(const TimeStepType& dt,
                           const ThreadRegionType & regionToProcess,
                           ThreadIdType threadId);

  /** Does the actual work of calculating change over a region supplied by
   * the multithreading mechanism.
   * \sa CalculateChange
   * \sa CalculateChangeThreaderCallback */
  virtual
  TimeStepType ThreadedCalculateChange(const ThreadRegionType & regionToProcess,
                                       ThreadIdType threadId);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DenseFiniteDifferenceImageFilter);

  /** Structure for passing information into static callback methods.  Used in
   * the subclasses' threading mechanisms. */
  struct DenseFDThreadStruct {
    DenseFiniteDifferenceImageFilter *Filter;
    TimeStepType TimeStep;
    std::vector< TimeStepType > TimeStepList;
    std::vector< bool > ValidTimeStepList;
  };

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedApplyUpdate for processing. */
  static ITK_THREAD_RETURN_TYPE ApplyUpdateThreaderCallback(void *arg);

  /** This callback method uses SplitUpdateContainer to acquire a region
   * which it then passes to ThreadedCalculateChange for processing. */
  static ITK_THREAD_RETURN_TYPE CalculateChangeThreaderCallback(void *arg);

  /** The buffer that holds the updates for an iteration of the algorithm. */
  typename UpdateBufferType::Pointer m_UpdateBuffer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDenseFiniteDifferenceImageFilter.hxx"
#endif

#endif
