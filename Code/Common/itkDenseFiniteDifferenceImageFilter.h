/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFiniteDifferenceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkDenseFiniteDifferenceImageFilter_h_
#define __itkDenseFiniteDifferenceImageFilter_h_

#include "itkFiniteDifferenceImageFilter.h"
#include "itkMultiThreader.h"

namespace itk {

/**
 * \class DenseFiniteDifferenceImageFilter
 *
 *  This layer of the finite diffence solver hierarchy implements
 *  the iterative algorithm for "dense" iteration, ie. iteration
 *  over all pixels in the input and output at each change calculation and
 *  update step.  This is in contrast to a "sparse" iteration over a subset of
 *  the pixels.
 *
 *  The virtual methods CalculateChange() and ApplyUpdate() specific to dense
 *  iterations are defined in this object.  This class also implements
 *  threading of the calculations and the updates.
 *
 */
template <class TInputImage, class TOutputImage>
class DenseFiniteDifferenceImageFilter  
  : public FiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard itk Self & Superclass typedefs
   */
  typedef DenseFiniteDifferenceImageFilter Self;
  typedef FiniteDifferenceImageFilter<TInputImage, TOutputImage> Superclass;

  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FiniteDifferenceEquationType
   FiniteDifferenceEquationType;

  /**
   * Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass.
   */
  enum { ImageDimension = Superclass::ImageDimension };

  /**
   * The pixel type of the output image will be used in computations.
   * Inherited from the superclass.
   */
  typedef typename Superclass::PixelType PixelType;

  /**
   * The value type of a time step.  Inherited from the superclass.
   */
  typedef typename Superclass::TimeStepType TimeStepType;

  /**
   * The container type for the update buffer.
   */
  typedef OutputImageType UpdateBufferType;
  
  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(DenseFiniteDifferenceImageFilter, ImageToImageFilter );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  DenseFiniteDifferenceImageFilter()
  { m_UpdateBuffer = UpdateBufferType::New(); }
  ~DenseFiniteDifferenceImageFilter() {}
  DenseFiniteDifferenceImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  /**
   * Structure for passing information into static callback methods.  Used in
   * the subclasses' threading mechanisms.
   */
  struct DenseFDThreadStruct
  {
    DenseFiniteDifferenceImageFilter *Filter;
    TimeStepType TimeStep;
    TimeStepType *TimeStepList;
    bool *ValidTimeStepList;
  };
  
  /**
   * The type of region used for multithreading
   */
  typedef typename UpdateBufferType::RegionType ThreadRegionType;

  /**
   * This method allocates storage in m_UpdateBuffer.  It is called from
   * Superclass::GenerateData().
   */
  virtual void AllocateUpdateBuffer();
  
  /**
   * This method applies changes from the m_UpdateBuffer to the output using
   * the ThreadedAPplyUpdate() method and a multithreading mechanism.  "dt" is
   * the time step to use for the update of each pixel.
   */
  virtual void ApplyUpdate(TimeStepType dt);

  /**
   * This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedApplyUpdate for processing.
   */
  static ITK_THREAD_RETURN_TYPE ApplyUpdateThreaderCallback( void *arg );
  
  /**
   * This method populates an update buffer with changes for each pixel in the
   * output using the ThreadedCalculateChange() method and a multithreading
   * mechanism. Returns value is a time step to be used for the update.
   */
  virtual TimeStepType CalculateChange();

  /**
   * This callback method uses SplitUpdateContainer to acquire a region
   * which it then passes to ThreadedCalculateChange for processing.
   */
  static ITK_THREAD_RETURN_TYPE CalculateChangeThreaderCallback( void *arg );
  
  /**
   * Split the UpdateBuffer into "num" pieces, returning region "i" as
   * "splitRegion". This method is called "num" times to return non-overlapping
   * regions. The method returns the number of pieces that the UpdateBuffer
   * can be split into by the routine. i.e. return value is less than or equal
   * to "num".
   * \sa ImageSource
   */
  //  virtual
  //  int SplitUpdateContainer(int i, int num, ThreadRegionType& splitRegion);

  /**
   *  Does the actual work of updating the output from the UpdateContainer over
   *  an output region supplied by the multithreading mechanism.
   *  \sa ApplyUpdate
   *  \sa ApplyUpdateThreaderCallback
   */ 
  virtual
  void ThreadedApplyUpdate(TimeStepType dt,
                           const ThreadRegionType &regionToProcess,
                           int threadId);
  // FOR ALL: iterator(output, splitRegion), iterator(update, splitRegion)

  /**
   * Does the actual work of calculating change over a region supplied by
   * the multithreading mechanism.
   * \sa CalculateChange
   * \sa CalculateChangeThreaderCallback
   */
  virtual
  TimeStepType ThreadedCalculateChange(const ThreadRegionType &regionToProcess,
                                       int threadId);
  // FOR ALL : iterator(input, splitRegion), iterator(update, splitRegion)

  /**
   * The buffer that holds the updates for an iteration of the algorithm.
   */
  typename UpdateBufferType::Pointer m_UpdateBuffer;
};
  

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDenseFiniteDifferenceImageFilter.txx"
#endif

#endif
