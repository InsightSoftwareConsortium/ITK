/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkFiniteDifferenceImageFilter_h_
#define __itkFiniteDifferenceImageFilter_h_

#include "itkImageToImageFilter.h"
#include "itkFiniteDifferenceEquation.h"

namespace itk {

/**
 * \class FiniteDifferenceImageFilter
 *
 *  This class defines a high-level framework for solving partial differential
 *  equations using finite differences.  The input is an itkImage that
 *  represents the initial values and the output is an itkImage that
 *  represents the solution.
 *
 *  The high-level, iterative algorithm is implemented in this class (see the
 *  method GenerateData() ); 
 * \code
 *   WHILE NOT convergence:
 *     FOR ALL pixels i
 *        time_step = calculate_change(i)
 *        update(i, time_step)
 *  \endcode
 *
 *  The following equation describes update \f$n+1\f$ at pixel \f$i\f$ on
 *  discrete image \f$u\f$ : 
 *
 *  \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 *  \todo Write GenerateInputRequestedRegion method to ask for appropriately
 *        padded buffers to process streamed chunks
 *
 */
template <class TInputImage, class TOutputImage>
class FiniteDifferenceImageFilter  
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
 public:
  /**
   * Standard itk Self & Superclass typedefs
   */
  typedef FiniteDifferenceImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;

  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /**
   * Dimensionality of input and output data is assumed to be the same.
   */
  enum { ImageDimension = OutputImageType::ImageDimension };

  /**
   * The pixel type of the output image will be used in computations.
   */
  typedef typename TOutputImage::PixelType PixelType;

  /**
   *
   */
  typedef FiniteDifferenceEquation<TOutputImage> FiniteDifferenceEquationType;

  /**
   * The value type of the time step.  This is distinct from PixelType
   * because PixelType may often be a vector value, while the TimeStep is
   * a scalar value.
   */
  typedef typename FiniteDifferenceEquationType::TimeStepType TimeStepType;
  
  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(FiniteDifferenceImageFilter, ImageToImageFilter );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Get the number of elapsed iterations of the filter.
   */
  itkGetConstMacro(ElapsedIterations, unsigned int);
    
protected:
  FiniteDifferenceImageFilter() : m_ElapsedIterations(0) {}
  ~FiniteDifferenceImageFilter() {}
  FiniteDifferenceImageFilter(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * This method allocates a temporary update container in the subclass.
   */
  virtual void AllocateUpdateBuffer() = 0;

  /**
   * This method is defined by a subclass to apply changes to the output
   * from an update buffer and a time step value "dt".
   * \param dt Time step value.
   */
  virtual void ApplyUpdate(TimeStepType dt) = 0;
  
  /**
   * This method is defined by a subclass to populate an update buffer
   * with changes for the pixels in the output.  It returns a time
   * step value to be used for the update.
   * \returns A time step to use in updating the output with the changes
   * calculated from this method.
   */
  virtual TimeStepType CalculateChange() = 0;

  /**
   * A simple method to copy the data from the input to the output.  ( Supports
   * "read-only" image adaptors in the case where the input image type converts
   * to a different output image type. ) 
   */
  void CopyInputToOutput();
  
  /**
   * This method returns a pointer to a FiniteDifferenceEquation object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer.
   */
  itkGetConstReferenceObjectMacro(DifferenceEquation,
                                  FiniteDifferenceEquationType );

  /**
   * This method sets the pointer to a FiniteDifferenceEquation object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer.
   */
  itkSetObjectMacro(DifferenceEquation, FiniteDifferenceEquationType );

  /**
   * This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm.
   */
  virtual void GenerateData();

  /**
   * This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving.  Defined by a subclass.
   */
  virtual bool Halt() = 0;

  /**
   * This method is optionally defined by a subclass and is called immediately
   * prior to each iterative CalculateChange-ApplyUpdate cycle.  It can be
   * used to set global variables needed for the next iteration (ie. average
   * gradient magnitude of the image in anisotropic diffusion functions), or
   * otherwise prepare for the next iteration.
   *
   */
  virtual void InitializeIteration()
    { m_DifferenceEquation->InitializeIteration(); }
  
  /**
   * Virtual method for resolving a single time step from a set of time steps
   * returned from processing threads.
   * \return Time step (dt) for the iteration update based on a list
   * of time steps generated from the threaded calculated change method (one
   * for each region processed).
   *
   * \param list The set of time changes compiled from all the threaded calls
   * to ThreadedGenerateData.
   * \param valid The set of flags indicating which of "list" elements are
   *  valid
   * \param size The size of "list" and "valid"
   *
   * The default is to return the minimum value in the list.
   */
  virtual TimeStepType ResolveTimeStep(const TimeStepType* list, const bool* valid,
                                    int size);

  /**
   * Set the number of elapsed iterations of the filter.
   */
  itkSetMacro(ElapsedIterations, unsigned int);

private:
  /**
   * A counter for keeping track of the number of elapsed iterations during filtering.
   */
  unsigned int m_ElapsedIterations;

  /**
   * The function that will be used in calculating updates for each pixel.
   */
  typename FiniteDifferenceEquationType::Pointer m_DifferenceEquation;
};
  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFiniteDifferenceImageFilter.txx"
#endif

#endif

