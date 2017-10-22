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
#ifndef itkProjectedIterativeDeconvolutionImageFilter_h
#define itkProjectedIterativeDeconvolutionImageFilter_h

#include "itkIterativeDeconvolutionImageFilter.h"

#include "itkThresholdImageFilter.h"

namespace itk
{
/** \class ProjectedIterativeDeconvolutionImageFilter
 * \brief Mix-in class that adds a projection step after each
 * iteration.
 *
 * This filter follows the mix-in design pattern. When templated over
 * an IterativeDeconvolutionImageFilter, this class will inherit the
 * members of that filter, and it will override the definition of
 * Iteration() to first call the superclass's Iteration() method
 * followed by projecting all negative voxel values of each
 * intermediate estimate image to 0.
 *
 * This code was adapted from the Insight Journal contribution:
 *
 * \author Cory Quammen, The University of North Carolina at Chapel Hill
 *
 * \ingroup ITKDeconvolution
 */
template< typename TSuperclass >
class ITK_TEMPLATE_EXPORT ProjectedIterativeDeconvolutionImageFilter : public TSuperclass
{
public:
  /** Standard typedefs. */
  typedef ProjectedIterativeDeconvolutionImageFilter Self;
  typedef TSuperclass                                Superclass;
  typedef SmartPointer< Self >                       Pointer;
  typedef SmartPointer< const Self >                 ConstPointer;

  /** Other useful typedefs. */
  typedef typename Superclass::InputImageType        InputImageType;
  typedef typename Superclass::KernelImageType       KernelImageType;
  typedef typename Superclass::OutputImageType       OutputImageType;

  /** Internal types used by the FFT filters. */
  typedef typename Superclass::InternalImageType               InternalImageType;
  typedef typename Superclass::InternalImagePointerType        InternalImagePointerType;
  typedef typename Superclass::InternalComplexType             InternalComplexType;
  typedef typename Superclass::InternalComplexImageType        InternalComplexImageType;
  typedef typename Superclass::InternalComplexImagePointerType InternalComplexImagePointerType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ProjectedIterativeDeconvolutionImageFilter,
               IterativeDeconvolutionImageFilter);

protected:
  ProjectedIterativeDeconvolutionImageFilter();
  virtual ~ProjectedIterativeDeconvolutionImageFilter() ITK_OVERRIDE;

  virtual void Initialize(ProgressAccumulator * progress,
                          float progressWeight,
                          float iterationProgressWeight) ITK_OVERRIDE;

  virtual void Iteration(ProgressAccumulator * progress,
                         float iterationProgressWeight) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ProjectedIterativeDeconvolutionImageFilter);

  typedef ThresholdImageFilter< InternalImageType > ProjectionFilterType;

  typename ProjectionFilterType::Pointer m_ProjectionFilter;
};
} // end namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProjectedIterativeDeconvolutionImageFilter.hxx"
#endif

#endif
