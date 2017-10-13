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
#ifndef itkSampleToSubsampleFilter_h
#define itkSampleToSubsampleFilter_h

#include "itkSubsample.h"
#include "itkProcessObject.h"

namespace itk
{
namespace Statistics
{
/** \class SampleToSubsampleFilter
 * \brief Base class of filters intended to select subsamples from samples.
 *
 * This filter will take as input a Sample and produce as output a Subsample
 * that derives from the original sample, and that refers to it.
 *
 * This is an Abstract class that can not be instantiated. There are multiple
 * filters that derive from this class and provide specific implementations of
 * subsampling methods.
 *
 * \sa Sample, Subsample
 *
 * \sa NeighborhoodSampler
 *
 * \ingroup ITKStatistics
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT SampleToSubsampleFilter:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef SampleToSubsampleFilter    Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard macros */
  itkTypeMacro(SampleToSubsampleFilter, ProcessObject);

  /** Typedefs for Measurement vector, measurement, Instance Identifier,
   * frequency, size, size element value from the template argument TSample */
  typedef TSample                                    SampleType;
  typedef typename SampleType::MeasurementVectorType MeasurementVectorType;
  typedef typename SampleType::MeasurementType       MeasurementType;
  typedef typename SampleType::InstanceIdentifier    InstanceIdentifier;

  /** Declare the output type */
  typedef Subsample< SampleType > SubsampleType;
  typedef SubsampleType           OutputType;

  /** Set/Get the input sample */
  using Superclass::SetInput;
  virtual void SetInput(const SampleType *sample);

  virtual const SampleType * GetInput() const;

  /** Get the output subsample */
  const OutputType  * GetOutput() const;

protected:
  SampleToSubsampleFilter();
  virtual ~SampleToSubsampleFilter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Make a DataObject of the correct type to used as the specified
   * output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.
   * \sa ProcessObject
   */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SampleToSubsampleFilter);
};                                       // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleToSubsampleFilter.hxx"
#endif

#endif
