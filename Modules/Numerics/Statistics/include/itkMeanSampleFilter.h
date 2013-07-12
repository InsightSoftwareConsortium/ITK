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
#ifndef __itkMeanSampleFilter_h
#define __itkMeanSampleFilter_h

#include "itkProcessObject.h"
#include "itkArray.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class MeanSampleFilter
 * \brief Given a sample, this filter computes the sample mean
 *
 * The sample is plugged in using SetSample method. Then invoke
 * update() method to compute the sample mean.
 *
 * The sample mean is computed as follows
 * \f$ = \frac{1}{n}\sum^{n}_{i=1}x_{i}\f$ where \f$n\f$ is the
 * number of measurement vectors in the target
 *
 * Recent API changes:
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained from the input sample.
 * Please use the function GetMeasurementVectorSize() to obtain the length.
 * \ingroup ITKStatistics
 */

template< class TSample >
class MeanSampleFilter:public ProcessObject
{
public:
  /**Standard class typedefs. */
  typedef MeanSampleFilter           Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef TSample                    SampleType;

  /**Standard Macros */
  itkTypeMacro(MeanSampleFilter, ProcessObject);
  itkNewMacro(Self);

  /** Length of a measurement vector */
  typedef unsigned int                                                MeasurementVectorSizeType;
  typedef typename TSample::MeasurementVectorType                     MeasurementVectorType;
  typedef typename TSample::MeasurementType                           MeasurementType;
  typedef typename NumericTraits< MeasurementType >::RealType         MeasurementRealType;
  typedef typename NumericTraits< MeasurementVectorType >::RealType   MeasurementVectorRealType;

  /** Method to set/get the sample */
  using Superclass::SetInput;
  void SetInput(const SampleType *sample);

  const SampleType *  GetInput() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MeasurementVectorRealType > MeasurementVectorDecoratedType;

  typedef MeasurementVectorDecoratedType OutputType;

  /** Get the mean measurement vector */
  const MeasurementVectorDecoratedType * GetOutput() const;

  const MeasurementVectorRealType GetMean() const;

  MeasurementVectorSizeType GetMeasurementVectorSize() const;

protected:
  MeanSampleFilter();
  virtual ~MeanSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** DataObject pointer */
  typedef DataObject::Pointer DataObjectPointer;

  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx);

  void GenerateData();

private:
  MeanSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);   //purposely not implemented
};                                // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSampleFilter.hxx"
#endif

#endif
