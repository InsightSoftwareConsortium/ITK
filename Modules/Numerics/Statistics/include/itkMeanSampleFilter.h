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
#ifndef itkMeanSampleFilter_h
#define itkMeanSampleFilter_h

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

template< typename TSample >
class ITK_TEMPLATE_EXPORT MeanSampleFilter : public ProcessObject
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

  /** Type of each measurement vector in sample */
  typedef typename SampleType::MeasurementVectorType                  MeasurementVectorType;

  /** Type of the length of each measurement vector */
  typedef typename SampleType::MeasurementVectorSizeType              MeasurementVectorSizeType;

  /** Type of measurement vector component value */
  typedef typename SampleType::MeasurementType                        MeasurementType;

  /** Type of a measurement vector, holding floating point values */
  typedef typename NumericTraits< MeasurementVectorType >::RealType   MeasurementVectorRealType;

  /** Type of a floating point measurement component value */
  typedef typename NumericTraits< MeasurementType >::RealType         MeasurementRealType;


  /** Method to set the sample */
  using Superclass::SetInput;
  void SetInput(const SampleType *sample);

  /** Method to get the sample */
  const SampleType * GetInput() const;


  /** Get the mean measurement vector */
  const MeasurementVectorRealType GetMean() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef SimpleDataObjectDecorator< MeasurementVectorRealType > MeasurementVectorDecoratedType;
  const MeasurementVectorDecoratedType * GetOutput() const;
  typedef MeasurementVectorDecoratedType                         OutputType;


  MeasurementVectorSizeType GetMeasurementVectorSize() const;

protected:
  MeanSampleFilter();
  virtual ~MeanSampleFilter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** DataObject pointer */
  typedef DataObject::Pointer DataObjectPointer;

  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MeanSampleFilter);
};                                // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSampleFilter.hxx"
#endif

#endif
