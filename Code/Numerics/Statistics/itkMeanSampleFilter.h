/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSampleFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanSampleFilter_h
#define __itkMeanSampleFilter_h

#include "itkProcessObject.h"
#include "itkArray.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkDataObject.h"

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
 */

template< class TSample >
class ITK_EXPORT MeanSampleFilter:public ProcessObject
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
  typedef   unsigned int                            MeasurementVectorSizeType;
  typedef   typename TSample::MeasurementVectorType MeasurementVectorType;
  typedef   typename TSample::MeasurementType       MeasurementType;

  /** Method to set/get the sample */
  void SetInput(const SampleType *sample);

  const SampleType *  GetInput() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MeasurementVectorType > MeasurementVectorDecoratedType;

  typedef MeasurementVectorDecoratedType OutputType;

  /** Get the mean measurement vector */
  const MeasurementVectorDecoratedType * GetOutput() const;

  const MeasurementVectorType            GetMean() const;

protected:
  MeanSampleFilter();
  virtual ~MeanSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** DataObject pointer */
  typedef DataObject::Pointer DataObjectPointer;

  virtual DataObjectPointer MakeOutput(unsigned int idx);

  void GenerateData();

private:
  MeanSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);   //purposely not implemented
};                                // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSampleFilter.txx"
#endif

#endif
