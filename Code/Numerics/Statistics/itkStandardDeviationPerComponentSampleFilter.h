/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStandardDeviationPerComponentSampleFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkStandardDeviationPerComponentSampleFilter_h
#define __itkStandardDeviationPerComponentSampleFilter_h

#include "itkProcessObject.h"

#include "itkArray.h"
#include "itkVariableSizeMatrix.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkNumericTraitsFixedArrayPixel.h"

namespace itk
{
namespace Statistics
{
/** \class StandardDeviationPerComponentSampleFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *
 * The filter calculates first the sample mean and use it in the covariance
 * calculation. The covariance is computed as follows
 * Let \f$\Sigma\f$ denotes covariance matrix for the sample, then:
 * When \f$x_{i}\f$ is \f$i\f$th component of a measurement vector
 * \f$\vec x\f$, \f$\mu_{i}\f$ is the \f$i\f$th componet of the \f$\vec\mu\f$,
 * and the \f$\sigma_{ij}\f$ is the \f$ij\f$th componet \f$\Sigma\f$,
 * \f$\sigma_{ij} = (x_{i} - \mu_{i})(x_{j} - \mu_{j})\f$
 *
 * Without the plugged in mean vector, this calculator will perform
 * the single pass mean and covariance calculation algorithm.
 *
 */

template< class TSample >
class ITK_EXPORT StandardDeviationPerComponentSampleFilter:
  public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef StandardDeviationPerComponentSampleFilter Self;
  typedef ProcessObject                             Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;
  typedef TSample                                   SampleType;

  /** Standard Macros */
  itkTypeMacro(StandardDeviationPerComponentSampleFilter, ProcessObject);
  itkNewMacro(Self);

  /** Length of a measurement vector */
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Measurement vector type */
  typedef typename TSample::MeasurementVectorType                   MeasurementVectorType;
  typedef typename NumericTraits< MeasurementVectorType >::RealType MeasurementVectorRealType;

  /** Method to set/get the sample */
  void SetInput(const SampleType *sample);

  const SampleType *  GetInput() const;

  /** MeasurementVector is not a DataObject, we need to decorate it to push it down
   * a ProcessObject's pipeline */
  typedef  SimpleDataObjectDecorator< MeasurementVectorRealType > MeasurementVectorRealDecoratedType;

  typedef MeasurementVectorRealDecoratedType OutputType;

  /** Return the standard deviation vector */
  const MeasurementVectorRealType GetStandardDeviationPerComponent() const;

  const MeasurementVectorRealDecoratedType * GetStandardDeviationPerComponentOutput() const;

  /** Return the mean vector */
  const MeasurementVectorRealType GetMeanPerComponent() const;

  const MeasurementVectorRealDecoratedType * GetMeanPerComponentOutput() const;

protected:
  StandardDeviationPerComponentSampleFilter(const Self &); //purposely not
                                                           // implemented
  void operator=(const Self &);                            //purposely not
                                                           // implemented

  StandardDeviationPerComponentSampleFilter();
  virtual ~StandardDeviationPerComponentSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** DataObject pointer */
  typedef DataObject::Pointer DataObjectPointer;

  virtual DataObjectPointer MakeOutput(unsigned int idx);

  void GenerateData();

  MeasurementVectorSizeType GetMeasurementVectorSize() const;

private:
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStandardDeviationPerComponentSampleFilter.txx"
#endif

#endif
