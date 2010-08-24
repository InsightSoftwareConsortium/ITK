/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleToSubsampleFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleToSubsampleFilter_h
#define __itkSampleToSubsampleFilter_h

#include "itkSample.h"
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
 */

template< class TSample >
class ITK_EXPORT SampleToSubsampleFilter:public ProcessObject
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
  virtual void SetInput(const SampleType *sample);

  virtual const SampleType * GetInput() const;

  /** Get the output subsample */
  const OutputType  * GetOutput() const;

protected:
  SampleToSubsampleFilter();
  virtual ~SampleToSubsampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Make a DataObject of the correct type to used as the specified
   * output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.
   * \sa ProcessObject
   */
  virtual DataObjectPointer MakeOutput(unsigned int idx);

private:
  SampleToSubsampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);          //purposely not implemented
};                                       // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleToSubsampleFilter.txx"
#endif

#endif
