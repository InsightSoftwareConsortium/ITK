/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSample.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSample_h
#define __itkListSample_h

#include "itkSample.h"

#include <vector>

namespace itk{ 
namespace Statistics{

/** \class ListSample 
 *  \brief This class is the base class for containers that have a list
 * of measurement vectors
 * 
 * ListSample allows duplicates of measurement vectors. It's not sorted.
 * It doesn't allow users to set frequency. The GetFrequency(...) methods
 * returns 1 if a measurement vector exists, else 0.
 *
 *\sa Sample, Histogram
 */

template< class TMeasurementVector >
class ITK_EXPORT ListSample : public Sample< TMeasurementVector >
{
public:
  /** Standard class typedef. */
  typedef ListSample  Self;
  typedef Sample< TMeasurementVector > Superclass;

  /** Standard macros */
  itkTypeMacro(ListSample, Sample);

  /** Superclass typedefs for Measurement vector, 
   * measurement, Instance Identifier, 
   * frequency, size, size element value */

  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementType MeasurementType;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;

  /** VMeasurementVectorSize template argument alias */
  enum { MeasurementVectorSize = TMeasurementVector::Length } ;

protected:
  ListSample() ;
  virtual ~ListSample() {};
  void PrintSelf(std::ostream& os, Indent indent) const; 
  
private:
  ListSample(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented
};

} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkListSample.txx"
#endif

#endif
