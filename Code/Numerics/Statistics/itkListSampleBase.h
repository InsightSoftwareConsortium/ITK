/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkListSampleBase_h
#define __itkListSampleBase_h

#include "itkSample.h"

#include <vector>

namespace itk{ 
namespace Statistics{

/** \class ListSampleBase 
 *  \brief This class is the base class for containers that have a list
 * of measurement vectors
 * 
 * ListSampleBase allows duplicates of measurement vectors. It's not sorted.
 * It doesn't allow users to set frequency. The GetFrequency(...) methods
 * returns 1 if a measurement vector exists, else 0.
 *
 *\sa Sample, Histogram
 */

template< class TMeasurementVector >
class ITK_EXPORT ListSampleBase : public Sample< TMeasurementVector >
{
public:
  /** Standard class typedef. */
  typedef ListSampleBase  Self;
  typedef Sample< TMeasurementVector > Superclass;

  /** Standard macros */
  itkTypeMacro(ListSampleBase, Sample);

  /** Superclass typedefs for Measurement vector, 
   * measurement, Instance Identifier, 
   * frequency, size, size element value */

  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementType MeasurementType;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;

  /** VMeasurementVectorSize template argument alias */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TMeasurementVector::Length);

protected:
  ListSampleBase() {}
  virtual ~ListSampleBase() {}
  
private:
  ListSampleBase(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented
};

} // end of namespace Statistics 
} // end of namespace itk 

#endif
