/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 *  \brief This class is the base class for Samples that store measurements in a list
 *
 * ListSampleBase stores measurements in a list type structure (as
 * opposed to a Histogram, etc.).  ListSampleBase allows duplicate
 * measurements. ListSampleBase is not sorted.
 *
 * ListSampleBase does not allow the user to specify the frequency of
 * a measurement directly.  The GetFrequency() methods returns 1 if
 * the measurement exists in the list, 0 otherwise.
 * 
 * Recent API changes:
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. Please use the function 
 * GetMeasurementVectorSize() instead.
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

  /** Typedefs inherited from the superclass */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  typedef typename Superclass::MeasurementType MeasurementType;
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;

  /** Vector of InstanceIdentifiers used for returning search
   * results. */
  typedef std::vector< InstanceIdentifier > SearchResultVectorType ;

  /** Search for measurements within the specified radius of a search
   * point. A vector of InstanceIdentifiers is returned. */
  inline void Search(MeasurementVectorType center, double radius, 
                     SearchResultVectorType& result) const
  {
    if (radius == 0.0)
      {
      itkGenericExceptionMacro("Search radius should be greater than zero.") ;
      }
    
    unsigned int j ;
    double squaredRadius ;
    double distance ;
    double coordinateDistance ;
    
    MeasurementVectorType tempVector ;
    
    squaredRadius = radius * radius ;
    
    result.clear() ;
    for ( InstanceIdentifier id = 0 ; id < this->Size() ; ++id )
      {
      distance = 0.0 ;
      tempVector = this->GetMeasurementVector( id ) ;
      for (j = 0 ; j < this->GetMeasurementVectorSize() && distance < squaredRadius ; j++)
        {
        coordinateDistance = (double)tempVector[j] - center[j] ;
        if (vnl_math_abs(coordinateDistance) > radius )
          {
          distance = squaredRadius ;
          }
        }
      
      for (j = 0 ; j < this->GetMeasurementVectorSize() && distance < squaredRadius ; j++)
        {
        coordinateDistance = (double)tempVector[j] - center[j] ;
        distance += coordinateDistance * coordinateDistance ;
        }
      
      if (distance < squaredRadius)
        {
        result.push_back( id ) ;
        }
      }
  }
  
protected:
  ListSampleBase() {}
  virtual ~ListSampleBase() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    {
    Superclass::PrintSelf(os,indent);
    }

  
private:
  ListSampleBase(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

};

} // end of namespace Statistics 
} // end of namespace itk 

#endif
