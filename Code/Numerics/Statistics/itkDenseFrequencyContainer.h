/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFrequencyContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDenseFrequencyContainer_h
#define __itkDenseFrequencyContainer_h

#include <map>
#include "itkObjectFactory.h"
#include "itkObject.h"
#include "itkValarrayImageContainer.h"

namespace itk{ 
  namespace Statistics{

/** \class DenseFrequencyContainer 
 *  \brief his class is a container for an histogram.
 *
 *  This class uses an map to store histogram. If your histogram is dense
 *  use DenseFrequencyContainer.  You should access each bin 
 * by (InstanceIdentifier)index or measurement vector.
 */
    
template< class TFrequencyValue = float >
class ITK_EXPORT DenseFrequencyContainer 
  : public Object
{
public:
  /** Standard class typedefs */
  typedef DenseFrequencyContainer  Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>   Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(DenseFrequencyContainer, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** instance idenfitifer alias */
  typedef unsigned long InstanceIdentifier ;

  /** frequency type alias */
  typedef TFrequencyValue FrequencyType ;

  /** Histogram typedef support */
  typedef ValarrayImageContainer< InstanceIdentifier, FrequencyType > 
  FrequencyContainerType ;
  typedef typename FrequencyContainerType::Pointer FrequencyContainerPointer ;

  /** calls the Initialize method of superclass to generate the offset table
   * and prepare the frequency container */
  void Initialize(unsigned long length) ;

  /** Method to set the frequency of histogram using instance identifier */
  void SetFrequency(const InstanceIdentifier id, const FrequencyType value) ;

  /** Method to increase the frequency by one.  This function is convinient
   * to create histogram. */
  void IncreaseFrequency(const InstanceIdentifier id, 
                         const FrequencyType value);

  /** Method to get the frequency of a bin from the histogram */
  FrequencyType GetFrequency(const InstanceIdentifier id) const ;

  FrequencyType GetTotalFrequency()
  { return m_TotalFrequency ; }

protected:
  DenseFrequencyContainer() ;
  virtual ~DenseFrequencyContainer() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  DenseFrequencyContainer(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  // Container of histogram
  FrequencyContainerPointer m_FrequencyContainer ;
  FrequencyType  m_TotalFrequency ;
} ; // end of class

  } // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATIONy
#include "itkDenseFrequencyContainer.txx"
#endif

#endif



