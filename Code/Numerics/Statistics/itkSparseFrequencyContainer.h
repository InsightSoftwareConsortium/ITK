/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSparseFrequencyContainer_h
#define __itkSparseFrequencyContainer_h

#include <map>
#include "itkObjectFactory.h"
#include "itkObject.h"

namespace itk{ 
  namespace Statistics{

/** \class SparseFrequencyContainer 
 *  \brief his class is a container for an histogram.
 *
 *  This class uses an map to store histogram. If your histogram is dense
 *  use DenseHistogram.  You should access each bin by 
 * (InstanceIdentifier)index or measurement vector.
 */
    
template< class TFrequencyValue = float >
class ITK_EXPORT SparseFrequencyContainer : public Object
{
public:
  /** Standard class typedefs. */
  typedef SparseFrequencyContainer  Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>   Pointer;

  /** Standard macros */
  itkTypeMacro(SparseFrequencyContainer, Object);
  itkNewMacro(Self);

  /** instance idenfitifer alias */
  typedef unsigned long InstanceIdentifier ;

  /** frequency type alias */
  typedef TFrequencyValue FrequencyType ;

  /** Histogram typedef support */
  typedef std::map< InstanceIdentifier, FrequencyType > 
  FrequencyContainerType ;

  /** Iterator typedef support */
  typedef typename FrequencyContainerType::iterator 
  FrequencyContainerIterator ;   

  /** prepares the frequency container */
  void Initialize(unsigned long length) ;

  /** Method to set the frequency of histogram using instance identifier */
  void SetFrequency(const InstanceIdentifier id, const FrequencyType value)
  { m_FrequencyContainer[id] = value ; }

  /** Method to increase the frequency by one.  This function is convinent
   * to create histogram. */
  void IncreaseFrequency(const InstanceIdentifier id, 
                         const FrequencyType value);

  /** Method to get the frequency of a bin from the histogram */
  FrequencyType GetFrequency(const InstanceIdentifier id) const ;

protected:
  SparseFrequencyContainer() {}
  virtual ~SparseFrequencyContainer() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SparseFrequencyContainer(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  // Container of histogram
  FrequencyContainerType m_FrequencyContainer ;
} ; // end of class

  } // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseFrequencyContainer.txx"
#endif

#endif
