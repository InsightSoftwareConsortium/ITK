/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDenseFrequencyContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
  typedef FrequencyContainerType::Pointer FrequencyContainerPointer ;

  /** calls the Initialize method of superclass to generate the offset table
   * and prepare the frequency container */
  void Initialize(unsigned long length) ;

  /** Method to set the frequency of histogram using instance identifier */
  void SetFrequency(const InstanceIdentifier id, const FrequencyType value)
  { (*m_FrequencyContainer)[id] = value ; }

  /** Method to increase the frequency by one.  This function is convinient
   * to create histogram. */
  void IncreaseFrequency(const InstanceIdentifier id, 
                         const FrequencyType value);

  /** Method to get the frequency of a bin from the histogram */
  FrequencyType GetFrequency(const InstanceIdentifier id) const ;

protected:
  DenseFrequencyContainer() ;
  virtual ~DenseFrequencyContainer() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  DenseFrequencyContainer(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  // Container of histogram
  FrequencyContainerPointer m_FrequencyContainer ;
} ; // end of class

  } // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATIONy
#include "itkDenseFrequencyContainer.txx"
#endif

#endif



