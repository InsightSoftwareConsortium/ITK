/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFrequencyContainer.h
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
  typedef FrequencyContainerType::iterator FrequencyContainerIterator;   

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
