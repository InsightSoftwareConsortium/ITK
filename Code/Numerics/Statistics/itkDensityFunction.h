/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDensityFunction.h
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
#ifndef __itkDensityFunction_h
#define __itkDensityFunction_h

#include "itkObject.h"

namespace itk{ 
  namespace Statistics{

/** \class DensityFunction
 * \brief DensityFunction class represents  Density Function.
 *
 * This class keeps parameter to define  Density Function  and has
 * method to return the probability density 
 * of an instance.  MeasurementVectorSize is the dimension of measurement space.
 * double is type of measurement. 
 */

template< class TMeasurementVector >
class ITK_EXPORT DensityFunction :
      public Object
{
public:
  /** Standard class typedefs */
  typedef DensityFunction Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Strandard macros */
  itkTypeMacro(DensityFunction, Object);
  itkNewMacro(Self);

  /** Typedef alias for the measurement vectors */
  typedef TMeasurementVector MeasurementVectorType ;

 /** Method to get probability of an instance. The return value is the
  * value of the density function, not probability. */
  virtual double Evaluate(MeasurementVectorType measurement) = 0 ;
  

protected:
  DensityFunction(void) {}
  virtual ~DensityFunction(void) {}
  void PrintSelf(std::ostream& os, Indent indent) const
  { Superclass::PrintSelf(os,indent) ; }
} ; // end of class

  } // end of namespace Statistics
} // end namespace itk

#endif
