/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaxDecisionRule.h
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
#ifndef __itkMaxDecisionRule_h
#define __itkMaxDecisionRule_h

#include <vector>
#include "itkObject.h"

namespace itk{ 
  namespace Statistics{

/** \class MaxDecisionRule
 *  \brief A Decision rule that choose the class that has maximum value
 */
 
class ITK_EXPORT MaxDecisionRule : 
      public Object
{
public:
 /** Standard class typedefs */ 
  typedef MaxDecisionRule Self ;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;

 /** Run-time type information (and related methods) */
  itkTypeMacro(MaxDecisionRule, Object);

  /** Standard New() method support */
  itkNewMacro(Self) ;

  
  unsigned int Evaluate(std::vector< double > discriminantScores)
  {
    double max = discriminantScores[0] ;
    unsigned int maxIndex = 0 ;
    unsigned int i ;
    for (i = 0 ; i < discriminantScores.size() ; i++)
      {
        if (discriminantScores[i] > max) 
          {
            max = discriminantScores[i] ;
            maxIndex = i ;
          }
      }
    return maxIndex ;
  }

protected:
  MaxDecisionRule() {}
  virtual ~MaxDecisionRule() {}
  void PrintSelf(std::ostream& os, Indent indent) const 
  { Superclass::PrintSelf(os, indent) ; } 
} ; // end of class


  } // end of namespace Statistics 
} // end of namespace itk

#endif







