/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeValleyFunction.cxx
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
#include "itkCompositeValleyFunction.h"

namespace itk {
CompositeValleyFunction
::CompositeValleyFunction(std::vector<double>& classMeans, 
                                std::vector<double>& classSigmas)
{
  int length = classMeans.size() ;

  if (length != classSigmas.size() || length == 0)
    throw ExceptionObject() ;
  
  for (int i = 0 ; i < length ; i++) 
    {
      this->AddNewClass(classMeans[i], classSigmas[i]) ;
    }

  this->Initialize() ;
}

void CompositeValleyFunction
::Initialize() 
{
  long i,low,high;
  
  // build table
  // when using valley-func then the table values run from 
  // lowest_my - 10 * sigma to highest_my + 10 * sigma
  
  low = 0; high = 0;
  
  int noOfClasses = m_Targets.size() ;

  for (i = 0 ; i < noOfClasses ; i++) 
    {
      if (m_Targets[i].GetMean() > m_Targets[high].GetMean()) 
        high = i;
      if (m_Targets[i].GetMean() < m_Targets[low].GetMean()) 
        low =i;
    }
  
  m_LowerBound = m_Targets[low].GetMean() - 
    9 * m_Targets[low].GetSigma() ;
  m_HigherBound = m_Targets[high].GetMean() + 
    9 * m_Targets[high].GetSigma() ;

  CreateCache(m_LowerBound, m_HigherBound, 1000000) ;
}

} // end of namespace itk
