/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedEquivalencyTable.txx
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
#ifndef __itkWatershedEquivalencyTable_txx
#define __itkWatershedEquivalencyTable_txx

namespace itk
{
namespace watershed
{
  
bool EquivalencyTable::Add(unsigned long a, unsigned long b)
{
  unsigned long temp;
  std::pair<Iterator, bool> result;
  if (a == b) return false;
  else if (a < b)
    {  // swap a, b
      temp = a;
      a = b;
      b = temp;
    }
  result = m_HashMap.insert( ValueType(a, b) );
  
  if (result.second == false)
    { // Stop endless loops.
      if ( (*(result.first)).second  == b ) return false;
      else return (this->Add((*(result.first)).second, b));
    }
  else return true;
}
  
  //void EquivalencyTable::PrintHashTable()
  //{
  //  ConstIterator it = this->Begin();
  //  while (it != this->End() )
  //    {
      //      std::cout << (*it).first << " = " << (*it).second << std::endl;
  //      it++;
  //    }
  //}

void EquivalencyTable::Flatten()
{
  Iterator it = this->Begin();
  while ( it != this->End() )
    {
      (*it).second = this->RecursiveLookup((*it).first);
      it++;
    }
}

unsigned long EquivalencyTable::RecursiveLookup(const unsigned a) const
{
  unsigned long ans = a;
  unsigned long last_ans;
  while ( this->IsEntry(ans) )
    {
      ans = this->Lookup(ans);
      if (ans == a ) return last_ans; // about to cycle again.
      last_ans = ans;
    }
  return ans;

}

void EquivalencyTable::UpdateOutputInformation()
{
  if (this->GetSource())
    {
      this->GetSource()->UpdateOutputInformation();
    }
  else
    {
      
    }
  
  // Now we should know what our largest possible region is. If our 
  // requested region was not set yet, (or has been set to something 
  // invalid - with no data in it ) then set it to the largest possible
  // region.
  if ( ! m_RequestedRegionInitialized)
    {
      this->SetRequestedRegionToLargestPossibleRegion();
      m_RequestedRegionInitialized = true;
    }
  
  m_LastRequestedRegionWasOutsideOfTheBufferedRegion = 0;
}

void EquivalencyTable
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


  
}// end namespace watershed
}// end namespace itk

#endif
