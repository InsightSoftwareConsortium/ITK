/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOneWayEquivalencyTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWatershedOneWayEquivalencyTable.h"

namespace itk
{
namespace watershed
{
  
bool OneWayEquivalencyTable::Add(unsigned long a, unsigned long b)
{
  //
  // Unlike EquivalencyTable, the order of the equivalence is important.
  //
  std::pair<Iterator, bool> result;
  if (a == b) return false;
  result = m_HashMap.insert( ValueType(a, b) );

  return result.second;
}
  
  //void OneWayEquivalencyTable::PrintHashTable()
  //{
  //  ConstIterator it = this->Begin();
  //  while (it != this->End() )
  //    {
  //      std::cout << (*it).first << " = " << (*it).second << std::endl;
  //      it++;
  //    }
  //}

void OneWayEquivalencyTable::Flatten()
{
  Iterator it = this->Begin();
  while ( it != this->End() )
    {
      (*it).second = this->RecursiveLookup((*it).first);
      it++;
    }
}

unsigned long OneWayEquivalencyTable::RecursiveLookup(const unsigned a) const
{
  unsigned long ans = a;
  unsigned long last_ans=a;
  while ( this->IsEntry(ans) )
    {
    ans = this->Lookup(ans);
    if (ans == a )
      {
      return last_ans; // about to cycle again.
      }
    last_ans = ans;
    }
 
  return ans;
}

void OneWayEquivalencyTable::UpdateOutputInformation()
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

void 
OneWayEquivalencyTable
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


  
}// end namespace watershed
}// end namespace itk
