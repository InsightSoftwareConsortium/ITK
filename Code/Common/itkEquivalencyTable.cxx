/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEquivalencyTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkEquivalencyTable.h"

namespace itk
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
    if ( (*(result.first)).second == b ) return false;
    else return (this->Add((*(result.first)).second, b));
    }
  else return true;
}


bool EquivalencyTable::AddAndFlatten(unsigned long a, unsigned long b)
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

  unsigned long bFlattened = this->RecursiveLookup(b);
  result = m_HashMap.insert( ValueType(a, bFlattened) );  
  
  if (result.second == false)
    { // Stop endless loops.
    if ( (*(result.first)).second == bFlattened ) return false;
    else return (this->Add((*(result.first)).second, bFlattened));
    }
  else
    {
    if (b != bFlattened)
      {
      // flatten b as well
      m_HashMap.insert( ValueType(b, bFlattened) );
      return true;
      }
    }
  
  return false;
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
    (*it).second = this->RecursiveLookup((*it).second);
    it++;
    }
}

unsigned long EquivalencyTable::RecursiveLookup(const unsigned a) const
{
  unsigned long ans = a;
  unsigned long last_ans=a;

  ConstIterator it;
  ConstIterator hashEnd = m_HashMap.end();
  while ( (it = m_HashMap.find(ans)) != hashEnd )
    {
    ans = (*it).second;
    if (ans == a ) return last_ans; // about to cycle again.
    last_ans = ans;
    }

  return ans;
}


void EquivalencyTable
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

  
}// end namespace itk
