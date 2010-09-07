/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOneWayEquivalencyTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkOneWayEquivalencyTable.h"

namespace itk
{
bool OneWayEquivalencyTable::Add(unsigned long a, unsigned long b)
{
  //
  // Unlike EquivalencyTable, the order of the equivalence is important.
  //
  std::pair< Iterator, bool > result;
  if ( a == b ) { return false; }
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
    ( *it ).second = this->RecursiveLookup( ( *it ).first );
    it++;
    }
}

unsigned long OneWayEquivalencyTable::RecursiveLookup(const unsigned long a) const
{
  unsigned long ans = a;
  unsigned long last_ans = a;

  ConstIterator it;
  ConstIterator hashEnd = m_HashMap.end();

  while ( ( it = m_HashMap.find(ans) ) != hashEnd )
    {
    ans = ( *it ).second;
    if ( ans == a )
      {
      return last_ans;              // about to cycle again.
      }
    last_ans = ans;
    }

  return ans;
}

void
OneWayEquivalencyTable
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk
