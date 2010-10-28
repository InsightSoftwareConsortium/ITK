/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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
