/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTable.txx
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
#ifndef __itkWatershedSegmentTable_txx
#define __itkWatershedSegmentTable_txx

namespace itk
{
namespace watershed
{

template <class TScalarType>
void SegmentTable<TScalarType>
::PruneEdgeLists(ScalarType maximum_saliency)
{
  Iterator it;
  edge_list_t::iterator e;
  for (it = this->Begin(); it != this->End(); ++it)
    {
      for (e = (*it).second.edge_list.begin();
           e != (*it).second.edge_list.end();
           e++)
        {
          if ( ( e->height - (*it).second.min ) > maximum_saliency )
            {  // dump the rest of the list, assumes list is sorted
              e++;
              (*it).second.edge_list.erase(e, (*it).second.edge_list.end() );
              break;  // through with this segment
            }
        }
    }
}
  
template <class TScalarType>
void SegmentTable<TScalarType>
::SortEdgeLists()
{
  Iterator it;
  for (it = this->Begin(); it != this->End(); ++it)
    {
      (*it).second.edge_list.sort();
    }
}
  
template <class TScalarType>
bool SegmentTable<TScalarType>
::Add(unsigned long a, const segment_t &t)
{
  std::pair<Iterator, bool> result;
  result = m_HashMap.insert( ValueType(a, t) );
  if (result.second == false) return false;
  else return true;
}

//template <class TScalarType>
//void SegmentTable<TScalarType>
//::PrintHashTable() const
//{
//  std::cout << "This has table has " << m_HashMap.size() << " entries." <<
//    std::endl;
//  std::cout << "Maximum depth is " << m_MaximumDepth << std::endl;
//  unsigned long i = 0;
// 
//  edge_list_t::const_iterator e;
//  ConstIterator it = this->Begin();
//  while (it != this->End() )
//    {
//          std::cout << "KEY: " << (*it).first << " = " << std::endl;
//          std::cout << "         min " << (*it).second.min << std::endl;
//         std::cout << "  edge_list = { ";
//          e = (*it).second.edge_list.begin();
//          while (e != (*it).second.edge_list.end() )
//            {
//              std::cout << "(" << (*e).label << ", " << (*e).height << ") ";
//              e++;
//            }
//          std::cout << std::endl;
//      it++;
//      i++;
//    }
//  std::cout << "Verified " << i << " entries" << std::endl;
//}
  
template <class TScalarType>
void
SegmentTable<TScalarType>
::UpdateOutputInformation()
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

template <class TScalarType>
void 
SegmentTable<TScalarType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}
  
}// end namespace watershed
}// end namespace itk

#endif
