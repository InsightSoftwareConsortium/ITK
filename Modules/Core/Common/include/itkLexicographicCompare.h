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

#ifndef itkLexicographicCompare_h
#define itkLexicographicCompare_h

#include <algorithm>
#include "itkMacro.h"

namespace itk
{

/*
The  Functor was only used in one spot in the
LevelSet class,  It does not exist in Slicer, BRAINSTools, Remote modules,
ANTs, or any other project that I could find.
*/
namespace Functor
{
/** \class LexicographicCompare
 * \brief Order Index instances lexicographically.
 *
 * This is a comparison functor suitable for storing Index instances
 * in an STL container.  The ordering is total and unique but has
 * little geometric meaning.
 * \ingroup ITKCommon
 */
template< class TAggregateType1, class TAggregateType2 = TAggregateType1 >
class ITK_TEMPLATE_EXPORT LexicographicCompare
{
public:
  bool operator()(const TAggregateType1 &lhs, const TAggregateType2 &rhs) const
  {
    return std::lexicographical_compare( lhs.begin(), lhs.end(), rhs.begin(), rhs.end() );
  }
};

} //end namespace Functor
} //end namespace itk

#endif
