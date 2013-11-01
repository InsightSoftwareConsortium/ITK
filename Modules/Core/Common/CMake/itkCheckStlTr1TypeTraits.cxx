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

//
// Check if the compiler has is_convertible and type traits in tr1 namespace
//

#include <iostream>

#ifdef ITK_HAS_STLTR1_TR1_TYPE_TRAITS
#  include <tr1/type_traits>
#elif defined ITK_HAS_STLTR1_TYPE_TRAITS
#  include <type_traits>
#endif

int main(void)
{
    // Just try to use one type_traits function in tr1
    std::tr1::is_convertible<float, double>();
    return 0;
}
