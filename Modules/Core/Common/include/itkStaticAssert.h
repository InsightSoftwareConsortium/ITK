/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkStaticAssert_h
#define itkStaticAssert_h

// This is no longer needed as C++11 provides this support directly
#define itkStaticAssert(expr, str) static_assert(false, "Use C++ 11 static_assert directly")

// TODO: remove this file entirely in the future (e.g. with ITKv6)

#endif // itkStaticAssert_h
