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
#include "itkParametricPath.h"

namespace itk
{
// Instantiate the versions of the ParametricPath that need to be
// placed in the library.  There are non-templated subclasses of
// ParametricPath (ex. OrthogonallyCorrected2DParametricPath is a
// subclass of ParametricPath<2>) and these instantiations must be in
// the library so that the instantiations can be shared amongst
// implicit instantiations of templated subclasses of ParametricPath.
template class ParametricPath< 2 >;
} // end namespace itk
