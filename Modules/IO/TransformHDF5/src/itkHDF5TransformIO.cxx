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

#define ITK_TEMPLATE_EXPLICIT_HDF5TransformIO
#include "itkHDF5TransformIO.h"
#include "itkHDF5TransformIO.hxx"

namespace itk
{

// HDF uses hierarchical paths to find particular data
// in a file. These strings are used by both reading and
// writing.
const std::string HDF5CommonPathNames::transformGroupName(std::string("/TransformGroup"));
const std::string HDF5CommonPathNames::transformTypeName("/TransformType");
const std::string HDF5CommonPathNames::transformFixedName("/TranformFixedParameters");
const std::string HDF5CommonPathNames::transformParamsName("/TranformParameters");
const std::string HDF5CommonPathNames::ItkVersion("/ITKVersion");
const std::string HDF5CommonPathNames::HDFVersion("/HDFVersion");
const std::string HDF5CommonPathNames::OSName("/OSName");
const std::string HDF5CommonPathNames::OSVersion("/OSVersion");

// I couldn't figure out a way to represent transforms
// excepts as groups -- the HDF5 composite only allows
// fixed-size structures.
// Since (for now) transforms are ordered in a file, but
// not nameed, I name them by their order in the file,
// beginning with zero.
const std::string
GetTransformName(int i)
{
  std::stringstream s;
  s << HDF5CommonPathNames::transformGroupName;
  s << "/";
  s << i;
  return s.str();
}

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformHDF5_EXPORT HDF5TransformIOTemplate< double >;
template class ITKIOTransformHDF5_EXPORT HDF5TransformIOTemplate< float >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif

} // end namespace itk
