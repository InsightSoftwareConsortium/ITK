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
#ifndef itkHDF5TransformIOExport_h
#define itkHDF5TransformIOExport_h

#if defined(ITKDLL) && defined(ITK_WINDOWS_EXPORT_ALL_SYMBOLS)
# ifdef ITKIOTransformHDF5_EXPORTS
#  define ITKHDF5TransformExport
# else
#  if !defined(ITKIOTransformHDF5_EXPORTS) && defined(_WIN32) || defined(WIN32)
#   define ITKHDF5TransformExport __declspec(dllimport)
#  endif
# endif
#endif

#ifndef ITKHDF5TransformExport
#  define ITKHDF5TransformExport
#endif
#endif
