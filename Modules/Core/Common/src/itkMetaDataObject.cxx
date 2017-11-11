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
#define ITK_TEMPLATE_EXPLICIT_MetaDataObject
#include "itkMetaDataObject.h"

namespace itk
{

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKCommon_EXPORT MetaDataObject< bool >;
template class ITKCommon_EXPORT MetaDataObject< unsigned char >;
template class ITKCommon_EXPORT MetaDataObject< char >;
template class ITKCommon_EXPORT MetaDataObject< signed char >;
template class ITKCommon_EXPORT MetaDataObject< unsigned short >;
template class ITKCommon_EXPORT MetaDataObject< short >;
template class ITKCommon_EXPORT MetaDataObject< unsigned int >;
template class ITKCommon_EXPORT MetaDataObject< int >;
template class ITKCommon_EXPORT MetaDataObject< unsigned long >;
template class ITKCommon_EXPORT MetaDataObject< long >;
template class ITKCommon_EXPORT MetaDataObject< unsigned long long >;
template class ITKCommon_EXPORT MetaDataObject< long long >;
template class ITKCommon_EXPORT MetaDataObject< float >;
template class ITKCommon_EXPORT MetaDataObject< double >;
template class ITKCommon_EXPORT MetaDataObject< std::string >;
template class ITKCommon_EXPORT MetaDataObject< Array<char> >;
template class ITKCommon_EXPORT MetaDataObject< Array<int> >;
template class ITKCommon_EXPORT MetaDataObject< Array<float> >;
template class ITKCommon_EXPORT MetaDataObject< Array<double> >;
template class ITKCommon_EXPORT MetaDataObject< Matrix<double> >;
template class ITKCommon_EXPORT MetaDataObject< std::vector<float> >;
template class ITKCommon_EXPORT MetaDataObject< std::vector<double> >;
template class ITKCommon_EXPORT MetaDataObject< std::vector<std::vector<double> > >;
template class ITKCommon_EXPORT MetaDataObject< std::vector<std::vector<float> > >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif

} // end namespace itk
