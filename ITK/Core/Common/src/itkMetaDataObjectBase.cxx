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
#include "itkMetaDataObjectBase.h"
#include "itkCommand.h"
#include "itkFastMutexLock.h"

void
itk::MetaDataObjectBase
::Print(std::ostream & os) const
{
  os << "[UNKNOWN_PRINT_CHARACTERISTICS]" << std::endl;
}

const char *
itk::MetaDataObjectBase
::GetMetaDataObjectTypeName(void) const
{
  return typeid( itk::MetaDataObjectBase ).name();
}

const std::type_info &
itk::MetaDataObjectBase
::GetMetaDataObjectTypeInfo(void) const
{
  return typeid( itk::MetaDataObjectBase );
}

itk::MetaDataObjectBase
::MetaDataObjectBase()
{
  //Nothing to do here
}

itk::MetaDataObjectBase
::~MetaDataObjectBase()
{
  //std::cout << "              MetaDataObjectBase Deleteing: " << this <<
  // std::endl;
  //Nothing to do here
}

#if __THIS_IS_UNNECESSARY_CODE__
itk::MetaDataObjectBase::Pointer
itk::MetaDataObjectBase
::New(void)
{
  Pointer                  smartPtr;
  itk::MetaDataObjectBase *rawPtr = ::itk::ObjectFactory< itk::MetaDataObjectBase >::Create();

  if ( rawPtr == NULL )
    {
    rawPtr = new itk::MetaDataObjectBase;
    }
  smartPtr = rawPtr;
  rawPtr->UnRegister();
  return smartPtr;
}

#endif
