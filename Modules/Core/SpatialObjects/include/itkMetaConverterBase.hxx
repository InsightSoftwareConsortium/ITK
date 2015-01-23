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
#ifndef itkMetaConverterBase_hxx
#define itkMetaConverterBase_hxx
#include "itkMetaConverterBase.h"

#include "metaObject.h"

namespace itk
{

template <unsigned VDimension>
typename MetaConverterBase<VDimension>::SpatialObjectPointer
MetaConverterBase<VDimension>
::ReadMeta(const char *name)
{
  SpatialObjectPointer rval;
  MetaObjectType *mo = this->CreateMetaObject();

  mo->Read(name);
  rval = this->MetaObjectToSpatialObject(mo);
  delete mo;
  return rval;
}

template <unsigned VDimension>
bool
MetaConverterBase<VDimension>
::WriteMeta(const SpatialObjectType *spatialObject, const char *name)
{
  MetaObject *mo = this->SpatialObjectToMetaObject(spatialObject);
  mo->Write(name);
  delete mo;
  return true;
}

template <unsigned VDimension>
bool
MetaConverterBase<VDimension>
::GetWriteImagesInSeparateFile()
{
  return this->m_WriteImagesInSeparateFile;
}

template <unsigned VDimension>
void
MetaConverterBase<VDimension>
::SetWriteImagesInSeparateFile(bool writeImagesInSeparateFile)
{
  this->m_WriteImagesInSeparateFile = writeImagesInSeparateFile;
}

}

#endif // itkMetaConverterBase_hxx
