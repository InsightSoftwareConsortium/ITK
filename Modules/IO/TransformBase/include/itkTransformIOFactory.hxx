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
#ifndef itkTransformIOFactory_hxx
#define itkTransformIOFactory_hxx

#include "itkTransformIOFactory.h"

namespace itk
{

template<typename TParametersValueType>
TransformIOFactoryTemplate<TParametersValueType>
::TransformIOFactoryTemplate()
{
}

template<typename TParametersValueType>
TransformIOFactoryTemplate<TParametersValueType>
::~TransformIOFactoryTemplate()
{
}

template<typename TParametersValueType>
typename TransformIOBaseTemplate<TParametersValueType>::Pointer
TransformIOFactoryTemplate<TParametersValueType>
::CreateTransformIO(const char *path, TransformIOFactoryFileModeType mode)
{
  typename std::list< typename TransformIOBaseTemplate<TParametersValueType>::Pointer > possibleTransformIO;
  std::list< LightObject::Pointer >     allobjects =
    ObjectFactoryBase::CreateAllInstance("itkTransformIOBaseTemplate");
  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end(); ++i )
    {
    TransformIOBaseTemplate<TParametersValueType> *io =
                        dynamic_cast< TransformIOBaseTemplate<TParametersValueType> * >( i->GetPointer() );
    if ( io )
      {
      possibleTransformIO.push_back(io);
      }
    }
  for ( typename std::list< typename TransformIOBaseTemplate<TParametersValueType>::Pointer >::iterator k = possibleTransformIO.begin();
        k != possibleTransformIO.end(); ++k )
    {
    if ( mode == ReadMode )
      {
      if ( ( *k )->CanReadFile(path) )
        {
        return *k;
        }
      }
    else if ( mode == WriteMode )
      {
      if ( ( *k )->CanWriteFile(path) )
        {
        return *k;
        }
      }
    }
  return ITK_NULLPTR;
}
} // end namespace itk

#endif
