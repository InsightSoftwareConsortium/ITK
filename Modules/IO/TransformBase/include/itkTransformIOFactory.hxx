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
#ifndef __itkTransformIOFactory_hxx
#define __itkTransformIOFactory_hxx

#include "itkTransformIOFactory.h"

namespace itk
{
template<typename ParametersValueType>
typename TransformIOBaseTemplate<ParametersValueType>::Pointer
TransformIOFactoryTemplate<ParametersValueType>
::CreateTransformIO(const char *path, TransformIOFactoryFileModeType mode)
{
  typename std::list< typename TransformIOBaseTemplate<ParametersValueType>::Pointer > possibleTransformIO;
  std::list< LightObject::Pointer >     allobjects =
    ObjectFactoryBase::CreateAllInstance("itkTransformIOBaseTemplate");
  for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
        i != allobjects.end(); ++i )
    {
    TransformIOBaseTemplate<ParametersValueType> *io =
                        dynamic_cast< TransformIOBaseTemplate<ParametersValueType> * >( i->GetPointer() );
    if ( io )
      {
      possibleTransformIO.push_back(io);
      }
    }
  for ( typename std::list< typename TransformIOBaseTemplate<ParametersValueType>::Pointer >::iterator k = possibleTransformIO.begin();
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
  return 0;
}
} // end namespace itk

#endif
