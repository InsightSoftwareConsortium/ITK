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


#include "itkMeshIOBase.h"
#include <type_traits>

namespace
{
// Tests `MeshIOBase::MapComponentType<TComponent>::CType`.
using itk::IOComponentEnum;
using itk::MeshIOBase;

struct UnknownComponentType
{};
static_assert(MeshIOBase::MapComponentType<UnknownComponentType>::CType == IOComponentEnum::UNKNOWNCOMPONENTTYPE);

static_assert(MeshIOBase::MapComponentType<unsigned char>::CType == IOComponentEnum::UCHAR);
static_assert(MeshIOBase::MapComponentType<signed char>::CType == IOComponentEnum::CHAR);
static_assert(MeshIOBase::MapComponentType<char>::CType ==
              (std::is_signed_v<char> ? IOComponentEnum::CHAR : IOComponentEnum::UCHAR));
static_assert(MeshIOBase::MapComponentType<unsigned short>::CType == IOComponentEnum::USHORT);
static_assert(MeshIOBase::MapComponentType<short>::CType == IOComponentEnum::SHORT);
static_assert(MeshIOBase::MapComponentType<unsigned int>::CType == IOComponentEnum::UINT);
static_assert(MeshIOBase::MapComponentType<int>::CType == IOComponentEnum::INT);
static_assert(MeshIOBase::MapComponentType<unsigned long>::CType == IOComponentEnum::ULONG);
static_assert(MeshIOBase::MapComponentType<long>::CType == IOComponentEnum::LONG);
static_assert(MeshIOBase::MapComponentType<unsigned long long>::CType == IOComponentEnum::ULONGLONG);
static_assert(MeshIOBase::MapComponentType<long long>::CType == IOComponentEnum::LONGLONG);
static_assert(MeshIOBase::MapComponentType<float>::CType == IOComponentEnum::FLOAT);
static_assert(MeshIOBase::MapComponentType<double>::CType == IOComponentEnum::DOUBLE);
static_assert(MeshIOBase::MapComponentType<long double>::CType == IOComponentEnum::LDOUBLE);
} // namespace
