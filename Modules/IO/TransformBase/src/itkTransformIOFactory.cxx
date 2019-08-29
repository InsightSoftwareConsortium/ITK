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

#define ITK_TEMPLATE_EXPLICIT_TransformIOFactory
#include "itkTransformIOFactory.h"
namespace itk
{

template <typename TParametersValueType>
TransformIOFactoryTemplate<TParametersValueType>::TransformIOFactoryTemplate() = default;

template <typename TParametersValueType>
TransformIOFactoryTemplate<TParametersValueType>::~TransformIOFactoryTemplate() = default;

template <typename TParametersValueType>
typename TransformIOBaseTemplate<TParametersValueType>::Pointer
TransformIOFactoryTemplate<TParametersValueType>::CreateTransformIO(const char *                   path,
                                                                    TransformIOFactoryFileModeType mode)
{
  typename std::list<typename TransformIOBaseTemplate<TParametersValueType>::Pointer> possibleTransformIO;
  for (auto & allobject : ObjectFactoryBase::CreateAllInstance("itkTransformIOBaseTemplate"))
  {
    auto * io = dynamic_cast<TransformIOBaseTemplate<TParametersValueType> *>(allobject.GetPointer());
    if (io)
    {
      possibleTransformIO.push_back(io);
    }
  }
  for (auto k = possibleTransformIO.begin(); k != possibleTransformIO.end(); ++k)
  {
    if (mode == TransformIOFactoryFileModeType::ReadMode)
    {
      if ((*k)->CanReadFile(path))
      {
        return *k;
      }
    }
    else if (mode == TransformIOFactoryFileModeType::WriteMode)
    {
      if ((*k)->CanWriteFile(path))
      {
        return *k;
      }
    }
  }
  return nullptr;
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const TransformIOFactoryFileModeType value)
{
  const char * s = 0;
  switch (value)
  {
    case TransformIOFactoryFileModeType::ReadMode:
      s = "TransformIOFactoryFileModeType::ReadMode";
      break;
    case TransformIOFactoryFileModeType::WriteMode:
      s = "TransformIOFactoryFileModeType::WriteMode";
      break;
    default:
      s = "INVALID VALUE FOR TransformIOFactoryFileModeType";
  }
  return out << s;
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformBase_EXPORT TransformIOFactoryTemplate<double>;
template class ITKIOTransformBase_EXPORT TransformIOFactoryTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
