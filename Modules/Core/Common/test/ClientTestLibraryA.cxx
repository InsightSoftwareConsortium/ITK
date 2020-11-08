/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "ClientTestLibraryA.h"

#include "itkEquivalencyTable.h"
#include "itkImage.h"

namespace
{

template <typename TDerived>
int
dynamic_castDownCast(const char * type, const char * instanceSource, itk::Object const * base)
{
  using DerivedType = TDerived;

  constexpr static int passed = 0;
  constexpr static int failed = 1;

  DerivedType const * derived = dynamic_cast<DerivedType const *>(base);
  if (derived != nullptr)
  {
    std::cout << type << " cast in library A      for an instance from " << instanceSource << "\tsucceeded."
              << std::endl;
    return passed;
  }
  std::cerr << type << " cast in library A      for an instance from " << instanceSource << "\tfailed!" << std::endl;
  return failed;
}

} // end anonymous namespace

namespace LibraryA
{

ITKObjectProducer ::ITKObjectProducer()
{
  m_EquivalencyTable = itk::EquivalencyTable::New();
  using ImageType = itk::Image<float, 3>;
  m_Image = ImageType::New();
}

itk::Object *
ITKObjectProducer::EquivalencyTable()
{
  return m_EquivalencyTable.GetPointer();
}

itk::Object *
ITKObjectProducer::Image()
{
  return m_Image.GetPointer();
}

int
dynamic_castDownCastEquivalencyTable(const char * type, const char * instanceSource, itk::Object const * base)
{
  using EquivalencyTableType = itk::EquivalencyTable;
  return dynamic_castDownCast<EquivalencyTableType>(type, instanceSource, base);
}

int
dynamic_castDownCastImage(const char * type, const char * instanceSource, itk::Object const * base)
{
  using ImageType = itk::Image<float, 3>;
  return dynamic_castDownCast<ImageType>(type, instanceSource, base);
}

} // end namespace LibraryA
