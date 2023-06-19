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
#ifndef itkVectorContainer_hxx
#define itkVectorContainer_hxx

#include "itkNumericTraits.h"

namespace itk
{

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::ElementAt(ElementIdentifier id) -> reference
{
  this->Modified();
  return this->VectorType::operator[](id);
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::ElementAt(ElementIdentifier id) const -> const_reference
{
  return this->VectorType::operator[](id);
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::CreateElementAt(ElementIdentifier id) -> reference
{
  if (id >= static_cast<ElementIdentifier>(this->VectorType::size()))
  {
    this->CreateIndex(id);
  }
  this->Modified();
  return this->VectorType::operator[](id);
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::GetElement(ElementIdentifier id) const -> Element
{
  return this->VectorType::operator[](id);
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::SetElement(ElementIdentifier id, Element element)
{
  this->VectorType::operator[](id) = element;
  this->Modified();
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::InsertElement(ElementIdentifier id, Element element)
{
  if (id >= static_cast<ElementIdentifier>(this->VectorType::size()))
  {
    this->CreateIndex(id);
  }
  this->VectorType::operator[](id) = element;

  this->Modified();
}

template <typename TElementIdentifier, typename TElement>
bool
VectorContainer<TElementIdentifier, TElement>::IndexExists(ElementIdentifier identifier) const
{
  return (NumericTraits<ElementIdentifier>::IsNonnegative(identifier) &&
          (identifier < static_cast<ElementIdentifier>(this->VectorType::size())));
}

template <typename TElementIdentifier, typename TElement>
bool
VectorContainer<TElementIdentifier, TElement>::GetElementIfIndexExists(ElementIdentifier identifier,
                                                                       Element *         element) const
{
  if (NumericTraits<ElementIdentifier>::IsNonnegative(identifier) &&
      (identifier < static_cast<ElementIdentifier>(this->VectorType::size())))
  {
    if (element)
    {
      *element = this->VectorType::operator[](identifier);
    }
    return true;
  }
  return false;
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::CreateIndex(ElementIdentifier id)
{
  if (id >= static_cast<ElementIdentifier>(this->VectorType::size()))
  {
    // The vector must be expanded to fit the new id.
    this->VectorType::resize(id + 1);
    this->Modified();
  }
  else if (id > 0)
  {
    // No expansion was necessary. Just overwrite the index's entry with the default element.
    this->VectorType::operator[](id) = Element();

    this->Modified();
  }
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::DeleteIndex(ElementIdentifier id)
{
  this->VectorType::operator[](id) = Element();
  this->Modified();
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::Begin() const -> ConstIterator
{
  return ConstIterator(0, this->VectorType::begin());
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::End() const -> ConstIterator
{
  return ConstIterator(this->VectorType::size() - 1, this->VectorType::end());
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::Begin() -> Iterator
{
  return Iterator(0, this->VectorType::begin());
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::End() -> Iterator
{
  return Iterator(this->VectorType::size() - 1, this->VectorType::end());
}

template <typename TElementIdentifier, typename TElement>
auto
VectorContainer<TElementIdentifier, TElement>::Size() const -> ElementIdentifier
{
  return static_cast<ElementIdentifier>(this->VectorType::size());
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::Initialize()
{
  this->VectorType::clear();
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::Reserve(ElementIdentifier sz)
{
  this->CreateIndex(sz - 1);
}

template <typename TElementIdentifier, typename TElement>
void
VectorContainer<TElementIdentifier, TElement>::Squeeze()
{}
} // end namespace itk

#endif
