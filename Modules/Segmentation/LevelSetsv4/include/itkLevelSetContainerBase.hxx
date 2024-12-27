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

#ifndef itkLevelSetContainerBase_hxx
#define itkLevelSetContainerBase_hxx


namespace itk
{

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::GetContainer() const -> const LevelSetContainerType &
{
  return m_Container;
}

template <typename TIdentifier, typename TLevelSet>
void
LevelSetContainerBase<TIdentifier, TLevelSet>::SetContainer(const LevelSetContainerType & iContainer)
{
  m_Container = iContainer;
}

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::Begin() -> Iterator
{
  return Iterator(m_Container.begin());
}

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::Begin() const -> ConstIterator
{
  return ConstIterator(m_Container.begin());
}

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::End() -> Iterator
{
  return Iterator(m_Container.end());
}

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::End() const -> ConstIterator
{
  return ConstIterator(m_Container.end());
}

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::Size() const -> LevelSetIdentifierType
{
  return static_cast<LevelSetIdentifierType>(m_Container.size());
}

template <typename TIdentifier, typename TLevelSet>
auto
LevelSetContainerBase<TIdentifier, TLevelSet>::GetLevelSet(const LevelSetIdentifierType & iId) const -> LevelSetPointer
{
  auto it = m_Container.find(iId);

  if (it != m_Container.end())
  {
    return it->second;
  }

  return nullptr;
}

template <typename TIdentifier, typename TLevelSet>
bool
LevelSetContainerBase<TIdentifier, TLevelSet>::AddLevelSet(const LevelSetIdentifierType & iId,
                                                           LevelSetType *                 iLevelSet,
                                                           const bool                     iForce)
{
  if (iForce)
  {
    m_Container[iId] = iLevelSet;
    this->Modified();
    return true;
  }

  if (m_Container.empty())
  {
    m_Container.insert(LevelSetPairType(iId, iLevelSet));
    this->Modified();
    return true;
  }
  else
  {
    auto it = m_Container.find(iId);

    if (it != m_Container.end())
    {
      return false;
    }
    else
    {
      m_Container.insert(LevelSetPairType(iId, iLevelSet));
      this->Modified();
      return true;
    }
  }
}

template <typename TIdentifier, typename TLevelSet>
bool
LevelSetContainerBase<TIdentifier, TLevelSet>::RemoveLevelSet(const LevelSetIdentifierType & iId)
{
  auto it = m_Container.find(iId);

  if (it != m_Container.end())
  {
    it->second = nullptr;
    m_Container.erase(it);

    this->Modified();

    return true;
  }

  return false;
}

template <typename TIdentifier, typename TLevelSet>
bool
LevelSetContainerBase<TIdentifier, TLevelSet>::HasDomainMap() const
{
  if (!this->m_DomainMapFilter.IsNull() && !this->m_DomainMapFilter->GetDomainMap().empty())
  {
    return true;
  }
  return false;
}

} // namespace itk

#endif // itkLevelSetContainerBase_hxx
