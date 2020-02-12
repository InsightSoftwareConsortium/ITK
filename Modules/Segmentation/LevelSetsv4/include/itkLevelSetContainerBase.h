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

#ifndef itkLevelSetContainerBase_h
#define itkLevelSetContainerBase_h

#include <map>
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkHeavisideStepFunctionBase.h"
#include "itkLevelSetDomainMapImageFilter.h"


namespace itk
{
/**
 *  \class LevelSetContainerBase
 *  \brief Container of level set
 *
 *  Encapsulate an ordered set of level set function (see LevelSet).
 *
 * \tparam TIdentifier type of the identifier used to reference on level set function
 * \tparam TLevelSet type of level set function in the container.
 * \ingroup ITKLevelSetsv4
 */
template <typename TIdentifier, typename TLevelSet>
class ITK_TEMPLATE_EXPORT LevelSetContainerBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetContainerBase);

  using Self = LevelSetContainerBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using Superclass = Object;

  /** Run-time type information */
  itkTypeMacro(LevelSetContainerBase, Object);

  /** type alias related to the type of level set*/
  using LevelSetType = TLevelSet;
  using LevelSetPointer = typename LevelSetType::Pointer;
  using InputIndexType = typename LevelSetType::InputType;
  using OutputType = typename LevelSetType::OutputType;
  using OutputRealType = typename LevelSetType::OutputRealType;
  using GradientType = typename LevelSetType::GradientType;
  using HessianType = typename LevelSetType::HessianType;
  using LevelSetDataType = typename LevelSetType::LevelSetDataType;

  /** IdentifierType */
  using LevelSetIdentifierType = TIdentifier;

  using LevelSetContainerType = std::map<LevelSetIdentifierType, LevelSetPointer>;
  using LevelSetContainerConstIteratorType = typename LevelSetContainerType::const_iterator;
  using LevelSetContainerIteratorType = typename LevelSetContainerType::iterator;

  using HeavisideType = HeavisideStepFunctionBase<OutputRealType, OutputRealType>;
  using HeavisideConstPointer = typename HeavisideType::ConstPointer;

  static constexpr unsigned int Dimension = LevelSetType::Dimension;

  using IdListType = std::list<LevelSetIdentifierType>;
  using IdListIterator = typename IdListType::iterator;
  using IdListConstIterator = typename IdListType::const_iterator;
  using IdListImageType = Image<IdListType, Dimension>;
  using CacheImageType = Image<short, Dimension>;

  using DomainMapImageFilterType = LevelSetDomainMapImageFilter<IdListImageType, CacheImageType>;

  using LevelSetPairType = std::pair<LevelSetIdentifierType, LevelSetPointer>;

  using DomainMapImageFilterPointer = typename DomainMapImageFilterType::Pointer;
  using LevelSetDomainType = typename DomainMapImageFilterType::LevelSetDomain;

  using DomainContainerType = std::map<LevelSetIdentifierType, LevelSetDomainType>;
  using DomainIteratorType = typename DomainContainerType::iterator;

  /** Declare iterators to container. */
  class Iterator;
  friend class Iterator;

  /**
   *\class ConstIterator
    \ingroup ITKLevelSetsv4
    */
  class ConstIterator
  {
  public:
    ConstIterator() = default;
    ConstIterator(const LevelSetContainerConstIteratorType & it)
      : m_Iterator(it)
    {}
    ~ConstIterator() = default;
    ConstIterator(const Iterator & it)
      : m_Iterator(it.m_Iterator)
    {}

    ConstIterator & operator*() { return *this; }
    ConstIterator * operator->() { return this; }
    ConstIterator &
    operator++()
    {
      ++m_Iterator;
      return *this;
    }
    ConstIterator
    operator++(int)
    {
      ConstIterator tmp(*this);
      ++(*this);
      return tmp;
    }
    ConstIterator &
    operator--()
    {
      --m_Iterator;
      return *this;
    }
    ConstIterator
    operator--(int)
    {
      ConstIterator tmp(*this);
      --(*this);
      return tmp;
    }
    bool
    operator==(const Iterator & it) const
    {
      return (m_Iterator == it.m_Iterator);
    }
    bool
    operator!=(const Iterator & it) const
    {
      return (m_Iterator != it.m_Iterator);
    }
    bool
    operator==(const ConstIterator & it) const
    {
      return (m_Iterator == it.m_Iterator);
    }
    bool
    operator!=(const ConstIterator & it) const
    {
      return (m_Iterator != it.m_Iterator);
    }

    LevelSetIdentifierType
    GetIdentifier() const
    {
      return m_Iterator->first;
    }

    LevelSetType *
    GetLevelSet() const
    {
      return m_Iterator->second;
    }

  private:
    LevelSetContainerConstIteratorType m_Iterator;
    friend class Iterator;
  };

  /**
   *\class Iterator
    \ingroup ITKLevelSetsv4 */
  class Iterator
  {
  public:
    Iterator() = default;
    Iterator(const LevelSetContainerIteratorType & it)
      : m_Iterator(it)
    {}
    Iterator(const ConstIterator & it)
      : m_Iterator(it.m_Iterator)
    {}
    ~Iterator() = default;

    Iterator & operator*() { return *this; }
    Iterator * operator->() { return this; }
    Iterator &
    operator++()
    {
      ++m_Iterator;
      return *this;
    }
    Iterator
    operator++(int)
    {
      Iterator tmp(*this);
      ++(*this);
      return tmp;
    }
    Iterator &
    operator--()
    {
      --m_Iterator;
      return *this;
    }
    Iterator
    operator--(int)
    {
      Iterator tmp(*this);
      --(*this);
      return tmp;
    }
    bool
    operator==(const Iterator & it) const
    {
      return (m_Iterator == it.m_Iterator);
    }
    bool
    operator!=(const Iterator & it) const
    {
      return (m_Iterator != it.m_Iterator);
    }
    bool
    operator==(const ConstIterator & it) const
    {
      return (m_Iterator == it.m_Iterator);
    }
    bool
    operator!=(const ConstIterator & it) const
    {
      return (m_Iterator != it.m_Iterator);
    }

    LevelSetIdentifierType
    GetIdentifier() const
    {
      return m_Iterator->first;
    }

    LevelSetType *
    GetLevelSet() const
    {
      return m_Iterator->second;
    }

  private:
    LevelSetContainerIteratorType m_Iterator;
    friend class ConstIterator;
  };

  Iterator
  Begin();
  Iterator
  End();

  ConstIterator
  Begin() const;
  ConstIterator
  End() const;

  /** Get the number of LevelSets in the container. */
  LevelSetIdentifierType
  Size() const;

  /** \brief Get the level set function given its id
    \param[in] iId
    \return the level set function if it is in the container, else nullptr.
  */
  LevelSetPointer
  GetLevelSet(const LevelSetIdentifierType & iId) const;

  /** \brief Add one level set function given its id.

    \param[in] iId id of the level set function
    \param[in] iLevelSet the level set function to be added
    \param[in] iForce if iForce is true (default) the level set function will be
    added to the container even if there is already one with the same id.

    \return true if the level set has been added.
  */
  bool
  AddLevelSet(const LevelSetIdentifierType & iId, LevelSetType * iLevelSet, const bool iForce = true);

  /** \brief Remove one level set function given its id.
    \param[in] iId id of the level set function to be removed
    \return true if it has been removed, false if the id was not present in the
    container.
  */
  bool
  RemoveLevelSet(const LevelSetIdentifierType & iId);

  /** \todo add documentation */
  itkSetConstObjectMacro(Heaviside, HeavisideType);
  itkGetConstObjectMacro(Heaviside, HeavisideType);

  /** Set/Get the domain map image filter. */
  itkSetObjectMacro(DomainMapFilter, DomainMapImageFilterType);
  itkGetModifiableObjectMacro(DomainMapFilter, DomainMapImageFilterType);

  /** Does the level set container have a domain map? */
  bool
  HasDomainMap() const;

protected:
  /** \brief Default Constructor */
  LevelSetContainerBase() = default;

  /** \brief Default Destructor */
  ~LevelSetContainerBase() override = default;

  const LevelSetContainerType &
  GetContainer() const;
  void
  SetContainer(const LevelSetContainerType & iContainer);

private:
  HeavisideConstPointer       m_Heaviside;
  DomainMapImageFilterPointer m_DomainMapFilter;
  LevelSetContainerType       m_Container;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLevelSetContainerBase.hxx"
#endif

#endif // itkLevelSetContainerBase_h
