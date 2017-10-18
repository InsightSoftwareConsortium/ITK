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
template< typename TIdentifier, typename TLevelSet >
class ITK_TEMPLATE_EXPORT LevelSetContainerBase : public Object
{
public:
  typedef LevelSetContainerBase      Self;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef Object                     Superclass;

  /** Run-time type information */
  itkTypeMacro ( LevelSetContainerBase, Object );

  /** typedefs related to the type of level set*/
  typedef TLevelSet                               LevelSetType;
  typedef typename LevelSetType::Pointer          LevelSetPointer;
  typedef typename LevelSetType::InputType        InputIndexType;
  typedef typename LevelSetType::OutputType       OutputType;
  typedef typename LevelSetType::OutputRealType   OutputRealType;
  typedef typename LevelSetType::GradientType     GradientType;
  typedef typename LevelSetType::HessianType      HessianType;
  typedef typename LevelSetType::LevelSetDataType LevelSetDataType;

  /** IdentifierType */
  typedef TIdentifier LevelSetIdentifierType;

  typedef std::map< LevelSetIdentifierType, LevelSetPointer > LevelSetContainerType;
  typedef typename LevelSetContainerType::const_iterator      LevelSetContainerConstIteratorType;
  typedef typename LevelSetContainerType::iterator            LevelSetContainerIteratorType;

  typedef HeavisideStepFunctionBase< OutputRealType, OutputRealType > HeavisideType;
  typedef typename HeavisideType::ConstPointer                        HeavisideConstPointer;

  itkStaticConstMacro ( Dimension, unsigned int, LevelSetType::Dimension );

  typedef std::list< LevelSetIdentifierType >           IdListType;
  typedef typename IdListType::iterator                 IdListIterator;
  typedef typename IdListType::const_iterator           IdListConstIterator;
  typedef Image< IdListType, Dimension >                IdListImageType;
  typedef Image< short, Dimension >                     CacheImageType;

  typedef LevelSetDomainMapImageFilter< IdListImageType, CacheImageType > DomainMapImageFilterType;

  typedef std::pair< LevelSetIdentifierType, LevelSetPointer > LevelSetPairType;

  typedef typename DomainMapImageFilterType::Pointer          DomainMapImageFilterPointer;
  typedef typename DomainMapImageFilterType::LevelSetDomain   LevelSetDomainType;

  typedef std::map< LevelSetIdentifierType, LevelSetDomainType >   DomainContainerType;
  typedef typename DomainContainerType::iterator                   DomainIteratorType;

  /** Declare iterators to container. */
  class Iterator;
  friend class Iterator;

  /** \class ConstIterator
    \ingroup ITKLevelSetsv4
    */
  class ConstIterator
  {
  public:
    ConstIterator( ) {}
    ConstIterator( const LevelSetContainerConstIteratorType& it ) : m_Iterator( it ) {}
    ~ConstIterator() {}
    ConstIterator( const Iterator& it ) : m_Iterator( it.m_Iterator ) {}

    ConstIterator & operator * () { return *this; }
    ConstIterator * operator->()  { return this; }
    ConstIterator & operator++()
    {
      ++m_Iterator;
      return *this;
    }
    ConstIterator operator++(int)
    {
      ConstIterator tmp( *this );
      ++(*this);
      return tmp;
    }
    ConstIterator & operator--()
    {
      --m_Iterator;
      return *this;
    }
    ConstIterator operator--(int)
    {
      ConstIterator tmp( *this );
      --(*this);
      return tmp;
    }
    bool operator==(const Iterator& it) const
    {
      return ( m_Iterator == it.m_Iterator );
    }
    bool operator!=(const Iterator& it) const
    {
      return (m_Iterator != it.m_Iterator );
    }
    bool operator==(const ConstIterator& it) const
    {
      return ( m_Iterator == it.m_Iterator );
    }
    bool operator!=(const ConstIterator& it) const
    {
      return (m_Iterator != it.m_Iterator );
    }

    LevelSetIdentifierType GetIdentifier() const
    {
      return m_Iterator->first;
    }

    LevelSetType* GetLevelSet() const
    {
      return m_Iterator->second;
    }

  private:
    LevelSetContainerConstIteratorType m_Iterator;
    friend class Iterator;
    };

  /** \class Iterator
    \ingroup ITKLevelSetsv4 */
  class Iterator
    {
  public:
    Iterator( ) {}
    Iterator( const LevelSetContainerIteratorType& it ) : m_Iterator( it ) {}
    Iterator( const ConstIterator& it ) : m_Iterator( it.m_Iterator ) {}
    ~Iterator() {}

    Iterator & operator * () { return *this; }
    Iterator * operator->()  { return this; }
    Iterator & operator++()
    {
      ++m_Iterator;
      return *this;
    }
    Iterator operator++(int)
    {
      Iterator tmp( *this );
      ++(*this);
      return tmp;
    }
    Iterator & operator--()
    {
      --m_Iterator;
      return *this;
    }
    Iterator operator--(int)
    {
      Iterator tmp( *this );
      --(*this);
      return tmp;
    }
    bool operator==(const Iterator& it) const
    {
      return ( m_Iterator == it.m_Iterator );
    }
    bool operator!=(const Iterator& it) const
    {
      return (m_Iterator != it.m_Iterator );
    }
    bool operator==(const ConstIterator& it) const
    {
      return ( m_Iterator == it.m_Iterator );
    }
    bool operator!=(const ConstIterator& it) const
    {
      return (m_Iterator != it.m_Iterator );
    }

    LevelSetIdentifierType GetIdentifier() const
    {
      return m_Iterator->first;
    }

    LevelSetType* GetLevelSet() const
    {
      return m_Iterator->second;
    }

  private:
    LevelSetContainerIteratorType m_Iterator;
    friend class ConstIterator;
  };

  Iterator Begin();
  Iterator End();

  ConstIterator Begin() const;
  ConstIterator End() const;

  /** Get the number of LevelSets in the container. */
  LevelSetIdentifierType Size() const;

  /** \brief Get the level set function given its id
    \param[in] iId
    \return the level set function if it is in the container, else ITK_NULLPTR.
  */
  LevelSetPointer GetLevelSet( const LevelSetIdentifierType& iId ) const;

  /** \brief Add one level set function given its id.

    \param[in] iId id of the level set function
    \param[in] iLevelSet the level set function to be added
    \param[in] iForce if iForce is true (default) the level set function will be
    added to the container even if there is already one with the same id.

    \return true if the level set has been added.
  */
  bool AddLevelSet( const LevelSetIdentifierType& iId,
                    LevelSetType * iLevelSet,
                    const bool iForce = true );

  /** \brief Remove one level set function given its id.
    \param[in] iId id of the level set function to be removed
    \return true if it has been removed, false if the id was not present in the
    container.
  */
  bool RemoveLevelSet( const LevelSetIdentifierType& iId );

  /** \todo add documentation */
  itkSetConstObjectMacro( Heaviside, HeavisideType );
  itkGetConstObjectMacro(Heaviside, HeavisideType );

  /** Set/Get the domain map image filter. */
  itkSetObjectMacro( DomainMapFilter, DomainMapImageFilterType );
  itkGetModifiableObjectMacro(DomainMapFilter, DomainMapImageFilterType );

  /** Does the level set container have a domain map? */
  bool HasDomainMap() const;

protected:
  /** \brief Default Constructor */
  LevelSetContainerBase();

  /** \brief Default Destructor */
  ~LevelSetContainerBase() ITK_OVERRIDE;

  const LevelSetContainerType& GetContainer() const;
  void SetContainer( const LevelSetContainerType& iContainer );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetContainerBase);

  HeavisideConstPointer         m_Heaviside;
  DomainMapImageFilterPointer   m_DomainMapFilter;
  LevelSetContainerType         m_Container;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetContainerBase.hxx"
#endif

#endif // itkLevelSetContainerBase_h
