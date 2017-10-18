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

#ifndef itkLevelSetEquationTermContainer_h
#define itkLevelSetEquationTermContainer_h

#include "itkLevelSetEquationTermBase.h"
#include "itkObject.h"

#include "itksys/hash_map.hxx"

#include <map>
#include <string>

namespace itk
{
/**
 *  \class LevelSetEquationTermContainer
 *  \brief Class for container holding the terms of a given level set update equation
 *
 *  \tparam TInputImage Input image or speed image or feature image for segmentation
 *  \tparam TLevelSetContainer Container holding the all the level set functions
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInputImage,
          typename TLevelSetContainer >
class ITK_TEMPLATE_EXPORT LevelSetEquationTermContainer : public Object
{
public:
  typedef LevelSetEquationTermContainer     Self;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;
  typedef Object                            Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationTermContainer,
                Object );

  typedef unsigned int                      TermIdType;

  typedef TInputImage                       InputImageType;
  typedef typename InputImageType::Pointer  InputImagePointer;

  typedef TLevelSetContainer                                LevelSetContainerType;
  typedef typename LevelSetContainerType::Pointer           LevelSetContainerPointer;

  typedef typename LevelSetContainerType::LevelSetType      LevelSetType;
  typedef typename LevelSetContainerType::LevelSetPointer   LevelSetPointer;

  typedef typename LevelSetContainerType::LevelSetIdentifierType
                                                            LevelSetIdentifierType;
  typedef typename LevelSetContainerType::OutputType        LevelSetOutputPixelType;
  typedef typename LevelSetContainerType::OutputRealType    LevelSetOutputRealType;
  typedef typename LevelSetContainerType::LevelSetDataType  LevelSetDataType;
  typedef typename LevelSetContainerType::InputIndexType    LevelSetInputIndexType;
  typedef typename LevelSetContainerType::GradientType      LevelSetGradientType;
  typedef typename LevelSetContainerType::HessianType       LevelSetHessianType;

  typedef LevelSetEquationTermBase< InputImageType, LevelSetContainerType >
                                                                       TermType;
  typedef typename TermType::Pointer                                   TermPointer;

  /** Set/Get the input image to be segmented. */
  itkSetObjectMacro( Input, InputImageType );
  itkGetModifiableObjectMacro(Input, InputImageType );

  itkSetMacro( CurrentLevelSetId, LevelSetIdentifierType );
  itkGetMacro( CurrentLevelSetId, LevelSetIdentifierType );

  itkSetObjectMacro( LevelSetContainer, LevelSetContainerType );
  itkGetModifiableObjectMacro(LevelSetContainer, LevelSetContainerType );

  /** Add a term to the end of the container  */
  void PushTerm( TermType* iTerm );

  /** Replace the pointer to the term with the given id */
  void AddTerm( const TermIdType& iId, TermType* iTerm );

  /** Get the term with the given id */
  TermType* GetTerm( const TermIdType& iId );

  /** Get the term with the given name */
  TermType* GetTerm( const std::string& iName );

  /** \todo  */
  void Initialize( const LevelSetInputIndexType& iP );

  /** Supply the update at a given pixel location to update the term parameters */
  void UpdatePixel( const LevelSetInputIndexType& iP,
                    const LevelSetOutputRealType & oldValue,
                    const LevelSetOutputRealType & newValue );

  /** Initialize the term parameters prior to the start of an iteration */
  void InitializeParameters();

  /** Evaluate the term at a given pixel location */
  LevelSetOutputRealType Evaluate( const LevelSetInputIndexType& iP );

  LevelSetOutputRealType Evaluate( const LevelSetInputIndexType& iP,
                                   const LevelSetDataType& iData );

  /** Update the term parameters at end of iteration */
  void Update();

  /** Return the CFL contribution of the current term */
  LevelSetOutputRealType ComputeCFLContribution() const;

  void ComputeRequiredData( const LevelSetInputIndexType& iP,
                            LevelSetDataType& ioData );

protected:

  typedef std::map< TermIdType, TermPointer >           MapTermContainerType;
  typedef typename MapTermContainerType::iterator       MapTermContainerIteratorType;
  typedef typename MapTermContainerType::const_iterator MapTermContainerConstIteratorType;

public:
  class Iterator;
  friend class Iterator;

  class ConstIterator
  {
  public:
    ConstIterator() {}
    ConstIterator( const MapTermContainerConstIteratorType& it ) : m_Iterator( it ) {}
    ~ConstIterator() {}
    ConstIterator( const Iterator& it ) : m_Iterator( it.m_Iterator ) {}
    ConstIterator & operator * () { return *this; }
    ConstIterator * operator->() { return this; }
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
    bool operator == (const Iterator& it) const
      {
      return (m_Iterator == it.m_Iterator);
      }
    bool operator != (const Iterator& it) const
      {
      return (m_Iterator != it.m_Iterator);
      }
    bool operator == (const ConstIterator& it) const
      {
      return (m_Iterator == it.m_Iterator);
      }
    bool operator != (const ConstIterator& it) const
      {
      return (m_Iterator != it.m_Iterator);
      }
    TermIdType GetIdentifier() const
      {
      return m_Iterator->first;
      }

    TermType * GetTerm() const
      {
      return m_Iterator->second;
      }
  private:
    MapTermContainerConstIteratorType m_Iterator;
    friend class Iterator;
  };

  class Iterator
  {
  public:
    Iterator() {}
    Iterator( const MapTermContainerIteratorType& it ) : m_Iterator( it ) {}
    Iterator( const ConstIterator& it ) : m_Iterator( it.m_Iterator ) {}
    ~Iterator() {}

    Iterator & operator * () { return *this; }
    Iterator * operator ->() { return this; }

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
      return (m_Iterator==it.m_Iterator);
      }
    bool operator!=(const Iterator& it) const
      {
      return (m_Iterator!=it.m_Iterator);
      }
    bool operator==(const ConstIterator& it)const
      {
      return (m_Iterator == it.m_Iterator);
      }
    bool operator!=(const ConstIterator& it)const
      {
      return (m_Iterator != it.m_Iterator);
      }
    TermIdType GetIdentifier() const
      {
      return m_Iterator->first;
      }

    TermType * GetTerm() const
      {
      return m_Iterator->second;
      }
  private:
    MapTermContainerIteratorType m_Iterator;
    friend class ConstIterator;
  };

   Iterator Begin();
   Iterator End();

  ConstIterator Begin() const;
  ConstIterator End() const;

protected:
  LevelSetEquationTermContainer();

  virtual ~LevelSetEquationTermContainer() ITK_OVERRIDE;

  LevelSetIdentifierType    m_CurrentLevelSetId;
  LevelSetContainerPointer  m_LevelSetContainer;

  InputImagePointer         m_Input;

  typedef itksys::hash_map< std::string, TermPointer > HashMapStringTermContainerType;

  HashMapStringTermContainerType m_NameContainer;

  typedef typename TermType::RequiredDataType RequiredDataType;
  RequiredDataType  m_RequiredData;

  MapTermContainerType  m_Container;

  typedef std::map< TermIdType, LevelSetOutputRealType >  MapCFLContainerType;
  typedef typename MapCFLContainerType::iterator          MapCFLContainerIterator;
  typedef typename MapCFLContainerType::const_iterator    MapCFLContainerConstIterator;

  MapCFLContainerType   m_TermContribution;

private:
  LevelSetEquationTermContainer( const Self& );
  void operator = ( const Self& );
};

}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationTermContainer.hxx"
#endif

#endif // itkLevelSetEquationTermContainer_h
