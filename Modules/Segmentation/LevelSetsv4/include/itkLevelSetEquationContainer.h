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

#ifndef itkLevelSetEquationContainer_h
#define itkLevelSetEquationContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 *  \class LevelSetEquationContainer
 *  \brief Class for holding a set of level set equations (PDEs).
 *
 *  \tparam TTermContainer Container holding the terms in a level set equation
 *
 *  Evolving level-set functions \f$\left\{ \phi_j \right\}_{j=1}^{M}\f$
 *  can be expressed as follows:
 *  \f{eqnarray*}{
 *  \frac{\partial \phi_1(p)}{\partial \tau} &=& \sum\limits_{i=1}^{N_1} \alpha_{i1}
 *  \cdot \omega_{i1}(p) \\
 *  \frac{\partial \phi_2(p)}{\partial \tau} &=& \sum\limits_{i=1}^{N_2} \alpha_{i2}
 *  \cdot \omega_{i2}(p) \\
 *  & \vdots & \\
 *  \frac{\partial \phi_M(p)}{\partial \tau} &=& \sum\limits_{i=1}^{N_M} \alpha_{iM}
 *  \cdot \omega_{iM}(p)
 *  \f}
 *  where \f$\omega_{iM}\f$ is a term which could depend on any of the level-set
 *  functions \f$\left\{ \phi_j \right\}_{j=1}^{M}\f$ , the input image,
 *  and \f$\alpha_{iM}\f$ is a weight to balance the contribution of each term
 *  in the PDE.
 *
 *  Each equation of this system of equation (PDE) is referred as an equation in
 *  the level-set framework. Each equation \f$ equation_{j} \f$ contributes to the
 *  evolution of the level-set function \f$ \phi_j \f$.
 *
 *  \sa LevelSetEquationTerm
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TTermContainer >
class ITK_TEMPLATE_EXPORT LevelSetEquationContainer : public Object
{
public:
  typedef LevelSetEquationContainer     Self;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;
  typedef Object                        Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationContainer, Object );

  typedef TTermContainer                            TermContainerType;
  typedef typename TermContainerType::Pointer       TermContainerPointer;

  typedef typename TermContainerType::InputImageType    InputImageType;
  typedef typename TermContainerType::InputImagePointer InputImagePointer;

  typedef typename TermContainerType::LevelSetOutputRealType  LevelSetOutputRealType;
  typedef typename TermContainerType::LevelSetInputIndexType  LevelSetInputIndexType;

  typedef typename TermContainerType::LevelSetIdentifierType    LevelSetIdentifierType;
  typedef typename TermContainerType::LevelSetContainerType     LevelSetContainerType;
  typedef typename TermContainerType::LevelSetContainerPointer  LevelSetContainerPointer;

  /** Add a equation to the system of equations in the EquationContainer map */
  void AddEquation( const LevelSetIdentifierType& iId, TermContainerType * iEquation );

  /** Return a pointer to the equation of given id */
  TermContainerType * GetEquation( const LevelSetIdentifierType& iId ) const;

  /** Update the equation container recursively by calling update on individual equations */
  void UpdateInternalEquationTerms();

  /** Supply the update at a given pixel index to update the terms */
  void UpdatePixel( const LevelSetInputIndexType & iP,
                    const LevelSetOutputRealType & oldValue,
                    const LevelSetOutputRealType & newValue );

  /** Initialize parameters in the terms of all the equations */
  void InitializeParameters();

  /** Returns the Courant-Friedrichs-Lewy (CFL) contribution
   * for all the equations */
  LevelSetOutputRealType ComputeCFLContribution() const;

  /** Set/Get the input speed or feature image */
  itkSetObjectMacro( Input, InputImageType );
  itkGetModifiableObjectMacro(Input, InputImageType );

  itkSetObjectMacro( LevelSetContainer, LevelSetContainerType );
  itkGetModifiableObjectMacro(LevelSetContainer, LevelSetContainerType );

protected:
  typedef std::map< LevelSetIdentifierType, TermContainerPointer >  MapContainerType;
  typedef typename MapContainerType::iterator                       MapContainerIterator;
  typedef typename MapContainerType::const_iterator                 MapContainerConstIterator;

public:
  class Iterator;
  friend class Iterator;

  class ConstIterator
  {
  public:
    ConstIterator() {}
    ConstIterator( const MapContainerConstIterator& it ) : m_Iterator( it ) {}
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
    LevelSetIdentifierType GetIdentifier() const
      {
      return m_Iterator->first;
      }

    TermContainerType * GetEquation() const
      {
      return m_Iterator->second;
      }
  private:
    MapContainerConstIterator m_Iterator;
    friend class Iterator;
  };

  class Iterator
  {
  public:
    Iterator() {}
    Iterator( const MapContainerIterator& it ) : m_Iterator( it ) {}
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
    LevelSetIdentifierType GetIdentifier() const
      {
      return m_Iterator->first;
      }

    TermContainerType * GetEquation() const
      {
      return m_Iterator->second;
      }
  private:
    MapContainerIterator m_Iterator;
    friend class ConstIterator;
  };

  Iterator Begin();
  Iterator End();

  ConstIterator Begin() const;
  ConstIterator End() const;

protected:

  LevelSetEquationContainer();
  virtual ~LevelSetEquationContainer() ITK_OVERRIDE;

  LevelSetContainerPointer  m_LevelSetContainer;
  MapContainerType          m_Container;
  InputImagePointer         m_Input;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetEquationContainer);

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationContainer.hxx"
#endif

#endif // itkLevelSetEquationContainer_h
