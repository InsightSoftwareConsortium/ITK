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

#ifndef __itkLevelSetEquationContainerBase_h
#define __itkLevelSetEquationContainerBase_h

#include "itkObject.h"

namespace itk
{
/**
 *  \class LevelSetEquationContainerBase
 *  \brief Class for holding a set of level set equation terms.
 *
 *  \todo Add full documentation
 *
 *  \tparam TTermContainer Container holding the terms in a level set equation
 *  \ingroup ITKLevelSetsv4
 */
template< class TTermContainer >
class LevelSetEquationContainerBase : public Object
{
public:
  typedef LevelSetEquationContainerBase Self;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;
  typedef Object                        Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( LevelSetEquationContainerBase, Object );

  typedef TTermContainer                            TermContainerType;
  typedef typename TermContainerType::Pointer       TermContainerPointer;

  typedef typename TermContainerType::InputImageType    InputImageType;
  typedef typename TermContainerType::InputImagePointer InputImagePointer;

  typedef typename TermContainerType::LevelSetOutputRealType  LevelSetOutputRealType;
  typedef typename TermContainerType::LevelSetInputIndexType  LevelSetInputIndexType;

  typedef typename TermContainerType::LevelSetIdentifierType    LevelSetIdentifierType;
  typedef typename TermContainerType::LevelSetContainerType     LevelSetContainerType;
  typedef typename TermContainerType::LevelSetContainerPointer  LevelSetContainerPointer;

  /** Return a pointer to the level set container */
  LevelSetContainerType * GetLevelSetContainer() const;

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
  itkGetConstObjectMacro( Input, InputImageType );

protected:

  LevelSetEquationContainerBase();
  virtual ~LevelSetEquationContainerBase();

  typedef std::map< LevelSetIdentifierType, TermContainerPointer >  MapContainerType;
  typedef typename MapContainerType::iterator                       MapContainerIterator;
  typedef typename MapContainerType::const_iterator                 MapContainerConstIterator;

  MapContainerType  m_Container;
  InputImagePointer m_Input;


private:
  LevelSetEquationContainerBase( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquationContainerBase.hxx"
#endif

#endif // __itkLevelSetEquationContainerBase_h
