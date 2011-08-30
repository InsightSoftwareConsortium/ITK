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

#ifndef __itkSparseLevelSetContainer_h
#define __itkSparseLevelSetContainer_h

#include "itkLevelSetContainerBase.h"

namespace itk
{
/**
 *  \class SparseLevelSetContainer
 *  \brief Container of level set
 *
 *  Encapsulate an ordered set of level set function (see LevelSetBase).
 *
 * \tparam TIdentifier type of the identifier used to reference on level set function
 * \tparam TLevelSet type of level set function in the container.
 * \ingroup ITKLevelSetsv4
 */
template< class TIdentifier, class TLevelSet >
class SparseLevelSetContainer :
   public LevelSetContainerBase< TIdentifier, TLevelSet >
{
public:
  typedef SparseLevelSetContainer                         Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef LevelSetContainerBase< TIdentifier, TLevelSet > Superclass;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  /** Run-time type information */
  itkTypeMacro ( SparseLevelSetContainer, LevelSetContainerBase );


protected:
  /** \brief Default Constructor */
  SparseLevelSetContainer();

  /** \brief Default Destructor */
  ~SparseLevelSetContainer();

private:
  SparseLevelSetContainer( const Self & ); // purposely not implemented
  void operator = ( const Self & ); // purposely not implemented
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseLevelSetContainer.hxx"
#endif

#endif // __itkSparseLevelSetContainer_h
