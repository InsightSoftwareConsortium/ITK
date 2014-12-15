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
#ifndef itkLevelSetEquationRegionTerm_h
#define itkLevelSetEquationRegionTerm_h

#include "itkLevelSetEquationTermBase.h"

namespace itk
{
template< typename TInput,
          typename TLevelSetContainer >
class LevelSetEquationRegionTerm :
    public LevelSetEquationTermBase< TInput, TLevelSetContainer >
{
public:
  typedef LevelSetEquationRegionTerm                             Self;
  typedef SmartPointer< Self >                                   Pointer;
  typedef SmartPointer< const Self >                             ConstPointer;
  typedef LevelSetEquationTermBase< TInput, TLevelSetContainer > Superclass;

  typedef TInput                      InputType;
  typedef typename InputType::Pointer InputPointer;

  typedef TLevelSetContainer                           LevelSetContainerType;
  typedef typename LevelSetContainerType::Pointer      LevelSetContainerPointer;
  typedef typename LevelSetContainerType::OutputType   LevelSetOutputType;
  typedef typename LevelSetContainerType::InputType    LevelSetInputType;
  typedef typename LevelSetContainerType::GradientType GradientType;
  typedef typename LevelSetContainerType::HessianType  HessianType;

protected:
  LevelSetEquationRegionTerm() : Superclass()
  {}

  virtual ~LevelSetEquationRegionTerm() {}

private:
  LevelSetEquationRegionTerm( const Self& );
  void operator = ( const Self& );
};
}
#endif // itkLevelSetEquationRegionTerm_h
