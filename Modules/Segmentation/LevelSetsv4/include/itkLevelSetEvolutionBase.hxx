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


#ifndef __itkLevelSetEvolutionBase_hxx
#define __itkLevelSetEvolutionBase_hxx

#include "itkLevelSetEvolutionBase.h"

namespace itk
{
template< class TEquationContainer, class TLevelSet >
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::LevelSetEvolutionBase()
{
  this->m_Alpha = 0.9;
  this->m_Dt = 1.;
  this->m_RMSChangeAccumulator = 0.;
  this->m_UserGloballyDefinedTimeStep = false;
}

template< class TEquationContainer, class TLevelSet >
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::~LevelSetEvolutionBase()
{}

template< class TEquationContainer, class TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::SetTimeStep( const LevelSetOutputRealType& iDt )
{
  if( iDt > NumericTraits< LevelSetOutputRealType >::epsilon() )
    {
    this->m_UserGloballyDefinedTimeStep = true;
    this->m_Dt = iDt;
    this->Modified();
    }
  else
    {
    itkGenericExceptionMacro( <<"iDt should be > epsilon")
    }
}

template< class TEquationContainer, class TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::Update()
{
  this->CheckSetUp();

  this->InvokeEvent( StartEvent() );
  this->RunOneIteration();
  this->InvokeEvent( EndEvent() );
}

template< class TEquationContainer, class TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::CheckSetUp()
{
  if( this->m_LevelSetContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer is NULL" );
    }

  if( this->m_EquationContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer is NULL" );
    }

  typename EquationContainerType::Iterator eqIt = this->m_EquationContainer->Begin();

  if( eqIt == this->m_EquationContainer->End() )
    {
    itkGenericExceptionMacro( <<"this->m_EquationContainer is empty" );
    }
  if( !eqIt->GetEquation() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer->GetEquation( 0 ) is NULL" );
    }

  if( this->m_LevelSetContainer != this->m_EquationContainer->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != this->m_EquationContainer->GetLevelSetContainer()");
    }

  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  if( inputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "input Image is NULL" );
    }

  // Get the LevelSetContainer from the EquationContainer
  TermContainerPointer termContainer = eqIt->GetEquation();
  typename TermContainerType::Iterator termIt = termContainer->Begin();

  if( termIt == termContainer->End() )
    {
    itkGenericExceptionMacro( << "TermContainer is empty" );
    }

  if( this->m_LevelSetContainer != termContainer->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != termContainer->GetLevelSetContainer()" );
    }

  TermPointer term = termIt->GetTerm();

  if( this->m_LevelSetContainer != term->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != term->GetLevelSetContainer()" );
    }

  if( this->m_StoppingCriterion.IsNull() )
    {
    itkGenericExceptionMacro( << "m_StoppingCriterion is NULL" );
    }

  this->m_NumberOfIterations = 0;
}

template< class TEquationContainer, class TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::InitializeIteration()
{
  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  // Initialize parameters here
  this->m_EquationContainer->InitializeParameters();

  DomainMapImageFilterPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();
  if( !domainMapFilter.IsNull() && domainMapFilter->GetDomainMap().size() > 0 )
    {
    typedef typename DomainMapImageFilterType::DomainMapType DomainMapType;
    const DomainMapType domainMap = domainMapFilter->GetDomainMap();
    typename DomainMapType::const_iterator map_it   = domainMap.begin();
    typename DomainMapType::const_iterator map_end  = domainMap.end();

    while( map_it != map_end )
      {
      // Iterator over the region for the current levelset overlap identifier.
      ImageRegionConstIteratorWithIndex< InputImageType > it( inputImage, map_it->second.m_Region );
      it.GoToBegin();

      while( !it.IsAtEnd() )
        {
        IdListType lout = map_it->second.m_List;

        if( lout.empty() )
          {
          itkGenericExceptionMacro( <<"No level set exists at voxel" );
          }

        for( IdListIterator lIt = lout.begin(); lIt != lout.end(); ++lIt )
          {
          TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( *lIt - 1 );
          termContainer->Initialize( it.GetIndex() );
          }
          ++it;
        }
      ++map_it;
      }
    }
  else // assume there is one level set that covers the RequestedRegion of the InputImage
    {
    TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( 0 );
    ImageRegionConstIteratorWithIndex< InputImageType > it( inputImage, inputImage->GetRequestedRegion() );
    it.GoToBegin();
    while( !it.IsAtEnd() )
      {
      termContainer->Initialize( it.GetIndex() );
      ++it;
      }
    }

  this->m_EquationContainer->UpdateInternalEquationTerms();
}

}
#endif // __itkLevelSetEvolutionBase_hxx
