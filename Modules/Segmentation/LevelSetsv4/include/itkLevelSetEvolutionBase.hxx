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


#ifndef itkLevelSetEvolutionBase_hxx
#define itkLevelSetEvolutionBase_hxx

#include "itkLevelSetEvolutionBase.h"

namespace itk
{
template< typename TEquationContainer, typename TLevelSet >
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::LevelSetEvolutionBase()
{
  this->m_Alpha = 0.9;
  this->m_Dt = 1.;
  this->m_RMSChangeAccumulator = 0.;
  this->m_UserGloballyDefinedTimeStep = false;
  this->m_NumberOfIterations = 0;
}

template< typename TEquationContainer, typename TLevelSet >
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::~LevelSetEvolutionBase()
{}

template< typename TEquationContainer, typename TLevelSet >
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

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::Update()
{
  this->CheckSetUp();

  this->InvokeEvent( StartEvent() );
  this->Evolve();
  this->InvokeEvent( EndEvent() );
}

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::CheckSetUp()
{
  if( this->m_LevelSetContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer is ITK_NULLPTR" );
    }

  if( this->m_EquationContainer.IsNull() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer is ITK_NULLPTR" );
    }

  typename EquationContainerType::Iterator eqIt = this->m_EquationContainer->Begin();

  if( eqIt == this->m_EquationContainer->End() )
    {
    itkGenericExceptionMacro( <<"this->m_EquationContainer is empty" );
    }
  if( !eqIt->GetEquation() )
    {
    itkGenericExceptionMacro( << "m_EquationContainer->GetEquation( 0 ) is ITK_NULLPTR" );
    }

  if( this->m_LevelSetContainer != this->m_EquationContainer->GetLevelSetContainer() )
    {
    itkGenericExceptionMacro( << "this->m_LevelSetContainer != this->m_EquationContainer->GetLevelSetContainer()" << std::endl
                              << this->m_LevelSetContainer.GetPointer() << " != " << this->m_EquationContainer->GetLevelSetContainer()
                              << std::endl
      );
    }

  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  if( inputImage.IsNull() )
    {
    itkGenericExceptionMacro( << "input Image is ITK_NULLPTR" );
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
    itkGenericExceptionMacro( << "m_StoppingCriterion is ITK_NULLPTR" );
    }

  this->m_NumberOfIterations = 0;
}

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::InitializeIteration()
{
  // Get the image to be segmented
  InputImageConstPointer inputImage = this->m_EquationContainer->GetInput();

  // Initialize parameters here
  this->m_EquationContainer->InitializeParameters();

  if( this->m_LevelSetContainer->HasDomainMap() )
    {
    typename DomainMapImageFilterType::ConstPointer domainMapFilter = this->m_LevelSetContainer->GetDomainMapFilter();
    typedef typename DomainMapImageFilterType::DomainMapType DomainMapType;
    const DomainMapType domainMap = domainMapFilter->GetDomainMap();
    typename DomainMapType::const_iterator mapIt   = domainMap.begin();
    typename DomainMapType::const_iterator mapEnd  = domainMap.end();

    while( mapIt != mapEnd )
      {
      // Iterator over the region for the current levelset overlap identifier.
      typedef typename DomainMapImageFilterType::LevelSetDomain LevelSetListImageDomainType;
      const LevelSetListImageDomainType & levelSetListImageDomain = mapIt->second;
      ImageRegionConstIteratorWithIndex< InputImageType > it( inputImage, *(levelSetListImageDomain.GetRegion()) );
      it.GoToBegin();

      while( !it.IsAtEnd() )
        {
        const IdListType * idList = levelSetListImageDomain.GetIdList();

        if( idList->empty() )
          {
          itkGenericExceptionMacro( <<"No level set exists at voxel" );
          }

        IdListConstIterator idListIt = idList->begin();
        while( idListIt != idList->end() )
          {
          //! \todo Fix me for string identifiers
          TermContainerPointer termContainer = this->m_EquationContainer->GetEquation( *idListIt - 1 );
          termContainer->Initialize( it.GetIndex() );
          ++idListIt;
          }
        ++it;
        }
      ++mapIt;
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

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::Evolve()
{
  this->AllocateUpdateBuffer();

  this->InitializeIteration();

  typename StoppingCriterionType::IterationIdType iter = 0;
  this->m_StoppingCriterion->SetCurrentIteration( iter );
  this->m_StoppingCriterion->SetLevelSetContainer( this->m_LevelSetContainer );

  // Trigger visualization classes to show initial level-set
  this->InvokeEvent( IterationEvent() );

  while( !this->m_StoppingCriterion->IsSatisfied() )
    {
    this->m_RMSChangeAccumulator = NumericTraits< LevelSetOutputRealType >::ZeroValue();

    // one iteration over all container
    // update each level set based on the different equations provided
    // Input image domain
    this->ComputeIteration();

    this->ComputeTimeStepForNextIteration();

    this->UpdateLevelSets();

    this->UpdateEquations();

    ++iter;

    this->m_StoppingCriterion->SetRMSChangeAccumulator( this->m_RMSChangeAccumulator );
    this->m_StoppingCriterion->SetCurrentIteration( iter );

    ++this->m_NumberOfIterations;

    // Trigger visualization classes to show updated level-set
    this->InvokeEvent( IterationEvent() );
    }
}

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::AllocateUpdateBuffer()
{
}

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::ComputeIteration()
{
}

template< typename TEquationContainer, typename TLevelSet >
void
LevelSetEvolutionBase< TEquationContainer, TLevelSet >
::ComputeTimeStepForNextIteration()
{
}

}
#endif // itkLevelSetEvolutionBase_hxx
