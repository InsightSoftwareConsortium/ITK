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

#ifndef itkLevelSetEquationChanAndVeseExternalTerm_hxx
#define itkLevelSetEquationChanAndVeseExternalTerm_hxx

#include "itkLevelSetEquationChanAndVeseExternalTerm.h"

namespace itk
{
template< typename TInput, typename TLevelSetContainer >
LevelSetEquationChanAndVeseExternalTerm< TInput, TLevelSetContainer >
::LevelSetEquationChanAndVeseExternalTerm()
{
  this->m_TermName = "External Chan And Vese term";
  this->m_RequiredData.insert( "Value" );
  this->m_DomainMapImageFilter = ITK_NULLPTR;
  this->m_CacheImage = ITK_NULLPTR;
}

template< typename TInput, typename TLevelSetContainer >
LevelSetEquationChanAndVeseExternalTerm< TInput, TLevelSetContainer >
::~LevelSetEquationChanAndVeseExternalTerm()
{
}


template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseExternalTerm< TInput, TLevelSetContainer >
::ComputeProduct( const LevelSetInputIndexType& iP, LevelSetOutputRealType& prod )
{
  this->ComputeProductTerm( iP, prod );
  LevelSetPointer levelSet = this->m_LevelSetContainer->GetLevelSet( this->m_CurrentLevelSetId );
  LevelSetOutputRealType value = levelSet->Evaluate( iP );
  prod *= -(1 - this->m_Heaviside->Evaluate( -value ) );
}

template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseExternalTerm< TInput, TLevelSetContainer >
::ComputeProductTerm( const LevelSetInputIndexType& iP, LevelSetOutputRealType& prod )
{
  prod = -1 * NumericTraits< LevelSetOutputRealType >::OneValue();

  if( this->m_LevelSetContainer->HasDomainMap() )
    {
    if(this->m_DomainMapImageFilter == ITK_NULLPTR)
      {
      this->m_DomainMapImageFilter = this->m_LevelSetContainer->GetModifiableDomainMapFilter();
      this->m_CacheImage = this->m_DomainMapImageFilter->GetOutput();
      }
    const LevelSetIdentifierType id = this->m_CacheImage->GetPixel( iP );

    typedef typename DomainMapImageFilterType::DomainMapType DomainMapType;
    const DomainMapType domainMap = this->m_DomainMapImageFilter->GetDomainMap();
    typename DomainMapType::const_iterator levelSetMapItr = domainMap.find(id);

    if( levelSetMapItr != domainMap.end() )
      {
      const IdListType * idList = levelSetMapItr->second.GetIdList();

      LevelSetIdentifierType kk;
      LevelSetPointer levelSet;
      LevelSetOutputRealType value;

      IdListConstIterator idListIt = idList->begin();
      while( idListIt != idList->end() )
        {
        //! \todo Fix me for string identifiers
        kk = *idListIt - 1;
        if( kk != this->m_CurrentLevelSetId )
          {
          levelSet = this->m_LevelSetContainer->GetLevelSet( kk );
          value = levelSet->Evaluate( iP );
          prod *= ( NumericTraits< LevelSetOutputRealType >::OneValue() - this->m_Heaviside->Evaluate( -value ) );
          }
        ++idListIt;
        }
      }
    }
  else
    {
    LevelSetIdentifierType kk;
    LevelSetPointer levelSet;
    LevelSetOutputRealType value;

    typename LevelSetContainerType::Iterator lsIt = this->m_LevelSetContainer->Begin();

    while( lsIt != this->m_LevelSetContainer->End() )
      {
      kk = lsIt->GetIdentifier();
      if( kk != this->m_CurrentLevelSetId )
        {
        levelSet = this->m_LevelSetContainer->GetLevelSet( kk );
        value = levelSet->Evaluate( iP );
        prod *= ( NumericTraits< LevelSetOutputRealType >::OneValue() - this->m_Heaviside->Evaluate( -value ) );
        }
      ++lsIt;
      }
    }
}

template< typename TInput, typename TLevelSetContainer >
void LevelSetEquationChanAndVeseExternalTerm< TInput, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& iP, const LevelSetOutputRealType & oldValue,
             const LevelSetOutputRealType & newValue )
{
  // Compute the product factor
  LevelSetOutputRealType prod;

  this->ComputeProductTerm( iP, prod );

  // For each affected h val: h val = new hval (this will dirty some cvals)
  InputPixelType input = this->m_Input->GetPixel( iP );

  const LevelSetOutputRealType oldH = this->m_Heaviside->Evaluate( -oldValue );
  const LevelSetOutputRealType newH = this->m_Heaviside->Evaluate( -newValue );
  const LevelSetOutputRealType change = oldH - newH;//(1 - newH) - (1 - oldH);

  // Determine the change in the product factor
  const LevelSetOutputRealType productChange = -( prod * change );

  this->m_TotalH += change;
  this->m_TotalValue += input * productChange;
}

}

#endif
