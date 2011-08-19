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

#ifndef __itkLevelSetEquationCurvatureTerm_hxx
#define __itkLevelSetEquationCurvatureTerm_hxx

#include "itkLevelSetEquationCurvatureTerm.h"

namespace itk
{
template< class TInput, class TLevelSetContainer >
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::LevelSetEquationCurvatureTerm()
{
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_NeighborhoodScales[i] = 1.0;
    }
  this->m_TermName = "Curvature term";
  this->m_RequiredData.insert( "MeanCurvature" );
}

template< class TInput, class TLevelSetContainer >
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::~LevelSetEquationCurvatureTerm()
{
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP, const LevelSetDataType& iData )
{
  return iData.MeanCurvature.m_Value;
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::InitializeParameters()
{
  this->SetUp();
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Initialize( const LevelSetInputIndexType& )
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Update()
{
}

template< class TInput, class TLevelSetContainer >
void
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::UpdatePixel( const LevelSetInputIndexType& itkNotUsed( iP ),
               const LevelSetOutputRealType& itkNotUsed( oldValue ),
               const LevelSetOutputRealType& itkNotUsed( newValue ) )
{
}

template< class TInput, class TLevelSetContainer >
typename LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >::LevelSetOutputRealType
LevelSetEquationCurvatureTerm< TInput, TLevelSetContainer >
::Value( const LevelSetInputIndexType& iP )
{
  //
  // FIXME TODO NEEDS A SECOND PASS OF REVIEW
  //
  if( this->m_Heaviside.IsNotNull() )
    {
    LevelSetOutputRealType center_value =
        static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( iP ) );

    LevelSetInputIndexType pA, pB;
    LevelSetInputIndexType pAa, pBa, pCa, pDa;
    LevelSetOutputRealType valueA, valueB;
    LevelSetOutputRealType valueAa, valueBa, valueCa, valueDa;
    LevelSetOutputRealType oValue = NumericTraits< LevelSetOutputRealType >::Zero;

    vnl_matrix_fixed< LevelSetOutputRealType,
                    itkGetStaticConstMacro(ImageDimension),
                    itkGetStaticConstMacro(ImageDimension) > m_dxy;

    /** Array of first derivatives */
    LevelSetOutputRealType m_dx[itkGetStaticConstMacro(ImageDimension)];
    LevelSetOutputRealType m_GradMagSqr = vnl_math::eps;

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      pA = pB = iP;
      pA[i] += 1;
      pB[i] -= 1;

      valueA = static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( pA ) );
      valueB = static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( pB ) );

      m_dx[i] = 0.5 * ( valueA - valueB ) * m_NeighborhoodScales[i];
      m_dxy[i][i] = ( valueA + valueB - 2.0 * center_value ) * vnl_math_sqr(m_NeighborhoodScales[i]);
      m_GradMagSqr += m_dx[i] * m_dx[i];

      for ( unsigned int j = i + 1; j < ImageDimension; j++ )
        {
        pAa = pB;
        pAa[j] -= 1;

        pBa = pB;
        pBa[j] += 1;

        pCa = pA;
        pCa[j] -= 1;

        pDa = pA;
        pDa[j] += 1;

        valueAa = static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( pAa ) );
        valueBa = static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( pBa ) );
        valueCa = static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( pCa ) );
        valueDa = static_cast< LevelSetOutputRealType >( this->m_CurrentLevelSetPointer->Evaluate( pDa ) );

        m_dxy[i][j] = m_dxy[j][i] = 0.25 * ( valueAa - valueBa - valueCa + valueDa )
                                            * m_NeighborhoodScales[i] * m_NeighborhoodScales[j];
        }
      }
    LevelSetOutputRealType m_GradMag = vcl_sqrt( m_GradMagSqr );

    // Compute curvature
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        if ( j != i )
          {
          oValue -= m_dx[i] * m_dx[j] * m_dxy[i][j];
          oValue += m_dxy[j][j] * m_dx[i] * m_dx[i];
          }
        }
      }

    if ( m_GradMag > vnl_math::eps )
      {
      oValue /= m_GradMag * m_GradMag * m_GradMag;
      }
    else
      {
      oValue /= 1 + m_GradMagSqr;
      }

    return oValue;
    }
  else
    {
    itkWarningMacro( << "m_Heaviside is NULL" );
    }

  return NumericTraits< LevelSetOutputPixelType >::Zero;
}

}
#endif
