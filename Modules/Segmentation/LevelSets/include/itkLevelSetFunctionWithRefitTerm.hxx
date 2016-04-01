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
#ifndef itkLevelSetFunctionWithRefitTerm_hxx
#define itkLevelSetFunctionWithRefitTerm_hxx

#include "itkLevelSetFunctionWithRefitTerm.h"
#include "itkVector.h"

namespace itk
{
template< typename TImageType, typename TSparseImageType >
const typename LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >::NeighborhoodSizeValueType
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::m_NumVertex = 1 << TImageType::ImageDimension;

template< typename TImageType, typename TSparseImageType >
const typename LevelSetFunctionWithRefitTerm< TImageType,
                                              TSparseImageType >::ScalarValueType
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::m_DimConst = static_cast< ScalarValueType >( 2.0 / m_NumVertex );

template< typename TImageType, typename TSparseImageType >
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::LevelSetFunctionWithRefitTerm()
{
  m_SparseTargetImage = SparseImageType::New();

  this->SetPropagationWeight (NumericTraits< ScalarValueType >::OneValue());
  m_RefitWeight = NumericTraits< ScalarValueType >::OneValue();
  m_OtherPropagationWeight = NumericTraits< ScalarValueType >::ZeroValue();
  m_MinVectorNorm = static_cast< ScalarValueType >( 1.0e-6 );
}

template< typename TImageType, typename TSparseImageType >
void
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "RefitWeight: " << m_RefitWeight << std::endl;
  os << indent << "OtherPropagationWeight: "
     << m_OtherPropagationWeight << std::endl;
  os << indent << "MinVectorNorm: " << m_MinVectorNorm << std::endl;
  os << indent << "DimConst: " << m_DimConst << std::endl;
  os << indent << "NumVertex: " << m_NumVertex << std::endl;
}

template< typename TImageType, typename TSparseImageType >
typename LevelSetFunctionWithRefitTerm< TImageType,
                                        TSparseImageType >::TimeStepType
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::ComputeGlobalTimeStep(void *GlobalData) const
{
  TimeStepType dt = Superclass::ComputeGlobalTimeStep (GlobalData);

  dt = std::min (dt, this->m_WaveDT);

  return dt;
}

template< typename TImageType, typename TSparseImageType >
typename LevelSetFunctionWithRefitTerm< TImageType,
                                        TSparseImageType >::ScalarValueType
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::ComputeCurvature(const NeighborhoodType & neighborhood) const
{
  unsigned int  j, k;
  unsigned int  counterN, counterP;
  NeighborhoodSizeValueType positionN,  positionP,
                stride[TImageType::ImageDimension], indicator[TImageType::ImageDimension];

  const NeighborhoodSizeValueType one = 1;
  const NeighborhoodSizeValueType center = neighborhood.Size() / 2;

  const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();

  NormalVectorType normalvector;
  ScalarValueType  curvature;

  for ( j = 0; j < TImageType::ImageDimension; j++ )
    {
    stride[j] = neighborhood.GetStride(j);
    indicator[j] = one << j;
    }
  curvature = NumericTraits< ScalarValueType >::ZeroValue();

  for ( counterN = 0; counterN < m_NumVertex; counterN++ )
    {
    // compute position of normal vector
    positionN = center;
    for ( k = 0; k < TImageType::ImageDimension; k++ )
      {
      if ( counterN & indicator[k] )
        {
        positionN -= stride[k];
        }
      }
    // compute the normal vector
    for ( j = 0; j < TImageType::ImageDimension; j++ ) // derivative axis
      {
      normalvector[j] = NumericTraits< ScalarValueType >::ZeroValue();
      for ( counterP = 0; counterP < m_NumVertex; counterP++ )
        {
        positionP = positionN;
        for ( k = 0; k < TImageType::ImageDimension; k++ )
          {
          if ( counterP & indicator[k] )
            {
            positionP += stride[k];
            }
          }
        if ( counterP & indicator[j] )
          {
          normalvector[j] += neighborhood.GetPixel (positionP) * neighborhoodScales[j];
          }
        else
          {
          normalvector[j] -= neighborhood.GetPixel (positionP) * neighborhoodScales[j];
          }
        } // end counterP
      }   // end derivative axis
    normalvector = normalvector / ( m_MinVectorNorm + normalvector.GetNorm() );
    // add normal to curvature computation
    for ( j = 0; j < TImageType::ImageDimension; j++ ) // derivative axis
      {
      if ( counterN & indicator[j] )
        {
        curvature -= normalvector[j] * neighborhoodScales[j];
        }
      else
        {
        curvature += normalvector[j] * neighborhoodScales[j];
        }
      } // end derivative axis
    }   // end counterN

  curvature *= m_DimConst;

  return curvature;
}

template< typename TImageType, typename TSparseImageType >
typename LevelSetFunctionWithRefitTerm< TImageType,
                                        TSparseImageType >::ScalarValueType
LevelSetFunctionWithRefitTerm< TImageType, TSparseImageType >
::PropagationSpeed(const NeighborhoodType & neighborhood,
                   const FloatOffsetType & offset,
                   GlobalDataStruct *globaldata) const
{
  IndexType       idx = neighborhood.GetIndex();
  NodeType *      targetnode = m_SparseTargetImage->GetPixel (idx);
  ScalarValueType refitterm, cv, tcv;

  if ( ( targetnode == ITK_NULLPTR ) || ( targetnode->m_CurvatureFlag == false ) )
    {
    if ( targetnode == ITK_NULLPTR )
      {
      itkExceptionMacro(<< "required node has null pointer\n");
      }
    else
      {
      itkExceptionMacro(<< "required node has CurvatureFlag = false\n");
      }
    refitterm = NumericTraits< ScalarValueType >::ZeroValue();
    }
  else
    {
    cv =  this->ComputeCurvature (neighborhood);
    tcv = targetnode->m_Curvature;
    refitterm = static_cast< ScalarValueType >( tcv - cv );
    }

  return m_RefitWeight * refitterm
         + m_OtherPropagationWeight *
         OtherPropagationSpeed (neighborhood, offset, globaldata);
}
} //end namespace itk

#endif
