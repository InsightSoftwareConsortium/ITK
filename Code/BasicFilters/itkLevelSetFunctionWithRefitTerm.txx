/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunctionWithRefitTerm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

     =========================================================================*/
#ifndef __itkLevelSetFunctionWithRefitTerm_txx_
#define __itkLevelSetFunctionWithRefitTerm_txx_

#include "itkLevelSetFunctionWithRefitTerm.h"
#include "itkSparseImage.h"
#include "itkNumericTraits.h"
#include "itkVector.h"

namespace itk {

template <class TImageType, class TSparseImageType> 
const unsigned long
LevelSetFunctionWithRefitTerm <TImageType, TSparseImageType>
::m_NumVertex = 1 << ImageDimension;

template <class TImageType, class TSparseImageType> 
const typename LevelSetFunctionWithRefitTerm <TImageType,
                                              TSparseImageType>::ScalarValueType
LevelSetFunctionWithRefitTerm <TImageType, TSparseImageType>
::m_DimConst = static_cast <ScalarValueType> (2.0/m_NumVertex);

template <class TImageType, class TSparseImageType>
LevelSetFunctionWithRefitTerm<TImageType, TSparseImageType>
::LevelSetFunctionWithRefitTerm()
{
  m_SparseTargetImage = SparseImageType::New();
  
  this->SetPropagationWeight (NumericTraits<ScalarValueType>::One);
  m_RefitWeight = NumericTraits<ScalarValueType>::One;
  m_OtherPropagationWeight = NumericTraits<ScalarValueType>::Zero;
  m_MinVectorNorm = static_cast<ScalarValueType> (1.0e-6);
}

template <class TImageType, class TSparseImageType>
void
LevelSetFunctionWithRefitTerm<TImageType, TSparseImageType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "RefitWeight: " << m_RefitWeight << std::endl;
  os << indent << "OtherPropagationWeight: "
     << m_OtherPropagationWeight << std::endl;
  os << indent << "MinVectorNorm: "<< m_MinVectorNorm << std::endl;
  os << indent << "DimConst: "<< m_DimConst << std::endl;
  os << indent << "NumVertex: "<< m_NumVertex << std::endl;
}

template <class TImageType, class TSparseImageType>
typename LevelSetFunctionWithRefitTerm<TImageType,
                                       TSparseImageType>::TimeStepType
LevelSetFunctionWithRefitTerm<TImageType, TSparseImageType>
::ComputeGlobalTimeStep(void *GlobalData) const
{
  TimeStepType dt = Superclass::ComputeGlobalTimeStep (GlobalData);
  dt = vnl_math_min ( dt, m_WaveDT );

  return dt;
}

template <class TImageType, class TSparseImageType>
typename LevelSetFunctionWithRefitTerm <TImageType,
                                        TSparseImageType>::ScalarValueType
LevelSetFunctionWithRefitTerm<TImageType, TSparseImageType>
::ComputeCurvature( const NeighborhoodType &neighborhood ) const
{
  unsigned int j, k;
  unsigned int counterN, counterP;
  unsigned long positionN,  positionP,
    stride[ImageDimension], indicator[ImageDimension];

  const unsigned long center = neighborhood.Size()/2;
  
  NormalVectorType normalvector;
  ScalarValueType curvature;

  for( j = 0; j < ImageDimension; j++ )
    {
    stride[j] = neighborhood.GetStride(j);
    indicator[j] = 1 << j;
    }
  curvature = NumericTraits<ScalarValueType>::Zero;
 
  for (counterN = 0; counterN < m_NumVertex; counterN++)
    {
    // compute position of normal vector
    positionN = center;
    for (k = 0; k < ImageDimension; k++)
      {
      if (counterN & indicator[k])
        {
        positionN -= stride[k];
        }
      }
    // compute the normal vector 
    for (j = 0; j < ImageDimension; j++) // derivative axis
      {
      normalvector[j] = NumericTraits<ScalarValueType>::Zero;
      for (counterP = 0; counterP < m_NumVertex; counterP++)
        {
        positionP = positionN;
        for (k = 0; k < ImageDimension; k++)
          {
          if (counterP & indicator[k])
            {
            positionP += stride[k];
            }
          }
        if ( counterP & indicator[j] ) 
          {
          normalvector[j] += neighborhood.GetPixel (positionP);
          }
        else
          {
          normalvector[j] -= neighborhood.GetPixel (positionP);
          }
        } // end counterP
      } // end derivative axis
    normalvector = normalvector / (m_MinVectorNorm + normalvector.GetNorm());
    // add normal to curvature computation
    for (j = 0; j < ImageDimension; j++) // derivative axis
      {
      if ( counterN & indicator[j] )
        {
        curvature -= normalvector[j];
        }
      else
        {
        curvature += normalvector[j];
        }
      } // end derivative axis
    } // end counterN
  
  curvature *= m_DimConst;

  return curvature;
}  


template <class TImageType, class TSparseImageType>
typename LevelSetFunctionWithRefitTerm <TImageType,
                                        TSparseImageType>::ScalarValueType
LevelSetFunctionWithRefitTerm<TImageType, TSparseImageType>
::PropagationSpeed(const NeighborhoodType &neighborhood,
                   const FloatOffsetType &offset,
                   GlobalDataStruct *globaldata) const
{
  IndexType idx = neighborhood.GetIndex();
  NodeType *targetnode = m_SparseTargetImage->GetPixel (idx);
  ScalarValueType refitterm, cv, tcv;
  if ((targetnode == 0)||(targetnode->m_CurvatureFlag == false))
    {
    if (targetnode == 0)
      {
      itkExceptionMacro( << "required node has null pointer\n");
      }
    else
      {
      itkExceptionMacro( << "required node has CurvatureFlag = false\n");
      }
    refitterm = NumericTraits<ScalarValueType>::Zero;
    }
  else
    {
    cv =  this->ComputeCurvature (neighborhood);
    tcv = targetnode->m_Curvature;
    refitterm = static_cast<ScalarValueType> (tcv - cv);
    }
  
  return m_RefitWeight*refitterm +
    m_OtherPropagationWeight*
    OtherPropagationSpeed (neighborhood, offset, globaldata);
}

} //end namespace itk

#endif
