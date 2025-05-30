/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkNormalVectorDiffusionFunction_hxx
#define itkNormalVectorDiffusionFunction_hxx

#include "itkVector.h"

namespace itk
{
template <typename TSparseImageType>
NormalVectorDiffusionFunction<TSparseImageType>::NormalVectorDiffusionFunction()
{
  // check: should some of this be in Initialize?
  RadiusType r;

  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    r[j] = 1;
  }

  this->SetRadius(r);
  this->SetTimeStep(static_cast<TimeStepType>(0.5 / ImageDimension));
  m_NormalProcessType = 0;
  m_ConductanceParameter = NodeValueType{};
  m_FluxStopConstant = NodeValueType{};
}

template <typename TSparseImageType>
void
NormalVectorDiffusionFunction<TSparseImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NormalProcessType: " << m_NormalProcessType << std::endl;
  os << indent << "ConductanceParameter: " << m_ConductanceParameter << std::endl;
  os << indent << "FluxStopConstant: " << m_FluxStopConstant << std::endl;
}

template <typename TSparseImageType>
void
NormalVectorDiffusionFunction<TSparseImageType>::PrecomputeSparseUpdate(NeighborhoodType & it) const
{
  NodeType *             CenterNode = it.GetCenterPixel();
  const NormalVectorType CenterPixel = CenterNode->m_Data;

  const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();
  SizeValueType                stride[ImageDimension];
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    stride[j] = it.GetStride(j);
  }
  const auto center = it.Size() / 2;

  NormalVectorType                      flux;
  Vector<NodeValueType, ImageDimension> gradient[ImageDimension];
  NormalVectorType                      PositiveSidePixel[2];
  NormalVectorType                      NegativeSidePixel[2];
  for (unsigned int i = 0; i < ImageDimension; ++i) // flux offset axis
  {
    const auto PreviousNode = it.GetPrevious(i);
    if (PreviousNode == nullptr)
    {
      for (unsigned int j = 0; j < ImageDimension; ++j)
      {
        CenterNode->m_Flux[i][j] = NodeValueType{};
      }
    }
    else
    {
      const auto PreviousPixel = PreviousNode->m_Data;
      for (unsigned int j = 0; j < ImageDimension; ++j) // derivative axis
      {
        if (i != j) // compute derivative on a plane
        {
          // compute differences (j-axis) in line with center pixel
          auto OtherNode = it.GetPrevious(j);
          if (OtherNode == nullptr)
          {
            NegativeSidePixel[0] = CenterPixel;
          }
          else
          {
            NegativeSidePixel[0] = OtherNode->m_Data;
          }
          OtherNode = it.GetNext(j);
          if (OtherNode == nullptr)
          {
            PositiveSidePixel[0] = CenterPixel;
          }
          else
          {
            PositiveSidePixel[0] = OtherNode->m_Data;
          }

          // compute derivative (j-axis) offset from center pixel on i-axis
          OtherNode = it.GetPixel(center - stride[i] - stride[j]);
          if (OtherNode == nullptr)
          {
            NegativeSidePixel[1] = PreviousPixel;
          }
          else
          {
            NegativeSidePixel[1] = OtherNode->m_Data;
          }
          OtherNode = it.GetPixel(center - stride[i] + stride[j]);
          if (OtherNode == nullptr)
          {
            PositiveSidePixel[1] = PreviousPixel;
          }
          else
          {
            PositiveSidePixel[1] = OtherNode->m_Data;
          }

          gradient[j] =
            ((PositiveSidePixel[0] + PositiveSidePixel[1]) - (NegativeSidePixel[0] + NegativeSidePixel[1])) *
            static_cast<NodeValueType>(0.25) * neighborhoodScales[j];
        }
        else // compute derivative on a line
        {
          gradient[i] = (CenterPixel - PreviousPixel) * neighborhoodScales[i];
        }
      } // end derivative axis

      // now compute the intrinsic derivative
      for (unsigned int j = 0; j < ImageDimension; ++j) // component axis
      {
        NodeValueType DotProduct{};
        for (unsigned int k = 0; k < ImageDimension; ++k) // derivative axis
        {
          DotProduct += (gradient[k][j] * CenterNode->m_ManifoldNormal[i][k]);
        }
        flux[j] = gradient[i][j] - CenterNode->m_ManifoldNormal[i][i] * DotProduct;
      }
      // do following line for non-intrinsic derivative
      // flux = gradient[i];
      if (m_NormalProcessType == 1)
      {
        // anisotropic diffusion
        CenterNode->m_Flux[i] = flux * this->FluxStopFunction(flux.GetSquaredNorm());
      }
      else
      {
        // isotropic diffusion
        CenterNode->m_Flux[i] = flux;
      }
    } // end if-else PreviousNode==0
  } // end flux offset axis
}

template <typename TSparseImageType>
auto
NormalVectorDiffusionFunction<TSparseImageType>::ComputeSparseUpdate(NeighborhoodType & it,
                                                                     void *,
                                                                     const FloatOffsetType &) const -> NormalVectorType
{
  const NodeType *       CenterNode = it.GetCenterPixel();
  const NormalVectorType CenterPixel = CenterNode->m_Data;

  const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();

  NormalVectorType change{};
  for (unsigned int i = 0; i < ImageDimension; ++i) // flux offset axis
  {
    const auto NextNode = it.GetNext(i);
    if (NextNode == nullptr)
    {
      change -= CenterNode->m_Flux[i] * neighborhoodScales[i];
    }
    else
    {
      change += (NextNode->m_Flux[i] - CenterNode->m_Flux[i]) * neighborhoodScales[i];
    }
  } // end flux offset axis
  const auto DotProduct = change * CenterPixel;
  change -= CenterPixel * DotProduct;

  return change;
}
} // end namespace itk

#endif
