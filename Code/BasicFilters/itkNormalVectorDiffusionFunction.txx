#ifndef __itkNormalVectorDiffusionFunction_txx_
#define __itkNormalVectorDiffusionFunction_txx_

#include "itkNormalVectorDiffusionFunction.h"
#include "itkNumericTraits.h"
#include "itkVector.h"

namespace itk {

template <class TSparseImageType>
NormalVectorDiffusionFunction <TSparseImageType>
::NormalVectorDiffusionFunction()
{
  // check: should some of this be in Initialize?
  RadiusType r;
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    r[j] = 1;
    } 
  
  this->SetRadius(r);
  this->SetNormalProcessType(0);
  this->SetConductanceParameter(NumericTraits<NodeValueType>::Zero);
  this->SetTimeStep(static_cast<TimeStepType> (0.5/ImageDimension));
}

template <class TSparseImageType>
void
NormalVectorDiffusionFunction <TSparseImageType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NormalProcessType: " << m_NormalProcessType << std::endl;
  os << indent << "ConductanceParameter: "<< m_ConductanceParameter << std::endl;
  os << indent << "FluxStopConstant: "<< m_FluxStopConstant << std::endl;
}

template <class TSparseImageType>
void
NormalVectorDiffusionFunction <TSparseImageType>
::PrecomputeSparseUpdate (NeighborhoodType &it) const 
{
  int i, j, k;
  NodeValueType DotProduct;
  
  NodeType* CenterNode = it.GetCenterPixel();
  const NormalVectorType CenterPixel = CenterNode->m_Data;

  NodeType *PreviousNode, *OtherNode;
  NormalVectorType PreviousPixel;
  Vector < NodeValueType, ImageDimension > gradient [ImageDimension];
  NormalVectorType PositiveSidePixel[2], NegativeSidePixel[2], flux;
  unsigned long stride [ImageDimension];
  unsigned long center;

  for( j = 0; j < ImageDimension; j++ )
    {
    stride[j] = it.GetStride( (unsigned long) j);
    }
   center =  it.Size() / 2;

  for (i=0;i<ImageDimension;i++) // flux offset axis
    {
    PreviousNode = it.GetPrevious (i);
    if (PreviousNode == 0) 
      {
      for( j = 0; j < ImageDimension; j++ )
        {
        CenterNode->m_Flux[i][j] = NumericTraits<NodeValueType>::Zero;
        }
      }
    else 
      {
      PreviousPixel = PreviousNode->m_Data;
      for (j=0;j<ImageDimension;j++) // derivative axis
        {
        if (i!=j) // compute derivative on a plane
          {
          // compute differences (j-axis) in line with center pixel
          OtherNode = it.GetPrevious (j);
          if (OtherNode == 0)
            {
            NegativeSidePixel[0] = CenterPixel;
            }
          else 
            {
            NegativeSidePixel[0] = OtherNode->m_Data;
            }
          OtherNode = it.GetNext (j);
          if (OtherNode == 0)
            {
            PositiveSidePixel[0] = CenterPixel;
            }
          else 
            {
            PositiveSidePixel[0] = OtherNode->m_Data;
            }

          // compute derivative (j-axis) offset from center pixel on i-axis
          OtherNode = it.GetPixel (center - stride[i] - stride[j]);
          if (OtherNode == 0)
            {
            NegativeSidePixel[1] = PreviousPixel;
            }
          else 
            {
            NegativeSidePixel[1] = OtherNode->m_Data;
            }
          OtherNode = it.GetPixel (center - stride[i] + stride[j]);
          if (OtherNode == 0)
            {
            PositiveSidePixel[1] = PreviousPixel;
            }
          else 
            {
            PositiveSidePixel[1] = OtherNode->m_Data;
            }
          
          gradient[j] = ( ( PositiveSidePixel[0]+PositiveSidePixel[1] )- 
                          ( NegativeSidePixel[0]+NegativeSidePixel[1] ) )*
            static_cast<NodeValueType>(0.25);
          }
        else // compute derivative on a line
          {
          gradient[i] = CenterPixel-PreviousPixel; 
          }
        } // end derivative axis

      // now compute the intrinsic derivative
      for (j = 0; j < ImageDimension; j++) // component axis
        {
        DotProduct = NumericTraits<NodeValueType>::Zero;
        for (k = 0; k < ImageDimension; k++) // derivative axis
          {
          DotProduct += (gradient[k][j]*CenterNode->m_ManifoldNormal[i][k]);
          }
        flux[j] = gradient[i][j]-CenterNode->m_ManifoldNormal[i][i]*DotProduct;
        }
      // do following line for non-intrinsic derivative
      //flux = gradient[i];
      if (m_NormalProcessType == 1)
        {
        // anisotropic diffusion
        CenterNode->m_Flux[i] =
          flux * this->FluxStopFunction(flux.GetSquaredNorm());
        }
      else
        {
        // isotropic diffusion
        CenterNode->m_Flux[i] = flux;
        }
      } // end if-else PreviousNode==0
    } // end flux offset axis
}

template <class TSparseImageType>
typename NormalVectorDiffusionFunction <TSparseImageType>::NormalVectorType
NormalVectorDiffusionFunction <TSparseImageType>
::ComputeSparseUpdate (NeighborhoodType &it,
                       void *globalData,
                       const FloatOffsetType &offset) const
{
  int i;
  NormalVectorType change;
  NodeValueType DotProduct;
  const NodeType* CenterNode = it.GetCenterPixel();
  const NormalVectorType CenterPixel = CenterNode->m_Data;
  NodeType* NextNode;
  
  change = NumericTraits<NodeValueType>::Zero;
  for (i=0;i<ImageDimension;i++) // flux offset axis
    {
    NextNode = it.GetNext (i);
    if (NextNode == 0)
      {
      change -= CenterNode->m_Flux[i];
      }
    else
      {
      change += ( NextNode->m_Flux[i] - CenterNode->m_Flux[i]);
      }
    } // end flux offset axis
  DotProduct = change*CenterPixel;
  change -= CenterPixel*DotProduct;

  return change;
}

} // end namespace itk

#endif
