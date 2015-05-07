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
#ifndef itkFEMFiniteDifferenceFunctionLoad_hxx
#define itkFEMFiniteDifferenceFunctionLoad_hxx

#include "itkFEMFiniteDifferenceFunctionLoad.h"

namespace itk
{
namespace fem
{

template <typename TMoving, typename TFixed>
::itk::LightObject::Pointer
FiniteDifferenceFunctionLoad<TMoving, TFixed>::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New();

  copyPtr->m_MovingImage = this->m_MovingImage;
  copyPtr->m_FixedImage = this->m_FixedImage;
  copyPtr->m_MetricRadius = this->m_MetricRadius;
  copyPtr->m_MovingSize = this->m_MovingSize;
  copyPtr->m_FixedSize = this->m_FixedSize;
  copyPtr->m_NumberOfIntegrationPoints = this->m_NumberOfIntegrationPoints;
  copyPtr->m_SolutionIndex = this->m_SolutionIndex;
  copyPtr->m_SolutionIndex2 = this->m_SolutionIndex2;
  copyPtr->m_Gamma = this->m_Gamma;
  copyPtr->m_Solution = this->m_Solution;
  copyPtr->m_GradSigma = this->m_GradSigma;
  copyPtr->m_Sign = this->m_Sign;
  copyPtr->m_WhichMetric = this->m_WhichMetric;
  copyPtr->m_DifferenceFunction = this->m_DifferenceFunction;
  copyPtr->m_DisplacementField = this->m_DisplacementField;

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

template <typename TMoving, typename TFixed>
FiniteDifferenceFunctionLoad<TMoving, TFixed>::FiniteDifferenceFunctionLoad() :
  m_MovingImage(ITK_NULLPTR),
  m_FixedImage(ITK_NULLPTR),
  m_NumberOfIntegrationPoints(0),
  m_SolutionIndex(1),
  m_SolutionIndex2(0),
  m_Gamma(NumericTraits< Float >::ZeroValue()),
  m_Solution(ITK_NULLPTR),
  m_GradSigma(0.0f),
  m_Sign(1.0f),
  m_WhichMetric(0.0f)
{
  m_MetricRadius.Fill(1);
}

template <typename TMoving, typename TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::InitializeIteration()
{

  typedef   MeanSquareRegistrationFunctionType defaultRegistrationFunctionType;

  if( !m_DifferenceFunction )
    {
    typename defaultRegistrationFunctionType::Pointer drfp
      = defaultRegistrationFunctionType::New();
    this->SetMetric(static_cast<FiniteDifferenceFunctionType *>(drfp) );
    }
  //std::cout << " load sizes " << m_DisplacementField->GetLargestPossibleRegion().GetSize()
  //          << "  image " << m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;

  m_DifferenceFunction->InitializeIteration();

}

template <typename TMoving, typename TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::InitializeMetric()
{
  this->InitializeIteration();
}

template <typename TMoving, typename TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::PrintCurrentEnergy()
{
  if( m_DifferenceFunction )
    {
    std::cout << " energy " << m_DifferenceFunction->GetEnergy() << std::endl;
    }
}

template <typename TMoving, typename TFixed>
double
FiniteDifferenceFunctionLoad<TMoving, TFixed>::GetCurrentEnergy()
{
  if( m_DifferenceFunction )
    {
    return m_DifferenceFunction->GetEnergy();
    }
  else
    {
    return 0.0;
    }
}

template <typename TMoving, typename TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::SetCurrentEnergy(double e)
{
  if( m_DifferenceFunction )
    {
    m_DifferenceFunction->SetEnergy(e);
    }
}


template <typename TMoving, typename TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::Float
FiniteDifferenceFunctionLoad<TMoving, TFixed>::EvaluateMetricGivenSolution( ElementContainerType *el, Float step)
{
  Float energy = 0.0, defe = 0.0;

  vnl_vector_fixed<Float, 2 *ImageDimension> InVec(0.0);

  typename Element::VectorType ip, shapef;
  typename Element::MatrixType solmat;
  typename Element::Float w;


  //ElementContainerType::Iterator elt;
  if ( (el == ITK_NULLPTR) || (el->Size() < 1) )
    {
    return 10.0;
    }

  Element::Pointer element = el->GetElement(0);
  const unsigned int Nnodes = element->GetNumberOfNodes();

  FEMVectorType Gpos;
  Gpos.set_size(ImageDimension);
  Gpos.fill(0.0);

  solmat.set_size(Nnodes * ImageDimension, 1);
  for(unsigned int elt = 0; elt < el->Size(); elt++ )
    {
    element = el->GetElement( elt );
    for( unsigned int i = 0; i < m_NumberOfIntegrationPoints; i++ )
      {
      element->GetIntegrationPointAndWeight(i, ip, w, m_NumberOfIntegrationPoints);
      //FIXME REMOVE WHEN ELEMENT NEW IS BASE CLASS
      shapef = element->ShapeFunctions(ip);

      float solval, posval;
      Float detJ = element->JacobianDeterminant(ip);
      for( unsigned int f = 0; f < ImageDimension; f++ )
        {
        solval = 0.0;
        posval = 0.0;
        for( unsigned int n = 0; n < Nnodes; n++ )
          {
          posval += shapef[n] * ( ( element->GetNodeCoordinates(n) )[f]);
          float nodeval = ( (m_Solution)->GetSolutionValue( element->GetNode(n)->GetDegreeOfFreedom(f), m_SolutionIndex)
                            + (m_Solution)->GetSolutionValue( element->GetNode(n)->GetDegreeOfFreedom(f),
                                                              m_SolutionIndex2) * step);

          solval += shapef[n] * nodeval;
          solmat[(n * ImageDimension) + f][0] = nodeval;
          }
        InVec[f] = posval;
        Gpos[f] = posval;
        InVec[f + ImageDimension] = solval;
        }

      float tempe = 0.0;
      try
        {
        this->Fe( Gpos );
        tempe = std::fabs(0.0);
        }
      catch( ... )
        {
        // do nothing we dont care if the metric region is outside the image
        }
      for( unsigned int n = 0; n < Nnodes; n++ )
        {
        itk::fem::Element::Float temp = shapef[n] * tempe * w * detJ;
        energy += temp;
        }
      }

    defe += element->GetElementDeformationEnergy( solmat );
    }

  // std::cout << " def e " << defe << " sim e " << energy*m_Gamma << std::endl;
  return std::fabs( (double)energy * (double)m_Gamma - (double)defe);
}


template <typename TMoving, typename TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::FEMVectorType
FiniteDifferenceFunctionLoad<TMoving, TFixed>::Fe( FEMVectorType  Gpos )
{

  // We assume the vector input is of size 2*ImageDimension.
  // The 0 to ImageDimension-1 elements contain the position, p,
  // in the reference image.  The next ImageDimension to 2*ImageDimension-1
  // elements contain the value of the vector field at that point, v(p).
  //
  // Thus, we evaluate the derivative at the point p+v(p) with respect to
  // some region of the target (fixed) image by calling the metric with
  // the translation parameters as provided by the vector field at p.
  // ------------------------------------------------------------

  VectorType    OutVec;
  FEMVectorType femVec;

  femVec.set_size(ImageDimension);
  femVec.fill(0.0);

  if( !m_DifferenceFunction || !m_DisplacementField || !m_FixedImage || !m_MovingImage )
    {
    this->InitializeIteration();
    if( !m_DisplacementField || !m_FixedImage || !m_MovingImage )
      {
      //std::cout << " input data {field,fixed/moving image} are not set " << std::endl;
      return femVec;
      }
    //std::cout << " sizes " << m_DisplacementField->GetLargestPossibleRegion().GetSize() << std::endl;
    //std::cout << "  image " << m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;
    }

  typedef typename TMoving::IndexType::IndexValueType OIndexValueType;
  typename TMoving::IndexType oindex;
  typename TMoving::PointType physicalPoint;

  unsigned int k;
  bool         inimage = true;
  for( k = 0; k < ImageDimension; k++ )
    {
    if( vnl_math_isnan(Gpos[k])  || vnl_math_isinf(Gpos[k]) || std::fabs(Gpos[k]) > 1.e33 )
      {
      return femVec;
      }

      physicalPoint[k] = Gpos[k];
    }

  m_FixedImage->TransformPhysicalPointToIndex(physicalPoint, oindex);

  for( k = 0; k < ImageDimension; k++ )
    {
    if( oindex[k] > static_cast<OIndexValueType>(m_FixedSize[k] - 1) || oindex[k] < 0 )
      {
      inimage = false;
      }
    }

  if( !inimage )
    {
    return femVec;
    }

  FieldIteratorType nD(m_MetricRadius, m_DisplacementField, m_DisplacementField->GetLargestPossibleRegion() );
  nD.SetLocation(oindex);

  void* globalData = ITK_NULLPTR;
  OutVec = m_DifferenceFunction->ComputeUpdate(nD, globalData);
  for( k = 0; k < ImageDimension; k++ )
    {
    if( vnl_math_isnan(OutVec[k])  || vnl_math_isinf(OutVec[k] ) )
      {
      femVec[k] = 0.0;
      }
    else
      {
      femVec[k] = OutVec[k] * m_Sign;
      }
    }
  return femVec;
}

template <typename TMoving, typename TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::ApplyLoad
  ( Element::ConstPointer element, Element::VectorType & F)
{
  // Order of integration
  // FIXME: Allow changing the order of integration by setting a
  //        static member within an element base class.
  unsigned int order = GetNumberOfIntegrationPoints();

  const unsigned int NumIntegrationPoints = element->GetNumberOfIntegrationPoints(order);
  const unsigned int NumDegreesOfFreedom = element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int NumNodes = element->GetNumberOfNodes();

  Element::VectorType force(NumDegreesOfFreedom, 0.0),
  ip, gip, force_tmp, shapef;
  Element::Float w, detJ;

  F.set_size(element->GetNumberOfDegreesOfFreedom() );
  F.fill(0.0);
  shapef.set_size(NumNodes);
  gip.set_size(NumDegreesOfFreedom);
  for( unsigned int i = 0; i < NumIntegrationPoints; i++ )
    {
    element->GetIntegrationPointAndWeight(i, ip, w, order);

    shapef = element->ShapeFunctions(ip);
    detJ = element->JacobianDeterminant(ip);
    for( unsigned int f = 0; f < NumDegreesOfFreedom; f++ )
      {
      float posval = 0.0;
      for( unsigned int n = 0; n < NumNodes; n++ )
        {
        posval += shapef[n] * ( (element->GetNodeCoordinates(n) )[f]);
        }
      gip[f] = posval;
      }

    // Adjust the size of a force vector returned from the load object so
    // that it is equal to the number of DOFs per node. If the Fg returned
    // a vector with less dimensions, we add zero elements. If the Fg
    // returned a vector with more dimensions, we remove the extra dimensions.
    force.fill(0.0);

    force = this->Fe(gip);
    // Calculate the equivalent nodal loads
    for( unsigned int n = 0; n < NumNodes; n++ )
      {
      for( unsigned int d = 0; d < NumDegreesOfFreedom; d++ )
        {
        itk::fem::Element::Float temp = shapef[n] * force[d] * w * detJ;
        F[n * NumDegreesOfFreedom + d] += temp;
        }
      }
    }
}

} // end namespace fem
} // end namespace itk

#endif
