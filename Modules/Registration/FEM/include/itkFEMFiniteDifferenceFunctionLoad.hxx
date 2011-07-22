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
#ifndef __itkFEMFiniteDifferenceFunctionLoad_hxx
#define __itkFEMFiniteDifferenceFunctionLoad_hxx

#include "itkFEMFiniteDifferenceFunctionLoad.h"

namespace itk
{
namespace fem
{

// Explicit New() method, used here because we need to split the itkNewMacro()
// in order to overload the CreateAnother() method.
template <class TMoving, class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::Pointer
FiniteDifferenceFunctionLoad<TMoving, TFixed>::New(void)
{
  Pointer smartPtr = ::itk::ObjectFactory<Self>::Create();

  if( smartPtr.IsNull() )
    {
    smartPtr = static_cast<Pointer>(new Self);
    }
  smartPtr->UnRegister();
  return smartPtr;
}

// Explicit New() method, used here because we need to split the itkNewMacro()
// in order to overload the CreateAnother() method.
template <class TMoving, class TFixed>
::itk::LightObject::Pointer
FiniteDifferenceFunctionLoad<TMoving, TFixed>::CreateAnother(void) const
{
  ::itk::LightObject::Pointer smartPtr;
  Pointer copyPtr = Self::New().GetPointer();

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
  copyPtr->m_DeformationField = this->m_DeformationField;

  smartPtr = static_cast<Pointer>(copyPtr);

  return smartPtr;
}

template <class TMoving, class TFixed>
FiniteDifferenceFunctionLoad<TMoving, TFixed>::FiniteDifferenceFunctionLoad()
{
  m_SolutionIndex = 1;
  m_SolutionIndex2 = 0;
  m_Sign = 1.0;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    m_MetricRadius[i] = 1;
    }

  m_DifferenceFunction = NULL;
  m_DeformationField = NULL;

}

template <class TMoving, class TFixed>
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
  std::cout << " load sizes " << m_DeformationField->GetLargestPossibleRegion().GetSize()
            << "  image " << m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;

  m_DifferenceFunction->InitializeIteration();

}

template <class TMoving, class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::InitializeMetric()
{
  this->InitializeIteration();
}

template <class TMoving, class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::PrintCurrentEnergy()
{
  if( m_DifferenceFunction )
    {
    std::cout << " energy " << m_DifferenceFunction->GetEnergy() << std::endl;
    }
}

template <class TMoving, class TFixed>
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

template <class TMoving, class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::SetCurrentEnergy(double e)
{
  if( m_DifferenceFunction )
    {
    m_DifferenceFunction->SetEnergy(e);
    }
}

template <class TMoving, class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::Float
FiniteDifferenceFunctionLoad<TMoving, TFixed>::EvaluateMetricGivenSolution( Element::ArrayType * itkNotUsed(
                                                                              el), Float itkNotUsed(step) )
{
  return 10.0;  // FIXME
}

template <class TMoving, class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::Float
FiniteDifferenceFunctionLoad<TMoving, TFixed>::EvaluateMetricGivenSolution1(
  ElementContainerType *, Float)
{
  return 10.0;  // FIXME
}

#if __DEFINED__FIXME__THIS_IS_NEVER_REACHED_BECAUSE_OF_OVERRIDING_RETURN_STATEMENT__

template <class TMoving, class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::Float
FiniteDifferenceFunctionLoad<TMoving, TFixed>::EvaluateMetricGivenSolution( Element::ArrayType* el, Float step)
{
  return 10.0;  // FIXME
  Float energy = 0.0, defe = 0.0;

  vnl_vector_fixed<Float, 2 *ImageDimension> InVec(0.0);

  typename Element::VectorType ip, shapef;
  typename Element::MatrixType solmat;
  typename Element::Float w;

  typedef typename Element::ArrayType ArrayType;

  ArrayType::iterator elt = el->begin();

  const unsigned int Nnodes = (*elt)->GetNumberOfNodes();

  FEMVectorType Gpos, Gsol;
  Gpos.set_size(ImageDimension); Gpos.fill(0.0);
  Gsol.set_size(ImageDimension); Gsol.fill(0.0);

  solmat.set_size(Nnodes * ImageDimension, 1);
  for(; elt != el->end(); elt++ )
    {
    for( unsigned int i = 0; i < m_NumberOfIntegrationPoints; i++ )
      {
      dynamic_cast<Element *>(&*(*elt) )->GetIntegrationPointAndWeight(i, ip, w, m_NumberOfIntegrationPoints);
      //FIXME REMOVE WHEN ELEMENT NEW IS BASE CLASS
      shapef = (*elt)->ShapeFunctions(ip);

      float solval, posval;
      Float detJ = (*elt)->JacobianDeterminant(ip);
      for( unsigned int f = 0; f < ImageDimension; f++ )
        {
        solval = 0.0;
        posval = 0.0;
        for( unsigned int n = 0; n < Nnodes; n++ )
          {
          posval += shapef[n] * ( ( (*elt)->GetNodeCoordinates(n) )[f]);
          float nodeval = ( (m_Solution)->GetSolutionValue( (*elt)->GetNode(n)->GetDegreeOfFreedom(f), m_SolutionIndex)
                            + (m_Solution)->GetSolutionValue( (*elt)->GetNode(n)->GetDegreeOfFreedom(f),
                                                              m_SolutionIndex2) * step);

          solval += shapef[n] * nodeval;
          solmat[(n * ImageDimension) + f][0] = nodeval;
          }
        InVec[f] = posval;
        Gpos[f] = posval;
        InVec[f + ImageDimension] = solval;
        Gsol[f] = solval;
        }

      float tempe = 0.0;
      try
        {
        this->Fe(Gpos, Gsol); // FIXME
        tempe = vcl_fabs(0.0);
        }
      catch( ... )
        {
        // do nothing we dont care if the metric region is outside the image
        // std::cerr << e << std::endl;
        }
      for( unsigned int n = 0; n < Nnodes; n++ )
        {
        itk::fem::Element::Float temp = shapef[n] * tempe * w * detJ;
        energy += temp;
        }
      }

    defe += 0.0; // (double)(*elt)->GetElementDeformationEnergy( solmat );
    }

  // std::cout << " def e " << defe << " sim e " << energy*m_Gamma << std::endl;
  return vcl_fabs( (double)energy * (double)m_Gamma - (double)defe);
}

#endif

template <class TMoving, class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving, TFixed>::FEMVectorType
FiniteDifferenceFunctionLoad<TMoving, TFixed>::Fe
  ( FEMVectorType  Gpos,
  FEMVectorType  Gsol)
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

  if( !m_DifferenceFunction || !m_DeformationField || !m_FixedImage || !m_MovingImage )
    {
    std::cout << " initializing FE() ";
    this->InitializeIteration();
    std::cout << " done " << std::endl;
    if( !m_DeformationField || !m_FixedImage || !m_MovingImage )
      {
      std::cout << " input data {field,fixed/moving image} are not set ";
      return femVec;
      }
    std::cout << " sizes " << m_DeformationField->GetLargestPossibleRegion().GetSize()
              << "  image " << m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;
    }

  typedef typename TMoving::IndexType::IndexValueType OIndexValueType;
  typename TMoving::IndexType oindex;

  unsigned int k;
  bool         inimage = true;
  for( k = 0; k < ImageDimension; k++ )
    {

    if( vnl_math_isnan(Gpos[k])  || vnl_math_isinf(Gpos[k]) ||
        vnl_math_isnan(Gsol[k])  || vnl_math_isinf(Gsol[k]) ||
        vcl_fabs(Gpos[k]) > 1.e33  || vcl_fabs(Gsol[k]) > 1.e33  )
      {
      return femVec;
      }
    else
      {
      oindex[k] = (long) (Gpos[k] + 0.5);
      }
    if( oindex[k] > static_cast<OIndexValueType>(m_FixedSize[k] - 1) || oindex[k] < 0 )
      {
      inimage = false;
      }
    // FIXME : resized images not same as vect field from expand image filter
    //  expandimagefilter does only dyadic size!!!

    }
  if( !inimage )
    {
    return femVec;
    }

//  std::cout << " index " << oindex << std::endl;

  FieldIteratorType nD(m_MetricRadius, m_DeformationField, m_DeformationField->GetLargestPossibleRegion() );
  nD.SetLocation(oindex);

  void* globalData = NULL;
  OutVec = m_DifferenceFunction->ComputeUpdate(nD, globalData);
  for( k = 0; k < ImageDimension; k++ )
    {
    if( vnl_math_isnan(OutVec[k])  || vnl_math_isinf(OutVec[k] ) )
      {
      femVec[k] = 0.0;
      }
    else
      {
      femVec[k] = OutVec[k];
      }
    }
  return femVec;
}

template <class TMoving, class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving, TFixed>::ApplyLoad
  ( Element::ConstPointer element, Element::VectorType & F)
{
  const unsigned int TotalSolutionIndex = 1; /* Need to change if the index changes in CrankNicolsonSolver */

  typename Solution::ConstPointer   S = GetSolution();   // has current solution state

  // Order of integration
  // FIXME: Allow changing the order of integration by setting a
  //        static member within an element base class.
  unsigned int order = GetNumberOfIntegrationPoints();

  const unsigned int NumIntegrationPoints = element->GetNumberOfIntegrationPoints(order);
  const unsigned int NumDegreesOfFreedom = element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int NumNodes = element->GetNumberOfNodes();

  Element::VectorType force(NumDegreesOfFreedom, 0.0),
  ip, gip, gsol, force_tmp, shapef;
  Element::Float w, detJ;

  F.set_size(element->GetNumberOfDegreesOfFreedom() );
  F.fill(0.0);
  shapef.set_size(NumNodes);
  gsol.set_size(NumDegreesOfFreedom);
  gip.set_size(NumDegreesOfFreedom);
  for( unsigned int i = 0; i < NumIntegrationPoints; i++ )
    {
    element->GetIntegrationPointAndWeight(i, ip, w, order);
    if( NumDegreesOfFreedom == 3 )
      {
      shapef = element->ShapeFunctions(ip);
      float solval, posval;
      detJ = element->JacobianDeterminant(ip);
      for( unsigned int f = 0; f < NumDegreesOfFreedom; f++ )
        {
        solval = 0.0;
        posval = 0.0;
        for( unsigned int n = 0; n < NumNodes; n++ )
          {
          posval += shapef[n] * ( (element->GetNodeCoordinates(n) )[f]);
          solval += shapef[n] * S->GetSolutionValue( element->GetNode(n)->GetDegreeOfFreedom(f), TotalSolutionIndex);
          }
        gsol[f] = solval;
        gip[f] = posval;
        }

      // Adjust the size of a force vector returned from the load object so
      // that it is equal to the number of DOFs per node. If the Fg returned
      // a vector with less dimensions, we add zero elements. If the Fg
      // returned a vector with more dimensions, we remove the extra dimensions.
      force.fill(0.0);

      force = this->Fe(gip, gsol);
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
}

} // end namespace fem
} // end namespace itk

#endif
