/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFEMFiniteDifferenceFunctionLoad.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

=========================================================================*/
#ifndef _itkFEMFiniteDifferenceFunctionLoad_txx_
#define _itkFEMFiniteDifferenceFunctionLoad_txx_

#include "itkFEMFiniteDifferenceFunctionLoad.h"


namespace itk {
namespace fem {



template<class TMoving,class TFixed>
FiniteDifferenceFunctionLoad<TMoving , TFixed>::FiniteDifferenceFunctionLoad()
{
  m_SolutionIndex=1;
  m_SolutionIndex2=0;
  m_Sign=1.0;

  for (unsigned int i=0; i<ImageDimension; i++)
  {
    m_MetricRadius[i] = 1;
  }
  
  m_DifferenceFunction=NULL;
  m_DeformationField=NULL;

}


template<class TMoving,class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving , TFixed>::InitializeIteration()
{

  typedef   MeanSquareRegistrationFunctionType  defaultRegistrationFunctionType;

  if (!m_DifferenceFunction)
  {
    typename defaultRegistrationFunctionType::Pointer drfp 
      = defaultRegistrationFunctionType::New();
    this->SetMetric(static_cast<FiniteDifferenceFunctionType *>(drfp));
  }
  std::cout << " load sizes " << m_DeformationField->GetLargestPossibleRegion().GetSize() 
      << "  image " << m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;

  m_DifferenceFunction->InitializeIteration();

}


template<class TMoving,class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving , TFixed>::InitializeMetric()
{
  this->InitializeIteration();
}

template<class TMoving,class TFixed>
void
FiniteDifferenceFunctionLoad<TMoving , TFixed>::PrintCurrentEnergy()
{
  if ( m_DifferenceFunction ) 
    std::cout << " energy " << m_DifferenceFunction->GetEnergy() << std::endl;
}

template<class TMoving,class TFixed>
double 
FiniteDifferenceFunctionLoad<TMoving , TFixed>::GetCurrentEnergy()
{
  if ( m_DifferenceFunction ) 
    return m_DifferenceFunction->GetEnergy();
  else return 0.0;
}

template<class TMoving,class TFixed>
void 
FiniteDifferenceFunctionLoad<TMoving , TFixed>::SetCurrentEnergy(double e)
{
  if ( m_DifferenceFunction ) m_DifferenceFunction->SetEnergy(e);
}

template<class TMoving,class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving , TFixed>::Float 
FiniteDifferenceFunctionLoad<TMoving , TFixed>::EvaluateMetricGivenSolution( Element::ArrayType* el,Float step)
{
  Float energy=0.0,defe=0.0; 

  return 10.0;  //FIXME

  vnl_vector_fixed<Float,2*ImageDimension> InVec(0.0);
   
  typename Element::VectorType ip,shapef;
  typename Element::MatrixType solmat;
  typename Element::Float w;
 
  typename Element::ArrayType::iterator elt=el->begin();
  const unsigned int Nnodes=(*elt)->GetNumberOfNodes();

  FEMVectorType Gpos,Gsol;
  Gpos.resize(ImageDimension); Gpos.fill(0.0);
  Gsol.resize(ImageDimension); Gsol.fill(0.0);

  solmat.resize(Nnodes*ImageDimension,1);

  for(  ; elt!=el->end(); elt++) 
  {
    for(unsigned int i=0; i<m_NumberOfIntegrationPoints; i++)
    {
      dynamic_cast<Element*>(&*(*elt))->GetIntegrationPointAndWeight(i,ip,w,m_NumberOfIntegrationPoints); // FIXME REMOVE WHEN ELEMENT NEW IS BASE CLASS
      shapef = (*elt)->ShapeFunctions(ip);

      float solval,posval;
      Float detJ=(*elt)->JacobianDeterminant(ip);
        
      for(unsigned int f=0; f<ImageDimension; f++)
      {
        solval=0.0;
        posval=0.0;
        for(unsigned int n=0; n<Nnodes; n++)
        {
          posval+=shapef[n]*(((*elt)->GetNodeCoordinates(n))[f]);
          float nodeval=( (m_Solution)->GetSolutionValue( (*elt)->GetNode(n)->GetDegreeOfFreedom(f) , m_SolutionIndex)
            +(m_Solution)->GetSolutionValue( (*elt)->GetNode(n)->GetDegreeOfFreedom(f) , m_SolutionIndex2)*step);
      
          solval+=shapef[n] * nodeval;   
          solmat[(n*ImageDimension)+f][0]=nodeval;
        }
        InVec[f]=posval;
        Gpos[f]=posval;
        InVec[f+ImageDimension]=solval;
        Gsol[f]=solval;
      }

      float tempe=0.0;
      try
      {
        this->Fe(Gpos,Gsol); // FIXME
        tempe=fabs(0.0);
      }
      catch( itk::ExceptionObject & e )
      { 
      // do nothing we dont care if the metric region is outside the image
      //std::cerr << e << std::endl;
      }
      for(unsigned int n=0; n<Nnodes; n++)
      {
        itk::fem::Element::Float temp=shapef[n]*tempe*w*detJ;
        energy+=temp;
      }
    }  
    
    defe+=0.0;//(double)(*elt)->GetElementDeformationEnergy( solmat );
  }
   
  //std::cout << " def e " << defe << " sim e " << energy*m_Gamma << std::endl;
  return fabs((double)energy*(double)m_Gamma-(double)defe);

}



template<class TMoving,class TFixed>
typename FiniteDifferenceFunctionLoad<TMoving , TFixed>::FEMVectorType 
FiniteDifferenceFunctionLoad<TMoving , TFixed>::Fe
(FiniteDifferenceFunctionLoad<TMoving , TFixed>::FEMVectorType  Gpos,
 FiniteDifferenceFunctionLoad<TMoving , TFixed>::FEMVectorType  Gsol) 
{

// We assume the vector input is of size 2*ImageDimension.
// The 0 to ImageDimension-1 elements contain the position, p,
// in the reference image.  The next ImageDimension to 2*ImageDimension-1
// elements contain the value of the vector field at that point, v(p).
//
// Thus, we evaluate the derivative at the point p+v(p) with respect to
// some region of the target (fixed) image by calling the metric with 
// the translation parameters as provided by the vector field at p.
//------------------------------------------------------------

  VectorType OutVec;
  FEMVectorType femVec;
  femVec.resize(ImageDimension);
  femVec.fill(0.0);

  if (!m_DifferenceFunction || !m_DeformationField || !m_FixedImage || !m_MovingImage)
  { 
    std::cout << " initializing FE() ";
    this->InitializeIteration();   
    std::cout << " done " << std::endl;
    if (!m_DeformationField || !m_FixedImage || !m_MovingImage )
    {
      std::cout << " input data {field,fixed/moving image} are not set ";
      return femVec;
    }
    std::cout << " sizes " << m_DeformationField->GetLargestPossibleRegion().GetSize() 
      << "  image " << m_FixedImage->GetLargestPossibleRegion().GetSize() << std::endl;
  }

  
  typename TMoving::IndexType oindex;

  unsigned int k=0;
  bool inimage=true;
  for(k = 0; k < ImageDimension; k++ ) 
  {
    
    if ( vnl_math_isnan(Gpos[k])  || vnl_math_isinf(Gpos[k]) ||
        vnl_math_isnan(Gsol[k])  || vnl_math_isinf(Gsol[k]) ||
         fabs(Gpos[k]) > 1.e33  || fabs(Gsol[k]) > 1.e33  ) 
    {
      return femVec;
    }
    else oindex[k]=(long) (Gpos[k]+0.5);
    if (oindex[k] > m_FixedSize[k]-1 || oindex[k] < 0) inimage=false; 
    // FIXME : resized images not same as vect field from expand image filter
    //  expandimagefilter does only dyadic size!!!

  }
  if (!inimage) 
  {
    return femVec;
  }

//  std::cout << " index " << oindex << std::endl;
  
  FieldIteratorType nD(m_MetricRadius, m_DeformationField, m_DeformationField->GetLargestPossibleRegion());
  nD.SetLocation(oindex);
 
  void* globalData=NULL;
  OutVec = m_DifferenceFunction->ComputeUpdate(nD, globalData);

  for (k=0;k<ImageDimension;k++) 
  {
    if ( vnl_math_isnan(OutVec[k])  || vnl_math_isinf(OutVec[k] )) femVec[k]=0.0;
    else femVec[k]=OutVec[k];
  }
  return femVec;
}



template<class TMoving,class TFixed> 
int FiniteDifferenceFunctionLoad<TMoving,TFixed>::CLID()
{
  static const int CLID_ = FEMOF::Register( FiniteDifferenceFunctionLoad::NewB,(std::string("FiniteDifferenceFunctionLoad(")
                +typeid(TMoving).name()+","+typeid(TFixed).name()+")").c_str());
  return CLID_;
}


template<class TMoving,class TFixed> 
const int FiniteDifferenceFunctionLoad<TMoving,TFixed>::DummyCLID=FiniteDifferenceFunctionLoad<TMoving,TFixed>::CLID();


} // end namespace fem
} // end namespace itk

#endif
