/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFEMImageMetricLoad.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

=========================================================================*/
#ifndef _itkFEMImageMetricLoad_txx_
#define _itkFEMImageMetricLoad_txx_

#include "itkFEMImageMetricLoad.h"


namespace itk {
namespace fem {


template<class TReference,class TTarget>
void
ImageMetricLoad<TReference , TTarget>
::InitializeMetric(void)
{ 
  if (!m_Transform) m_Transform = DefaultTransformType::New();
  if (!m_Metric)    m_Metric = DefaultMetricType::New();
  
  m_Temp=0.0;

//------------------------------------------------------------
// Set up the metric -- see MetricTest in Testing
//------------------------------------------------------------
  
  m_Metric->SetMovingImage( m_RefImage );
  m_Metric->SetFixedImage( m_TarImage );

  typename TargetType::RegionType requestedRegion;
  typename TargetType::SizeType  size;
  typename TargetType::IndexType tindex;
//  typename ReferenceType::IndexType rindex;
  // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
  { 
  //Set the size of the image region
    size[k] = 1;
    tindex[k]=0;
  }

// Set the number of integration points to zero (default for an element)

  m_NumberOfIntegrationPoints=0;

// Set the associated region
  requestedRegion.SetSize(size);
  requestedRegion.SetIndex(tindex);
  m_TarImage->SetRequestedRegion(requestedRegion);  
  m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );
  
  m_Metric->SetTransform( m_Transform.GetPointer() );
  m_Interpolator = InterpolatorType::New();
  m_Interpolator->SetInputImage( m_RefImage );
  m_Metric->SetInterpolator( m_Interpolator.GetPointer() );

  
//------------------------------------------------------------
// This call is mandatory before start querying the Metric
// This method do all the necesary connections between the 
// internal components: Interpolator, Transform and Images
//------------------------------------------------------------
  try 
  {
    m_Metric->Initialize();
  } catch( ExceptionObject & e )
  {
    std::cout << "Metric initialization failed" << std::endl;
    std::cout << "Reason " << e.GetDescription() << std::endl;
  }

}


template<class TReference,class TTarget>
ImageMetricLoad<TReference , TTarget>::ImageMetricLoad()
{
  m_Metric=NULL;
  m_Transform = NULL;
  m_SolutionIndex=1;
  m_SolutionIndex2=0;
  m_Sign=1.0;

  for (unsigned int i=0; i<ImageDimension; i++)
  {
    m_MetricRadius[i] = 1;
  }

}


template<class TReference,class TTarget>
typename ImageMetricLoad<TReference , TTarget>::Float 
ImageMetricLoad<TReference , TTarget>::EvaluateMetricGivenSolution( Element::ArrayType* el,Float step)
{
  Float energy=0.0; 

  vnl_vector_fixed<Float,ImageDimension> Sol(0.0);  // total solution at the local point plus incremental solution at the local point
  vnl_vector_fixed<Float,ImageDimension> Gpt(0.0);  // global position given by local point
  vnl_vector_fixed<Float,ImageDimension> Pos(0.0);  // solution at the point
  
  vnl_vector_fixed<Float,2*ImageDimension> InVec(0.0);
   
  Element::VectorType ip;
  Element::Float w;
  
  for(  Element::ArrayType::iterator elt=el->begin(); elt!=el->end(); elt++) 
  {
    for(unsigned int i=0; i<m_NumberOfIntegrationPoints; i++)
    {
      dynamic_cast<Element*>(&*(*elt))->GetIntegrationPointAndWeight(i,ip,w,m_NumberOfIntegrationPoints); // FIXME REMOVE WHEN ELEMENT NEW IS BASE CLASS
      Gpt=(*elt)->GetGlobalFromLocalCoordinates(ip);
      // interpolate the total solution and the incremental solution
      Sol=(*elt)->InterpolateSolution(ip,*m_Solution,m_SolutionIndex)+step*(*elt)->InterpolateSolution(ip,*m_Solution,m_SolutionIndex2);

      for (unsigned int ii=0; ii < ImageDimension; ii++)
      { 
        InVec[ii]=Gpt[ii];
        InVec[ii+ImageDimension]=Sol[ii];
      }
      
      try
      {
      energy+=w*fabs(GetMetric(InVec));
      }
      catch( itk::ExceptionObject & e )
      { 
      // do nothing we dont care if the metric region is outside the image
      //std::cerr << e << std::endl;
      }
      
    }  
  }
   

  return energy;

}



template<class TReference,class TTarget>
typename ImageMetricLoad<TReference , TTarget>::VectorType 
ImageMetricLoad<TReference , TTarget>::Fe
(ImageMetricLoad<TReference , TTarget>::VectorType  Gpos,ImageMetricLoad<TReference , TTarget>::VectorType  Gsol) 
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
// Set up transform parameters
//------------------------------------------------------------
//  int temp=1;
  ParametersType parameters( m_Transform->GetNumberOfParameters() );
  typename TargetType::RegionType requestedRegion;
  TargetRadiusType regionRadius;
  typename TargetType::IndexType tindex;
  typename ReferenceType::IndexType rindex;
  VectorType OutVec(ImageDimension,0.0); // gradient direction
  int lobordercheck=0,hibordercheck=0;
  for( unsigned int k = 0; k < ImageDimension; k++ )
  { 
  //Set the size of the image region
    parameters[k]= Gsol[k]; // this gives the translation by the vector field 
    tindex[k] =(long)(Gpos[k]+Gsol[k]+0.5);  // where the piece of reference image currently lines up under the above translation
    rindex[k]= (long)(Gpos[k]);  // position in reference image
    hibordercheck=(int)tindex[k]+(int)m_MetricRadius[k]-(int)m_TarSize[k];
    lobordercheck=(int)tindex[k]-(int)m_MetricRadius[k];
    if (hibordercheck >= 0) regionRadius[k]=m_MetricRadius[k]-(long)hibordercheck-1;
    else if (lobordercheck < 0) regionRadius[k]=m_MetricRadius[k]+(long)lobordercheck;
    else regionRadius[k]=m_MetricRadius[k];
  }

// Set the associated region

  requestedRegion.SetSize(regionRadius);
  requestedRegion.SetIndex(tindex);

  m_TarImage->SetRequestedRegion(requestedRegion);  
  m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );

//--------------------------------------------------------
// Get metric values

//  typename MetricBaseType::MeasureType     measure;
  typename MetricBaseType::DerivativeType  derivative;

  try
  { 
  //m_Metric->GetValueAndDerivative( parameters, measure, derivative );
    m_Metric->GetDerivative( parameters, derivative );
  }
  catch( ... )
  {
  // do nothing we don't care if the metric lies outside the image sometimes
  //std::cerr << e << std::endl;
  }
 
  for( unsigned int k = 0; k < ImageDimension; k++ )
  {
    if (lobordercheck < 0 || hibordercheck >=0) OutVec[k]=0.0; 
      else OutVec[k]= m_Sign*derivative[k];
  }
 // NOTE : POSSIBLE THAT DERIVATIVE DIRECTION POINTS UP OR DOWN HILL!
 // IN FACT, IT SEEMS MEANSQRS AND NCC POINT IN DIFFT DIRS
  //std::cout   << " deriv " << derivative <<  " val " << measure << endl;
  if (m_Temp !=0.0) 
  return OutVec * exp(-1.*OutVec.magnitude()/m_Temp);
  else return OutVec;
}


template<class TReference,class TTarget>
typename ImageMetricLoad<TReference , TTarget>::Float 
ImageMetricLoad<TReference , TTarget>::GetMetric
(ImageMetricLoad<TReference , TTarget>::VectorType  InVec) 
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
// Set up transform parameters
//------------------------------------------------------------
  ParametersType parameters( m_Transform->GetNumberOfParameters());
  typename TargetType::RegionType requestedRegion;
  typename TargetType::IndexType tindex;
  typename ReferenceType::IndexType rindex;
  TargetRadiusType regionRadius;
  VectorType OutVec(ImageDimension,0.0); // gradient direction
  //std::cout << " pos   translation " << InVec  << endl;
   // initialize the offset/vector part
  for( unsigned int k = 0; k < ImageDimension; k++ )
  { 
  //Set the size of the image region
    parameters[k]= InVec[k+ImageDimension]; // this gives the translation by the vector field 
    tindex[k] =(long)(InVec[k]+InVec[k+ImageDimension]+0.5);  // where the piece of reference image currently lines up under the above translation
    rindex[k]= (long)(InVec[k]);  // position in reference image
    int hibordercheck=(int)tindex[k]+(int)m_MetricRadius[k]-(int)m_TarSize[k];
  int lobordercheck=(int)tindex[k]-(int)m_MetricRadius[k];
  if (hibordercheck > 0) regionRadius[k]=m_MetricRadius[k]-(long)hibordercheck-1;
  else if (lobordercheck < 0) regionRadius[k]=m_MetricRadius[k]+(long)lobordercheck;
  else regionRadius[k]=m_MetricRadius[k];
  }

// Set the associated region

  requestedRegion.SetSize(regionRadius);
  requestedRegion.SetIndex(tindex);

  m_TarImage->SetRequestedRegion(requestedRegion);  
  m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );

//--------------------------------------------------------
// Get metric values

  typename MetricBaseType::MeasureType     measure=0.0;
  try
  { 
  measure=m_Metric->GetValue( parameters);
  }
  catch( ... )
  {
  // do nothing we dont care if the metric lies outside the image sometimes
  //std::cerr << e << std::endl;
  }
      
 
  return (Float) measure;
}


template<class TReference,class TTarget> 
int ImageMetricLoad<TReference,TTarget>::CLID()
{
  static const int CLID_ = FEMOF::Register( ImageMetricLoad::NewB,(std::string("ImageMetricLoad(")
                +typeid(TReference).name()+","+typeid(TTarget).name()+")").c_str());
  return CLID_;
}


template<class TReference,class TTarget> 
const int ImageMetricLoad<TReference,TTarget>::DummyCLID=ImageMetricLoad<TReference,TTarget>::CLID();


} // end namespace fem
} // end namespace itk

#endif
