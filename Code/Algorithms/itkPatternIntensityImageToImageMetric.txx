/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPatternIntensityImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPatternIntensityImageToImageMetric_txx
#define _itkPatternIntensityImageToImageMetric_txx

#include "itkPatternIntensityImageToImageMetric.h"

namespace itk
{

/**
 * Constructor
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::PatternIntensityImageToImageMetric()
{
  m_Parameters = ParametersType::New();
  m_Parameters->Reserve(TMapper::SpaceDimension);
  m_MatchMeasureDerivatives = DerivativeType::New();
  m_MatchMeasureDerivatives->Reserve(TMapper::SpaceDimension);
}



/**
 * Set Target 
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
void
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::SetTarget( TTarget * target ) 
{
  this->m_Target = target;
}


/**
 * Set Mapper
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
void
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::SetMapper( TMapper * mapper ) 
{
  this->m_Mapper = mapper;
}



/**
 * Get the match Measure
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>::MeasureType
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::GetValue( void)
{

  typename TTarget::RegionType  m_Target_region = m_Target->GetLargestPossibleRegion();
  itk::Point<double, TTarget::ImageDimension> Point;  

  double ReferenceValue;
  double TargetValue;

  typedef  itk::SimpleImageRegionIterator<TTarget> TargetIteratorType;


  TargetIteratorType ti(m_Target,m_Target_region);
  ti.Begin();

  typename TTarget::IndexType index;

  m_MatchMeasure=0;
  
  ParametersPointer scaleParameters= ParametersType::New();
  scaleParameters->Reserve(m_Parameters->Size());

  /* Scale the parameters and assume that the translation is at the end*/
  scaleParameters = m_Parameters;
  std::cout  << "ScaleParameters = ";
  ParametersType::Iterator pit = m_Parameters->Begin();
  ParametersType::Iterator spit = scaleParameters->Begin();
  int i=0;
  while (pit != m_Parameters->End())
  {
    spit.Value() = pit.Value();
  if( (i >= TTarget::ImageDimension * TTarget::ImageDimension) \
      || (m_Parameters->Size() == TTarget::ImageDimension) )
  {
    spit.Value() *= m_Target_region.GetSize()[0];
  }
    std::cout  <<  spit.Value() << " ";
  spit++;
  i++;
  pit++;
  }
 
  std::cout<< std::endl; 

  bool insidePoint; 
  int count = 0;

  m_Mapper->GetTransformation()->SetParameters( scaleParameters );

  while(!ti.IsAtEnd())
  {
    index = ti.GetIndex();
    for(unsigned int i=0 ; i<TTarget::ImageDimension ; i++)
    {
    Point[i]=index[i];
    }

    insidePoint = true;

    try {
     ReferenceValue = m_Mapper->Evaluate(Point);               
    }

    //If the Mapped Voxel is outside the image
    catch (MapperException) {  
      insidePoint = false;
    }

    if(insidePoint) {
      TargetValue = ti.Get();
      count++;
      const double diff=ReferenceValue-TargetValue; 
      m_MatchMeasure += diff * diff; 
    }  
  
   ++ti;
  }

 if(count == 0) {
    std::cout << "All the mapped image is outside !" << std::endl;
  return 100000;
  } 


  m_MatchMeasure = m_MatchMeasure / ( count * 1e2 );     
  std::cout<<"m_MatchMeasure= "<<m_MatchMeasure<<std::endl; 
  return m_MatchMeasure;

}


/**
 * Get the match Measure
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
void
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::GetValue(VectorMeasurePointer & matchMeasure)
{
  typename TTarget::RegionType  m_Target_region = m_Target->GetLargestPossibleRegion();
  itk::Point<double, TTarget::ImageDimension> Point;  

  double ReferenceValue;
  double TargetValue;

  typedef  itk::SimpleImageRegionIterator<TTarget> TargetIteratorType;


  TargetIteratorType ti(m_Target,m_Target_region);
  ti.Begin();

  typename TTarget::IndexType index;

  m_MatchMeasure=0;
  
  ParametersPointer scaleParameters= ParametersType::New();
  scaleParameters->Reserve(m_Parameters->Size());

  /* Scale the parameters */
  scaleParameters = m_Parameters;
  std::cout  << "ScaleParameters = ";
  ParametersType::Iterator pit = m_Parameters->Begin();
  ParametersType::Iterator spit = scaleParameters->Begin();
  int i=0;
  while (pit != m_Parameters->End())
  {
    spit.Value() = pit.Value();
  
  if( (i >= TTarget::ImageDimension * TTarget::ImageDimension) \
      || (m_Parameters->Size() == TTarget::ImageDimension) )
  {
    spit.Value() *= m_Target_region.GetSize()[0];
  }

    std::cout  <<  spit.Value() << " ";
  i++;
  spit++;
  pit++;
  }
 
  std::cout<< std::endl; 


  bool insidePoint; 
  int count = 0;
  m_Mapper->GetTransformation()->SetParameters( scaleParameters );

  while(!ti.IsAtEnd())
  {
    index = ti.GetIndex();
     for (unsigned i=0; i<TTarget::ImageDimension ;i++)
   {
     Point[i]=index[i];
   }
    
    insidePoint = true;

    try {
     ReferenceValue = m_Mapper->Evaluate(Point);               
    }

    //If the Mapped Voxel is outside the image
    catch (MapperException) {  
      insidePoint = false;
    }

    if(insidePoint) {
      TargetValue = ti.Get();
      count++;
      const double diff=ReferenceValue-TargetValue; 
      m_MatchMeasure += 1.0/(1.0 + diff * diff); 
    }  
  
   ++ti;
  }

 if(count == 0) {
    std::cout << "All the mapped image is outside !" << std::endl;
  m_MatchMeasure=100000;
  } 

   
  m_MatchMeasure = m_MatchMeasure / count; 
       
  std::cout<< "m_MatchMeasure= " << m_MatchMeasure <<std::endl;
   
  VectorMeasureType::Iterator it = matchMeasure->Begin();
  while(it != matchMeasure->End() )
  {
    it.Value() = m_MatchMeasure;
  it++;
  }

}




/**
 * Get the Derivative Measure
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>::DerivativePointer
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::GetDerivative( void )
{
  const double delta = 0.00011;

  /* Maybe move that into the constructor */
  ParametersPointer derivative =  ParametersType::New();
  derivative->Reserve(m_Parameters->Size());
  
  DerivativeType::Iterator dit = m_MatchMeasureDerivatives->Begin();
  ParametersType::Iterator it  = m_Parameters->Begin();

  double valuepd0;
  double valuepd1;
  while( it != m_Parameters->End() ) 
  { 
  
    it.Value() -= delta; 
  valuepd0 = (double)GetValue();
    it.Value() = it.Value()+2*delta; 
    valuepd1 = (double)GetValue();
    it.Value() -= delta; // to restore the original parameter 
  dit.Value() = (double) ( valuepd1 - valuepd0 ) / (2*delta);  
    dit.Value() /= 1e5 ;
  dit++;
  it++;
  }

  std::cout<<"m_MatchMeasureDerivatives= ";
  
  dit = m_MatchMeasureDerivatives->Begin();
  while( dit !=  m_MatchMeasureDerivatives->End() ) 
  { 
  std::cout << dit.Value() << " ";
  dit++;
  }
      
  std::cout << std::endl;
  return m_MatchMeasureDerivatives;
}


/**
 * Get both the match Measure and theDerivative Measure 
 */
template < class TTarget, class TMapper, class TMeasure,  class TDerivative > 
void
PatternIntensityImageToImageMetric<TTarget,TMapper,TMeasure,TDerivative>
::GetValueAndDerivative(MeasureType & Value, DerivativeType  & Derivative)
{
  Value = GetValue();
  Derivative = GetDerivative();
}



} // end namespace itk


#endif
