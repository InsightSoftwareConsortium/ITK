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
  m_Gamma=1.0;

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
  m_MetricGradientImage=NULL;

}


template<class TReference,class TTarget>
typename ImageMetricLoad<TReference , TTarget>::Float 
ImageMetricLoad<TReference , TTarget>::EvaluateMetricGivenSolution( Element::ArrayType* el,Float step)
{
  Float energy=0.0,defe=0.0; 

  vnl_vector_fixed<Float,2*ImageDimension> InVec(0.0);
   
  Element::VectorType ip,shapef;
  Element::MatrixType solmat;
  Element::Float w;
 
  Element::ArrayType::iterator elt=el->begin();
  const unsigned int Nnodes=(*elt)->GetNumberOfNodes();

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
        InVec[f+ImageDimension]=solval;
      }

      float tempe=0.0;
      try
      {
      tempe=fabs(GetMetric(InVec.as_ref()));
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

  VectorType OutVec;
  
  for( unsigned int k = 0; k < ImageDimension; k++ ) {
    if ( vnl_math_isnan(Gpos[k])  || vnl_math_isinf(Gpos[k]) ||
        vnl_math_isnan(Gsol[k])  || vnl_math_isinf(Gsol[k]) ||
         fabs(Gpos[k]) > 1.e33  || fabs(Gsol[k]) > 1.e33  ) 
    {
      OutVec.resize(ImageDimension);  OutVec.fill(0.0);  return OutVec;
    }
  }
//  OutVec=this->MetricFiniteDiff(Gpos,Gsol); // gradient direction
//  OutVec=this->GetPolynomialFitToMetric(Gpos,Gsol); // gradient direction
//  for( unsigned int k = 0; k < ImageDimension; k++ ) {
//    if ( vnl_math_isnan(OutVec[k])  || vnl_math_isinf(OutVec[k])
//      || fabs(OutVec[k]) > 1.e33 ) OutVec[k]=0.0;
//    else OutVec[k]=m_Sign*OutVec[k];
//  }
//  return OutVec;

  ParametersType parameters( m_Transform->GetNumberOfParameters() );
  typename TargetType::RegionType requestedRegion;
  TargetRadiusType regionRadius;
  typename TargetType::IndexType tindex;
  typename ReferenceType::IndexType rindex; 
  OutVec.resize(ImageDimension);

  int lobordercheck=0,hibordercheck=0;
  for( unsigned int k = 0; k < ImageDimension; k++ )
  { 
  //Set the size of the image region
    parameters[k]= Gsol[k]; // this gives the translation by the vector field 
    rindex[k] =(long)(Gpos[k]+Gsol[k]+0.5);  // where the piece of reference image currently lines up under the above translation
    tindex[k]= (long)(Gpos[k]+0.5);  // position in reference image
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
    if (lobordercheck < 0 || hibordercheck >=0 ||
       vnl_math_isnan(derivative[k])  || vnl_math_isinf(derivative[k]) ) 
    {
      OutVec[k]=0.0;
    } 
    else OutVec[k]= m_Sign*m_Gamma*derivative[k];
  }
 // NOTE : POSSIBLE THAT DERIVATIVE DIRECTION POINTS UP OR DOWN HILL!
 // IN FACT, IT SEEMS MEANSQRS AND NCC POINT IN DIFFT DIRS
  //std::cout   << " deriv " << derivative <<  " val " << measure << endl;
  //if (m_Temp !=0.0) 
  //return OutVec * exp(-1.*OutVec.magnitude()/m_Temp);
  //else 
  return OutVec;
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
    rindex[k] =(long)(InVec[k]+InVec[k+ImageDimension]+0.5);  // where the piece of reference image currently lines up under the above translation
    tindex[k]= (long)(InVec[k]+0.5);  // position in reference image
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
typename ImageMetricLoad<TReference , TTarget>::VectorType 
ImageMetricLoad<TReference , TTarget>::MetricFiniteDiff
(ImageMetricLoad<TReference , TTarget>::VectorType  Gpos,
 ImageMetricLoad<TReference , TTarget>::VectorType  Gsol ) 
{

  typename MetricBaseType::MeasureType     measure;
  ParametersType parameters( ImageDimension );
  typename TargetType::RegionType requestedRegion;
  typename TargetType::IndexType tindex;
  TargetRadiusType regionRadius;

  VectorType OutVec;
  OutVec.resize(ImageDimension);

  for( unsigned int k = 0; k < ImageDimension; k++ )
  { 
    parameters[k]= Gsol[k]; // this gives the translation by the vector field 
    tindex[k]= (long)(Gpos[k]+0.5);  // position in reference image
    if (tindex[k] > m_TarSize[k]-1 || tindex[k] < 0) tindex[k]=(long)(Gpos[k]+0.5);
    int hibordercheck=(int)tindex[k]+(int)m_MetricRadius[k]-(int)m_TarSize[k];
    int lobordercheck=(int)tindex[k]-(int)m_MetricRadius[k];
    if (hibordercheck >= 0) regionRadius[k]=m_MetricRadius[k]-(long)hibordercheck-1;
    else if (lobordercheck < 0) regionRadius[k]=m_MetricRadius[k]+(long)lobordercheck;
    else regionRadius[k]=m_MetricRadius[k];
  }
  
  unsigned int row;
  typename ImageType::IndexType difIndex[ImageDimension][2];
  
  typename MetricBaseType::MeasureType   dPixL,dPixR;
  for(row=0; row< ImageDimension;row++){
    difIndex[row][0]=tindex;
    difIndex[row][1]=tindex;
    if (tindex[row] < m_TarSize[row]-1) difIndex[row][0][row]=tindex[row]+1;
    if (tindex[row] > 0 )               difIndex[row][1][row]=tindex[row]-1;

    try
    { 
    requestedRegion.SetIndex(difIndex[row][1]);
    requestedRegion.SetSize(regionRadius);
    m_TarImage->SetRequestedRegion(requestedRegion);  
    m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );
    dPixL=m_Metric->GetValue( parameters);
    }
    catch( ... )
    {
      dPixL=0.0;
    } 
    try
    { 
    requestedRegion.SetIndex(difIndex[row][0]);
    requestedRegion.SetSize(regionRadius);
    m_TarImage->SetRequestedRegion(requestedRegion);  
    m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );
    dPixR=m_Metric->GetValue( parameters);
    }
    catch( ... )
    {
      dPixR=0.0;
    }
    
    OutVec[row]=dPixL-dPixR;
  }
  return OutVec;
}


template<class TReference,class TTarget>
typename ImageMetricLoad<TReference , TTarget>::VectorType 
ImageMetricLoad<TReference , TTarget>::GetPolynomialFitToMetric
(ImageMetricLoad<TReference , TTarget>::VectorType  Gpos,
 ImageMetricLoad<TReference , TTarget>::VectorType  Gsol ) 
{

//discrete orthogonal polynomial fitting
//see p.394-403 haralick computer and robot vision
//
//here, use chebyshev polynomials for fitting a plane to the data
//
//f(x,y,z) = a0 + a1*x + a2*y + a3*z
//
  typename MetricBaseType::MeasureType     measure;
  ParametersType parameters( ImageDimension );
  typename TargetType::RegionType requestedRegion;
  typename TargetType::IndexType tindex;
  TargetRadiusType regionRadius;

  typename ImageType::IndexType temp;

  VectorType chebycoefs; // gradient direction
  chebycoefs.resize(ImageDimension);
  double chebycoefs0=0.0;  // the constant term
  double datatotal=0.0;
  double a0norm=1.0;
  double a1norm=1.0/2.0;
  
  double met, ind1,ind2;
  double inds[3]; inds[0]=-1.0;  inds[1]=0.0;  inds[2]=1.0;

  for( unsigned int k = 0; k < ImageDimension; k++ )
  { 
    a0norm/=3.0;
    if (k < ImageDimension-1) a1norm/=3.0;
    chebycoefs[k]=0.0;
    parameters[k]= Gsol[k]; // this gives the translation by the vector field 
    tindex[k]= (long)(Gpos[k]+0.5);  // position in reference image
    if (tindex[k] > m_TarSize[k]-1 || tindex[k] < 0) tindex[k]=(long)(Gpos[k]+0.5);
    int hibordercheck=(int)tindex[k]+(int)m_MetricRadius[k]-(int)m_TarSize[k];
    int lobordercheck=(int)tindex[k]-(int)m_MetricRadius[k];
    if (hibordercheck >= 0) regionRadius[k]=m_MetricRadius[k]-(long)hibordercheck-1;
    else if (lobordercheck < 0) regionRadius[k]=m_MetricRadius[k]+(long)lobordercheck;
    else regionRadius[k]=m_MetricRadius[k];
  }
  

  if (ImageDimension==2){

  double measure[3][3];
  for(int row=-1; row< 2; row++){
  for(int col=-1; col< 2; col++){

    temp[0]=tindex[0]+(long)row;
    temp[1]=tindex[1]+(long)col;

    for (unsigned int i=0; i<ImageDimension; i++){
      if (temp[i] > m_TarSize[i]-1) temp[i]=m_TarSize[i]-1;
      else if (temp[i] < 0 ) temp[i]=0;
    }

    requestedRegion.SetIndex(temp);
    requestedRegion.SetSize(regionRadius);
    m_TarImage->SetRequestedRegion(requestedRegion);  
    m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );
    measure[row+1][col+1]=0.0;
   
    try
    { 
      measure[row+1][col+1]=m_Metric->GetValue( parameters);
    }
    catch( ... )
    {
    }
 
    
     datatotal+=measure[row+1][col+1];
  }}
  for( unsigned int cb1 = 0; cb1 < 3; cb1++ ) 
  for( unsigned int cb2 = 0; cb2 < 3; cb2++ ) 
  {
    met=measure[cb1][cb2];
    ind1=inds[cb1]*a1norm;
    ind2=inds[cb2]*a1norm;
    chebycoefs[0]+=met*ind1;
    chebycoefs[1]+=met*ind2;
  }
  }
  else if (ImageDimension == 3) {

  double measure3D[3][3][3];
  for(int row=-1; row< 2; row++){
  for(int col=-1; col< 2; col++){
  for(int z=-1; z< 2; z++){

    temp[0]=tindex[0]+(long)row;
    temp[1]=tindex[1]+(long)col;
    temp[2]=tindex[2]+(long)z;

    for (unsigned int i=0; i<ImageDimension; i++){
      if (temp[i] > m_TarSize[i]-1) temp[i]=m_TarSize[i]-1;
      else if (temp[i] < 0 ) temp[i]=0;
    }

    requestedRegion.SetIndex(temp);
    requestedRegion.SetSize(regionRadius);
    m_TarImage->SetRequestedRegion(requestedRegion);  
    m_Metric->SetFixedImageRegion( m_TarImage->GetRequestedRegion() );
    measure3D[row+1][col+1][z+1]=0.0;
   
    try
    { 
      measure3D[row+1][col+1][z+1]=m_Metric->GetValue( parameters);
    }
    catch( ... )
    {
    }
 
    
     datatotal+=measure3D[row+1][col+1][z+1];
  }}}
  for( unsigned int cb1 = 0; cb1 < 2; cb1++ ) 
  for( unsigned int cb2 = 0; cb2 < 2; cb2++ ) 
  for( unsigned int cb3 = 0; cb3 < 2; cb3++ ) 
  {
    chebycoefs[0]+=measure3D[cb1][cb2][cb3]*inds[cb1]*a1norm;
    chebycoefs[1]+=measure3D[cb1][cb2][cb3]*inds[cb2]*a1norm;
    chebycoefs[2]+=measure3D[cb1][cb2][cb3]*inds[cb3]*a1norm;
  }
  }
  
  chebycoefs0=a0norm*datatotal;
//  std::cout << " cb " << chebycoefs << std::endl;
  return chebycoefs;

}


/*
template<class TReference,class TTarget>
void
ImageMetricLoad<TReference , TTarget>::InitializeGradientImage() 
{

  typedef itk::ImageRegionIteratorWithIndex<GradientImageType>         gIterator; 

  GradientImageType::RegionType metricGradientImageRegion;
  metricGradientImageRegion.SetSize( m_TarSize );
  m_MetricGradientImage = GradientImageType::New();
  m_MetricGradientImage->SetLargestPossibleRegion( metricGradientImageRegion );
  m_MetricGradientImage->SetBufferedRegion( metricGradientImageRegion );
  m_MetricGradientImage->Allocate(); 
 
  gIterator metricGradientImageIter( m_MetricGradientImage, metricGradientImageRegion );
  metricGradientImageIter.GoToBegin();
  
  GradientPixelType disp;
  for (int i=0; i<ImageDimension; i++) disp[i]=0.;
  for( ; !metricGradientImageIter.IsAtEnd(); ++metricGradientImageIter )
  {
    metricGradientImageIter.Set(disp);
  }

}
*/

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
