/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectToImageRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkEllipseSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include <itkSpatialObjectToImageFilter.h>
#include <itkImageToSpatialObjectRegistrationMethod.h>
#include <itkImageToSpatialObjectMetric.h>
#include "itkLinearInterpolateImageFunction.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkEuler2DTransform.h"
#include "itkDiscreteGaussianImageFilter.h"
#include <itkNormalVariateGenerator.h> 
#include <itkCommand.h>

namespace itk
{

/** Iteration callback */
template < class TOptimizer >
class IterationCallback : public Command 
{

public:
  typedef IterationCallback   Self;
  typedef itk::Command  Superclass;
  typedef itk::SmartPointer<Self>  Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;
  
  itkTypeMacro( IterationCallback, Superclass );
  itkNewMacro( Self );

  /** Type defining the optimizer */
  typedef    TOptimizer     OptimizerType;


  /** Set Optimizer */
  void SetOptimizer( OptimizerType * optimizer )
  { 
    m_Optimizer = optimizer;
    m_Optimizer->AddObserver( itk::IterationEvent(), this );
  }


  /** Execute method will print data at each iteration */
  void Execute(itk::Object *caller, const itk::EventObject & event)
  {
    Execute( (const itk::Object *)caller, event);
  }

  void Execute(const itk::Object *, const itk::EventObject & event)
  {
    if( typeid( event ) == typeid( itk::StartEvent ) )
    {
      std::cout << std::endl << "Position              Value";
      std::cout << std::endl << std::endl;
    }    
    else if( typeid( event ) == typeid( itk::IterationEvent ) )
    {
      std::cout << "#" << m_Optimizer->GetCurrentIteration() 
                << " Current parameters = " << m_Optimizer->GetCurrentPosition();
    }
    else if( typeid( event ) == typeid( itk::EndEvent ) )
    {
      std::cout << std::endl << std::endl;
      std::cout << "After " << m_Optimizer->GetCurrentIteration();
      std::cout << "  iterations " << std::endl;
      std::cout << "Solution is    = " << m_Optimizer->GetCurrentPosition();
      std::cout << std::endl;
    }

  }

protected:
  IterationCallback() {};
  WeakPointer<OptimizerType>   m_Optimizer;
 
};

/** Cost Function */
template <typename TFixedImage, typename TMovingSpatialObject>
class SimpleImageToSpatialObjectMetric : public ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>
{
public:

  /** Standard class typedefs. */
  typedef SimpleImageToSpatialObjectMetric  Self;
  typedef ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>  
                                                                     Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef Point<double,2>   PointType;
  typedef std::list<PointType> PointListType;
  typedef TMovingSpatialObject MovingSpatialObjectType;
  typedef typename Superclass::ParametersType ParametersType;
  typedef typename Superclass::DerivativeType DerivativeType;
  typedef typename Superclass::MeasureType    MeasureType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleImageToSpatialObjectMetric, ImageToSpatialObjectMetric);

  enum { SpaceDimension = 3 };

  /** Connect the MovingSpatialObject */
  void SetMovingSpatialObject( const MovingSpatialObjectType * object)
  {
    if(!m_FixedImage)
    {
      std::cout << "Please set the image before the moving spatial object" << std::endl;
      return;
    }
    m_MovingSpatialObject = object;
    m_PointList.clear();
    typedef itk::ImageRegionConstIteratorWithIndex<TFixedImage> myIteratorType;

    myIteratorType it(m_FixedImage,m_FixedImage->GetLargestPossibleRegion());

    itk::Point<double,2> point;

    while(!it.IsAtEnd())
    {
      for(unsigned int i=0;i<ObjectDimension;i++)
      {
        point[i]=it.GetIndex()[i];
      }
      
      if(m_MovingSpatialObject->IsInside(point,99999))
      { 
        m_PointList.push_back(point);
      }    
      ++it;
    }

    std::cout << "Number of points in the metric = " << static_cast<unsigned long>( m_PointList.size() ) << std::endl;
  }


  unsigned int GetNumberOfParameters(void) const  {return SpaceDimension;};

  /** Get the Derivatives of the Match Measure */
  void GetDerivative(const ParametersType&, DerivativeType&) const
  {
    return;
  }

  /** Get the Value for SingleValue Optimizers */
  MeasureType    GetValue( const ParametersType & parameters ) const
  {   
    double value;
    m_Transform->SetParameters(parameters);
    
    PointListType::const_iterator it = m_PointList.begin();
    
    Index<2> index;
    value = 0;
    while(it != m_PointList.end())
    {
      PointType transformedPoint = m_Transform->TransformPoint(*it);
      m_FixedImage->TransformPhysicalPointToIndex(transformedPoint,index);
      if(index[0]>0L && index[1]>0L
        && index[0]< static_cast<signed long>(m_FixedImage->GetLargestPossibleRegion().GetSize()[0])
        && index[1]< static_cast<signed long>(m_FixedImage->GetLargestPossibleRegion().GetSize()[1])
        )
      {
        value += m_FixedImage->GetPixel(index);
      }
      it++;
    }
    return value;
  }

  /** Get Value and Derivatives for MultipleValuedOptimizers */
  void GetValueAndDerivative( const ParametersType & parameters,
       MeasureType & Value, DerivativeType  & Derivative ) const
  {
    Value = this->GetValue(parameters);
    this->GetDerivative(parameters,Derivative);
  }

private:

  PointListType m_PointList;


};

} // end namespace itk


/** test */
int itkSpatialObjectToImageRegistrationTest(int, char* [] )
{
  typedef itk::GroupSpatialObject<2>   GroupType;
  typedef itk::EllipseSpatialObject<2> EllipseType;
  typedef itk::LineSpatialObject<2>    LineType;

  // Create a group with 3 ellipses linked by lines.
  EllipseType::Pointer ellipse1 = EllipseType::New();
  EllipseType::Pointer ellipse2 = EllipseType::New();
  EllipseType::Pointer ellipse3 = EllipseType::New();

  // Set the radius
  ellipse1->SetRadius(10);
  ellipse2->SetRadius(10);
  ellipse3->SetRadius(10);

  // Place each ellipse at the right position to form a triangle
  EllipseType::TransformType::OffsetType offset;
  offset[0]=100;
  offset[1]=40;
  ellipse1->GetTransform()->SetOffset(offset);
  ellipse1->ComputeGlobalTransform();
 
  offset[0]=40;
  offset[1]=150;
  ellipse2->GetTransform()->SetOffset(offset);
  ellipse2->ComputeGlobalTransform();

  offset[0]=150;
  offset[1]=150;
  ellipse3->GetTransform()->SetOffset(offset);
  ellipse3->ComputeGlobalTransform();

  GroupType::Pointer group = GroupType::New();
  group->AddSpatialObject(ellipse1);
  group->AddSpatialObject(ellipse2);
  group->AddSpatialObject(ellipse3);

  typedef itk::Image<double,2> ImageType;

  typedef itk::SpatialObjectToImageFilter<GroupType,ImageType> SpatialObjectToImageFilterType;
  SpatialObjectToImageFilterType::Pointer imageFilter = SpatialObjectToImageFilterType::New();
  imageFilter->SetInput(group);
  ImageType::SizeType size;
  size[0]=200;
  size[1]=200;
  imageFilter->SetSize(size);
  imageFilter->Update();

  ImageType::Pointer image = imageFilter->GetOutput();

  // blurr the image to have a global maximum
  typedef itk::DiscreteGaussianImageFilter<ImageType,ImageType> GaussianFilterType;
  GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(image);
  const double variance = 20;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  image = gaussianFilter->GetOutput();

  typedef itk::ImageToSpatialObjectRegistrationMethod<ImageType,GroupType>  RegistrationType;
  RegistrationType::Pointer registration = RegistrationType::New();

  typedef itk::SimpleImageToSpatialObjectMetric<ImageType,GroupType> MetricType;
  MetricType::Pointer metric = MetricType::New();

  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  typedef itk::OnePlusOneEvolutionaryOptimizer  OptimizerType;
  OptimizerType::Pointer optimizer  = OptimizerType::New();

  typedef itk::Euler2DTransform<> TransformType;
  TransformType::Pointer transform = TransformType::New();

  registration->SetFixedImage(image);
  registration->SetMovingSpatialObject(group);
  registration->SetTransform(transform);
  registration->SetInterpolator(interpolator.GetPointer());


  /** Setup the optimizer */
  TransformType::ParametersType m_ParametersScale;
  m_ParametersScale.resize(3);

  m_ParametersScale[0]=1000; // angle scale

  for(unsigned int i=1;i<3;i++)
  {
    m_ParametersScale[i] = 2; // offset scale
  }

  optimizer->SetScales( m_ParametersScale );

  TransformType::ParametersType initialParameters;
  initialParameters.resize(3);

  initialParameters[0] = 0.2; // angle
  initialParameters[1] = 7; // offset 
  initialParameters[2] = 6; // offset 

  std::cout << "Initial Parameters  : " << initialParameters << std::endl;

  registration->SetInitialTransformParameters(initialParameters);
  optimizer->MaximizeOn();

  itk::Statistics::NormalVariateGenerator::Pointer generator 
                      = itk::Statistics::NormalVariateGenerator::New();
  generator->Initialize(12345);

  optimizer->SetNormalVariateGenerator(generator);
  optimizer->Initialize(10);
  optimizer->SetMaximumIteration(1000);

  typedef itk::IterationCallback<OptimizerType> IterationCallbackType;
  IterationCallbackType::Pointer callback = IterationCallbackType::New();
  callback->SetOptimizer( optimizer );

  registration->SetOptimizer(optimizer);
  registration->SetMetric(metric);

  registration->StartRegistration();

  RegistrationType::ParametersType finalParameters 
                                 = registration->GetLastTransformParameters();

  std::cout << "Final Solution is : " << finalParameters << std::endl;

  for(unsigned int i=0;i<3;i++)
  {
    if(finalParameters[i]>1) // if we are not within 1 pixel the registration fails
    {
      return EXIT_FAILURE;
    }
  }

  std::cout<<"Test Succeed!"<<std::endl;
  return EXIT_SUCCESS;

}
