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

#include "itkEllipseSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkImageToSpatialObjectRegistrationMethod.h"
#include "itkOnePlusOneEvolutionaryOptimizer.h"
#include "itkEuler2DTransform.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkNormalVariateGenerator.h"
#include "itkTestingMacros.h"

namespace itk
{

/** \class Iteration callback */
template < typename TOptimizer >
class IterationCallback : public Command
{
public:
  using Self = IterationCallback;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  itkTypeMacro( IterationCallback, Superclass );
  itkNewMacro( Self );

  /** Type defining the optimizer */
  using OptimizerType = TOptimizer;


  /** Set Optimizer */
  void SetOptimizer( OptimizerType * optimizer )
    {
      m_Optimizer = optimizer;
      m_Optimizer->AddObserver( itk::IterationEvent(), this );
    }


  /** Execute method will print data at each iteration */
  void Execute(itk::Object *caller, const itk::EventObject & event) override
    {
      Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object *, const itk::EventObject & event) override
    {
      if( typeid( event ) == typeid( itk::StartEvent ) )
        {
        std::cout << std::endl << "Position              Value";
        std::cout << std::endl << std::endl;
        }
      else if( typeid( event ) == typeid( itk::IterationEvent ) )
        {
        std::cout << "#" << m_Optimizer->GetCurrentIteration()
                  << " Current parameters = " << m_Optimizer->GetCurrentPosition()
                  << std::endl;
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
  IterationCallback() = default;
  WeakPointer<OptimizerType>   m_Optimizer;

};

/** \class Cost Function */
template <typename TFixedImage, typename TMovingSpatialObject>
class SimpleImageToSpatialObjectMetric : public ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>
{
public:

  /** Standard class type aliases. */
  using Self = SimpleImageToSpatialObjectMetric;
  using Superclass =
      ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using PointType = Point<double,2>;
  using PointListType = std::list<PointType>;
  using MovingSpatialObjectType = TMovingSpatialObject;
  using ParametersType = typename Superclass::ParametersType;
  using DerivativeType = typename Superclass::DerivativeType;
  using MeasureType = typename Superclass::MeasureType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleImageToSpatialObjectMetric, ImageToSpatialObjectMetric);

  enum { SpaceDimension = 3 };

  /** Connect the MovingSpatialObject */
  void SetMovingSpatialObject( const MovingSpatialObjectType * object) override
    {
      if(!this->m_FixedImage)
        {
        std::cout << "Please set the image before the moving spatial object" << std::endl;
        return;
        }
      this->m_MovingSpatialObject = object;
      m_PointList.clear();
      using myIteratorType = itk::ImageRegionConstIteratorWithIndex<TFixedImage>;

      myIteratorType it(this->m_FixedImage,this->m_FixedImage->GetLargestPossibleRegion());

      itk::Point<double,2> point;

      while(!it.IsAtEnd())
        {
        for(unsigned int i=0;i<Self::ObjectDimension;i++)
          {
          point[i]=it.GetIndex()[i];
          }

        if(this->m_MovingSpatialObject->IsInsideInWorldSpace(point,99999))
          {
          m_PointList.push_back(point);
          }
        ++it;
        }

      std::cout << "Number of points in the metric = " << static_cast<unsigned long>( m_PointList.size() ) << std::endl;
    }


  /** Get the Derivatives of the Match Measure */
  void GetDerivative(const ParametersType&, DerivativeType&) const override
    {
      return;
    }

  /** Get the Value for SingleValue Optimizers */
  MeasureType    GetValue( const ParametersType & parameters ) const override
    {
      double value;
      this->m_Transform->SetParameters(parameters);

      auto it = m_PointList.begin();

      Index<2> index;
      value = 0;
      while(it != m_PointList.end())
        {
        PointType transformedPoint = this->m_Transform->TransformPoint(*it);
        this->m_FixedImage->TransformPhysicalPointToIndex(transformedPoint,index);
        if(index[0]>0L && index[1]>0L
           && index[0]< static_cast<signed long>(this->m_FixedImage->GetLargestPossibleRegion().GetSize()[0])
           && index[1]< static_cast<signed long>(this->m_FixedImage->GetLargestPossibleRegion().GetSize()[1])
          )
          {
          value += this->m_FixedImage->GetPixel(index);
          }
        ++it;
        }
      return value;
    }

  /** Get Value and Derivatives for MultipleValuedOptimizers */
  void GetValueAndDerivative( const ParametersType & parameters,
                              MeasureType & Value, DerivativeType  & Derivative ) const override
    {
      Value = this->GetValue(parameters);
      this->GetDerivative(parameters,Derivative);
    }

private:

  PointListType m_PointList;


};

} // end namespace itk


/** test */
int itkImageToSpatialObjectRegistrationTest(int, char* [] )
{
  using GroupType = itk::GroupSpatialObject<2>;
  using EllipseType = itk::EllipseSpatialObject<2>;

  // Create a group with 3 ellipses linked by lines.
  EllipseType::Pointer ellipse1 = EllipseType::New();
  EllipseType::Pointer ellipse2 = EllipseType::New();
  EllipseType::Pointer ellipse3 = EllipseType::New();

  // Set the radius
  ellipse1->SetRadiusInObjectSpace(10);
  ellipse2->SetRadiusInObjectSpace(10);
  ellipse3->SetRadiusInObjectSpace(10);

  // Place each ellipse at the right position to form a triangle
  EllipseType::TransformType::OffsetType offset;
  offset[0]=100;
  offset[1]=40;
  ellipse1->SetCenterInObjectSpace(offset);
  ellipse1->Update();

  offset[0]=40;
  offset[1]=150;
  ellipse2->SetCenterInObjectSpace(offset);
  ellipse2->Update();

  offset[0]=150;
  offset[1]=150;
  // Moving the object using the ObjectToParentTransform should
  //   be equivalent to setting its CenterInObjectSpace
  ellipse3->GetObjectToParentTransform()->SetOffset(offset);
  ellipse3->Update();

  GroupType::Pointer group = GroupType::New();
  group->AddChild(ellipse1);
  group->AddChild(ellipse2);
  group->AddChild(ellipse3);
  group->Update();

  using ImageType = itk::Image<double,2>;

  using SpatialObjectToImageFilterType
    = itk::SpatialObjectToImageFilter<GroupType,ImageType>;
  SpatialObjectToImageFilterType::Pointer imageFilter
    = SpatialObjectToImageFilterType::New();
  imageFilter->SetInput(group);
  ImageType::SizeType size;
  size[0]=200;
  size[1]=200;
  imageFilter->SetSize(size);
  imageFilter->Update();

  ImageType::Pointer image = imageFilter->GetOutput();

  // blurr the image to have a global maximum
  using GaussianFilterType
    = itk::DiscreteGaussianImageFilter<ImageType,ImageType>;
  GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(image);
  constexpr double variance = 20;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  image = gaussianFilter->GetOutput();

  using RegistrationType
    = itk::ImageToSpatialObjectRegistrationMethod<ImageType,GroupType>;
  RegistrationType::Pointer registration = RegistrationType::New();

  EXERCISE_BASIC_OBJECT_METHODS( registration,
    ImageToSpatialObjectRegistrationMethod,
    ProcessObject );

  using MetricType = itk::SimpleImageToSpatialObjectMetric<ImageType,GroupType>;
  MetricType::Pointer metric = MetricType::New();

  std::cout << "metric = " << metric << std::endl;

  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType,double>;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  using OptimizerType = itk::OnePlusOneEvolutionaryOptimizer;
  OptimizerType::Pointer optimizer  = OptimizerType::New();

  using TransformType = itk::Euler2DTransform<>;
  TransformType::Pointer transform = TransformType::New();

  metric->SetTransform(transform);
  std::cout << "Number of Parameters  : "<< metric->GetNumberOfParameters() << std::endl;
  TEST_EXPECT_EQUAL( metric->GetNumberOfParameters(), 3 );

  bool catching;
  try
    {
    catching = false;
    registration->Update();
    }
  catch(...)
    {
    catching = true;
    }

  if(!catching)
    {
    std::cout<<"Test failed!"<<std::endl;
    return EXIT_FAILURE;
    }

  registration->SetFixedImage(image);

  try
    {
    catching = false;
    registration->Update();
    }
  catch(...)
    {
    catching = true;
    }

  if(!catching)
    {
    std::cout<<"Test failed!"<<std::endl;
    return EXIT_FAILURE;
    }

  registration->SetMovingSpatialObject(group);

  try
    {
    catching = false;
    registration->Update();
    }
  catch(...)
    {
    catching = true;
    }

  if(!catching)
    {
    std::cout<<"Test failed!"<<std::endl;
    return EXIT_FAILURE;
    }

  registration->SetMetric(metric);

  try
    {
    catching = false;
    registration->Update();
    }
  catch(...)
    {
    catching = true;
    }

  if(!catching)
    {
    std::cout<<"Test failed!"<<std::endl;
    return EXIT_FAILURE;
    }

  /** Setup the optimizer */
  TransformType::ParametersType m_ParametersScale;
  m_ParametersScale.set_size(3);

  m_ParametersScale[0]=100; // angle scale

  for(unsigned int i=1;i<3;i++)
    {
    m_ParametersScale[i] = 1; // offset scale
    }

  optimizer->SetScales( m_ParametersScale );

  TransformType::ParametersType initialParameters;
  initialParameters.set_size(3);

  initialParameters[0] = 0.2; // angle
  initialParameters[1] = 7; // offset
  initialParameters[2] = 6; // offset

  std::cout << "Initial Parameters  : "<< initialParameters << std::endl;

  registration->SetInitialTransformParameters(initialParameters);
  optimizer->MaximizeOn();

  itk::Statistics::NormalVariateGenerator::Pointer generator
    = itk::Statistics::NormalVariateGenerator::New();
  generator->Initialize(12345);

  optimizer->SetNormalVariateGenerator(generator);
  optimizer->Initialize( 1.02, 1.1 );
  optimizer->SetEpsilon( 0.01 );
  optimizer->SetMaximumIteration( 500 );

  using IterationCallbackType = itk::IterationCallback<OptimizerType>;
  IterationCallbackType::Pointer callback = IterationCallbackType::New();
  callback->SetOptimizer( optimizer );

  registration->SetOptimizer(optimizer);

  try
    {
    catching = false;
    registration->Update();
    }
  catch(...)
    {
    catching = true;
    }

  if(!catching)
    {
    std::cout<<"Test failed!"<<std::endl;
    return EXIT_FAILURE;
    }


  registration->SetTransform(transform);


  try
    {
    catching = false;
    registration->Update();
    }
  catch(...)
    {
    catching = true;
    }

  if(!catching)
    {
    std::cout<<"Test failed!"<<std::endl;
    return EXIT_FAILURE;
    }

  registration->SetInterpolator(interpolator);

  registration->Update();

  RegistrationType::ParametersType finalParameters
    = registration->GetLastTransformParameters();

  std::cout << "Final Solution is : " << finalParameters << std::endl;

  for(unsigned int i=0;i<3;i++)
    {
    if(finalParameters[i]>1) // if we are not within 1 pixel the registration fails
      {
      std::cout<<"Test failed!"<<std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout<<"Test Succeed!"<<std::endl;
  return EXIT_SUCCESS;

}
