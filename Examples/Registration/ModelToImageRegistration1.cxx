/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ModelToImageRegistration1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
//  This example illustrates the use of the \doxygen{SpatialObject} as a
//  component of the registration framework in order to perfom model based
//  registration. The current example creates a geometrical model composed of
//  several ellipses. Then, it uses the model to produce a synthetic binary
//  image of the ellipses. Next, it introduces perturbations on the position
//  and shape of the model, and finally it use the perturbed version as input
//  of a registration process. A metric is defined to evaluate the fitness
//  between the geometric model and the image.
// 
// Software Guide : EndLatex 



//  Software Guide : BeginLatex
//  
//  Let's look first at the elements required for supporting the
//  \doxygen{SpatialObject}. In this example we use the
//  \doxygen{EllipseSpatialObject} as the basic shape components and we use the
//  \doxygen{GroupSpatialObject} to group them together as a representation of
//  a more complex shape. Their respective headers are included below.
//
//  \index{itk::EllipseSpatialObject!header}
//  \index{itk::GroupSpatialObject!header}
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkEllipseSpatialObject.h"
#include "itkGroupSpatialObject.h"
//  Software Guide : EndCodeSnippet 





//  Software Guide : BeginLatex
//  
//  In order to generate the initial syntethic image of the ellipses, we use
//  the \doxygen{SpatialObjectToImageFilter} which basically test for every
//  pixel in the image whether it is \emph{inside} or \emph{outside} of the
//  geometric model.
//
//  \index{itk::SpatialObjectToImageFilter!header}
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include <itkSpatialObjectToImageFilter.h>
//  Software Guide : EndCodeSnippet 


#include <itkImageToSpatialObjectRegistrationMethod.h>





//  Software Guide : BeginLatex
//  
//  A metric is defined to evaluate the fitness between the
//  \doxygen{SpatialObject} and the \doxygen{Image}. The base class for this
//  type of metric is the \doxygen{ImageToSpatialObjectMetric} whose header is
//  included below.
//
//  \index{itk::ImageToSpatialObjectMetric!header}
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include <itkImageToSpatialObjectMetric.h>
//  Software Guide : EndCodeSnippet 



//  Software Guide : BeginLatex
//  
//  As in previous registration problems, it arises here that we have to
//  evaluate the image intensity in non-grid positions. The
//  \doxygen{LinearInterpolateImageFunction} is used here for this purpose.
//
//  \index{itk::LinearInterpolateImageFunction!header}
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkLinearInterpolateImageFunction.h"
//  Software Guide : EndCodeSnippet 




//  Software Guide : BeginLatex
//  
//  The \doxygen{SpatialObject} is mapped from its own space into the image
//  space by using a \doxygen{Transform}. In the particular case of this
//  example, we use the \doxygen{Euler2DTransform}.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkEuler2DTransform.h"
//  Software Guide : EndCodeSnippet 





//  Software Guide : BeginLatex
//  
//  Registration is fundamentally an optimization problem. Here we include the
//  optimizer to be used for searching the parameter space and identify the
//  best combination that will map the shape model on top of the image. The
//  optimizer used in this example is the
//  \doxygen{OnePlusOneEvaolutionaryOptimizer} which implements an
//  \emph{Evolutionary Algorithm}.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkOnePlusOneEvolutionaryOptimizer.h"
//  Software Guide : EndCodeSnippet 




#include "itkDiscreteGaussianImageFilter.h"
#include <itkNormalVariateGenerator.h> 
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


//  Software Guide : BeginLatex
//  
//  As in previous registration examples, it is fundamental to be able to track
//  the evolution of the optimizer as it walks through the parameter space.
//  This is done by using the \code{Command/Observer} paradigm.  The following
//  lines of code implement the \doxygen{Command} observer that will monitor
//  the progress of the registration process. The code is quite similar to what
//  we have used in previous registration examples.
//
//  \index{Model to Image Registration!Observer}
// 
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
#include "itkCommand.h"
template < class TOptimizer >
class IterationCallback : public itk::Command 
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
//  Software Guide : EndCodeSnippet 


protected:
  IterationCallback() {};
  itk::WeakPointer<OptimizerType>   m_Optimizer;
 
};




//  Software Guide : BeginLatex
//  
//  The Metric that evaluate the match between the \doxygen{SpatialObject} and
//  the \doxygen{Image} is a critical component of the registration process.
//  The properties of smoothness and regularity define the dificulty of the
//  task assigned to the optimizer. In this case, we use a very robust
//  optimizer that should be able to find its way even in the most
//  discontinuous cost functions. The metric to be implemented should derive
//  from the \doxygen{ImageToSpatialObjectMetric} class.
//
//  The following section of code implements a simple metric which computes the
//  sum of the pixels that are inside the spatial object. In fact, the maximum
//  of the metric is obtained when the model and the image are aligned. The
//  metric is templated over the tyep of the \doxygen{SpatialObject} and the
//  type of the \doxygen{Image}.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
template <typename TFixedImage, typename TMovingSpatialObject>
class SimpleImageToSpatialObjectMetric : 
    public itk::ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>
{
//  Software Guide : EndCodeSnippet 
public:

  /** Standard class typedefs. */
  typedef SimpleImageToSpatialObjectMetric  Self;
  typedef itk::ImageToSpatialObjectMetric<TFixedImage,TMovingSpatialObject>  
                                                                     Superclass;
  typedef itk::SmartPointer<Self>   Pointer;
  typedef itk::SmartPointer<const Self>  ConstPointer;

  typedef itk::Point<double,2>   PointType;
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

    std::cout << "Number of points in the metric = " << m_PointList.size() << std::endl;
  }

  unsigned int GetNumberOfParameters(void) const  {return SpaceDimension;};

  /** Get the Derivatives of the Match Measure */
  void GetDerivative( const ParametersType & parameters,
                                    DerivativeType & derivative ) const
  {
    return;
  }


  //  Software Guide : BeginLatex
  //  
  //  The most fundamental part of the metric is its \code{GetValue()} method.
  //  It is in this method that the fitness value is computed. In our current
  //  example, the fitness is computed over the points of the
  //  \doxygen{SpatialObject}. For each point, its coordinates are mapped
  //  through the transform into the image space. The resulting point is used
  //  to evaluate the image and the obtained value is cummulated in a sum.
  //  Since the image is binary, the optimial value of the sum will result when
  //  all the \doxygen{SpatialObject} points are mapped on the white regions of
  //  the image. Note that the argument for the \code{GetValue()} method is the
  //  array of parameters of the transform. 
  // 
  //  \index{ImageToSpatialObjectMetric!GetValue()}
  //
  //  Software Guide : EndLatex 


  /** Get the Value for SingleValue Optimizers */

  //  Software Guide : BeginCodeSnippet
  MeasureType    GetValue( const ParametersType & parameters ) const
  {   
    double value;
    m_Transform->SetParameters( parameters );
    
    PointListType::const_iterator it = m_PointList.begin();
    
    itk::Index<2> index;
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
  //  Software Guide : EndCodeSnippet 

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




//  Software Guide : BeginLatex
//  
//  Having defined all the registration components we are ready to put the
//  pieces together in the frame of a registration method.
//
//  Software Guide : EndLatex 


int main( int argc, char ** argv )
{


  //  Software Guide : BeginLatex
  //  
  //  First we instantiate the types of the \doxygen{GroupSpatialObject} and
  //  \doxygen{EllipseSpatialObject}.These two objects are parameterized by the
  //  dimension of the space. In our current example a $2D$ instantiation is
  //  made.
  //
  //  \index{GroupSpatialObject!Instantiation}
  //  \index{EllipseSpatialObject!Instantiation}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::GroupSpatialObject< 2 >     GroupType;
  
  typedef itk::EllipseSpatialObject< 2 >   EllipseType;
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //  
  //  The image type is instantiated in the following lines using the a type
  //  and the space dimension. Two image types are created here. First a
  //  \code{float} image that will be used as the target for registration.
  //  Second, a \code{unsigned char} image that will be used for writing the
  //  final position of the model once registration is done.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::Image< float,         2 >      ImageType;
  typedef itk::Image< unsigned char, 2 >      OutputImageType;
  //  Software Guide : EndCodeSnippet 





  //  Software Guide : BeginLatex
  //  
  //  The following casting and writer filter types will be used for saving the
  //  registered model in to a file. Note that, what we are actually saving
  //  here is the digitalization of the \doxygen{SpatialObject} in the form of
  //  a binary image}. We could imagine a more sophisticated approach in which
  //  a geometric file format could have been used to save the parameters of
  //  the spatial object and its associated transform.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::CastImageFilter< 
                        ImageType,
                        OutputImageType > CastFilterType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  //  Software Guide : EndCodeSnippet 





  //  Software Guide : BeginLatex
  //  
  //  Here is where the fun begins !. We create int the following lines the
  //  \doxygen{EllipseSpatialObject}s using their \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}. These lines will create
  //  three ellipses linked by lines.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  EllipseType::Pointer ellipse1 = EllipseType::New();
  EllipseType::Pointer ellipse2 = EllipseType::New();
  EllipseType::Pointer ellipse3 = EllipseType::New();
  //  Software Guide : EndCodeSnippet  



  //  Software Guide : BeginLatex
  //  
  //  Each type of \doxygen{SpatialObject} has particular parameters allowing
  //  to tailor its shape. In the case of the \doxygen{EllipseSpatialObject},
  //  the \code{SetRadius()} is used to define the ellipses size.
  //
  //  \index{itk::EllipseSpatialObject!SetRadius()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  ellipse1->SetRadius(  10.0  );
  ellipse2->SetRadius(  10.0  );
  ellipse3->SetRadius(  10.0  );
  //  Software Guide : EndCodeSnippet 



  //  Software Guide : BeginLatex
  //  
  //  By default the ellipses are created centered in space. We use the
  //  following lines of code to place the ellipses arranged in a triangle.
  //  The spatial transform associated intrinsically with the object is
  //  accessed by the \code{GetTransform()} method. This transform can define a
  //  translation in space with the \code{SetOffset()} method.  We take
  //  advantage of this mechanism for placing the ellipses in particular points
  //  in space.
  //
  //  Software Guide : EndLatex 

  // Place each ellipse at the right position to form a triangle

  //  Software Guide : BeginCodeSnippet
  EllipseType::TransformType::OffsetType offset;
  offset[ 0 ] = 100.0;
  offset[ 1 ] =  40.0;

  ellipse1->GetTransform()->SetOffset(offset);
  ellipse1->ComputeGlobalTransform();
 
  offset[ 0 ] =  40.0;
  offset[ 1 ] = 150.0;
  ellipse2->GetTransform()->SetOffset(offset);
  ellipse2->ComputeGlobalTransform();

  offset[ 0 ] = 150.0;
  offset[ 1 ] = 150.0;
  ellipse3->GetTransform()->SetOffset(offset);
  ellipse3->ComputeGlobalTransform();
  //  Software Guide : EndCodeSnippet 


  //  Software Guide : BeginLatex
  //  
  //  Note that after a change has been made in the transform, the
  //  \doxygen{SpatialObject} invokes the method
  //  \code{ComputeGlobalTransform()} in order to update its global transform.
  //  The reason for doing this is that \doxygen{SpatialObject}s can be
  //  arranged in hierarchies. It is then possible to change the position of a
  //  set of spatial objects by moving the parent of the group.
  //
  //  Software Guide : EndLatex 

  
  



  //  Software Guide : BeginLatex
  //  
  //  Now we add the three \doxygen{EllipseSpatialObject} to a
  //  \doxygen{GroupSpatialObject} that will be passed later on to the
  //  registration method.
  //
  //  \index{itk::GroupSpatialObject!New()}
  //  \index{itk::GroupSpatialObject!Pointer}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  GroupType::Pointer group = GroupType::New();
  
  group->AddSpatialObject( ellipse1 );
  group->AddSpatialObject( ellipse2 );
  group->AddSpatialObject( ellipse3 );
  //  Software Guide : EndCodeSnippet 





  //  Software Guide : BeginLatex
  //  
  //  Having the geometrical model ready, we proceed to generate the binary
  //  image representing the imprint of the space occupied by the ellipses.
  //  The \doxygen{SpatialObjectToImageFilter} is used to that end. Note that
  //  this filter is instantiated over the spatial object used and the image
  //  type to be generated.
  //
  //  \index{itk::SpatialObjectToImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  typedef itk::SpatialObjectToImageFilter<  
                                   GroupType,
                                   ImageType >   SpatialObjectToImageFilterType;
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //  
  //  With the defined type, we construct a filter using the \code{New()}
  //  method. The newly created filter is assigned to a \code{SmartPointer}.
  //
  //  \index{itk::SpatialObjectToImageFilter!New()}
  //  \index{itk::SpatialObjectToImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  SpatialObjectToImageFilterType::Pointer imageFilter = 
                                     SpatialObjectToImageFilterType::New();
  //  Software Guide : EndCodeSnippet 
  


  //  Software Guide : BeginLatex
  //  
  //  The \doxygen{GroupSpatialObject} is passed as input to the filter.
  //
  //  \index{itk::SpatialObjectToImageFilter!SetInput()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  imageFilter->SetInput(  group  );
  //  Software Guide : EndCodeSnippet 



  //  Software Guide : BeginLatex
  //  
  //  The \doxygen{SpatialObjectToImageFilter} acts as a resampling filter.
  //  Henceforth it requires the user to define the size of the desired output
  //  image. This is specified with the \code{SetSize()} method.
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  ImageType::SizeType size;
  size[ 0 ] = 200;
  size[ 1 ] = 200;
 
  imageFilter->SetSize( size );
  //  Software Guide : EndCodeSnippet 




  //  Software Guide : BeginLatex
  //  
  //  Finally we trigger the execution of the filter by calling the
  //  \code{Update()} method.
  //
  //  \index{itk::SpatialObjectToImageFilter!Update()}
  //
  //  Software Guide : EndLatex 

  //  Software Guide : BeginCodeSnippet
  imageFilter->Update();
  //  Software Guide : EndCodeSnippet 




  ImageType::Pointer image = imageFilter->GetOutput();

  // Blurr the image to obtain a global maximum
  typedef itk::DiscreteGaussianImageFilter<ImageType,ImageType> GaussianFilterType;
  GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();

  gaussianFilter->SetInput(image);
  const double variance = 20;
  gaussianFilter->SetVariance(variance);
  gaussianFilter->Update();
  image = gaussianFilter->GetOutput();

  // Registration typedefs
  typedef itk::ImageToSpatialObjectRegistrationMethod<ImageType,GroupType>  RegistrationType;
  RegistrationType::Pointer registration = RegistrationType::New();

  typedef SimpleImageToSpatialObjectMetric<ImageType,GroupType> MetricType;
  MetricType::Pointer metric = MetricType::New();

  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  typedef itk::OnePlusOneEvolutionaryOptimizer  OptimizerType;
  OptimizerType::Pointer optimizer  = OptimizerType::New();

  typedef itk::Euler2DTransform<> TransformType;
  TransformType::Pointer transform = TransformType::New();

  // Setup the registration framework
  registration->SetFixedImage(image);
  registration->SetMovingSpatialObject(group);
  registration->SetTransform(transform);
  registration->SetInterpolator(interpolator.GetPointer());

  // The OnePlusOneEvaolutionaryOptimizer requires a random generator
  itk::Statistics::NormalVariateGenerator::Pointer generator 
                      = itk::Statistics::NormalVariateGenerator::New();

  generator->Initialize(12345);

  optimizer->SetNormalVariateGenerator(generator);
  optimizer->Initialize(10);
  optimizer->SetMaximumIteration(1000);


  // Initialize the optimizer
  TransformType::ParametersType m_ParametersScale;
  m_ParametersScale.resize(3);

  m_ParametersScale[0]=1000; // angle scale

  for(unsigned int i=1;i<3;i++)
  {
    m_ParametersScale[i] = 2; // offset scale
  }

  optimizer->SetScales( m_ParametersScale );
  typedef IterationCallback<OptimizerType> IterationCallbackType;
  IterationCallbackType::Pointer callback = IterationCallbackType::New();
  callback->SetOptimizer( optimizer );

  registration->SetOptimizer(optimizer);
  registration->SetMetric(metric);

  TransformType::ParametersType initialParameters;
  initialParameters.resize(3);

  // Apply an initial transformation
  initialParameters[0] = 0.2; // angle
  initialParameters[1] = 7; // offset 
  initialParameters[2] = 6; // offset 

  std::cout << "Initial Parameters  : " << initialParameters << std::endl;

  registration->SetInitialTransformParameters(initialParameters);
  optimizer->MaximizeOn();

  // Show the two sets of spheres misregistered
  // Create a group with 3 ellipses linked by lines.
  EllipseType::Pointer ellipse1copy = EllipseType::New();
  EllipseType::Pointer ellipse2copy = EllipseType::New();
  EllipseType::Pointer ellipse3copy = EllipseType::New();

  // Set the radius
  ellipse1copy->SetRadius(10);
  ellipse2copy->SetRadius(10);
  ellipse3copy->SetRadius(10);

  GroupType::Pointer transformedGroup = GroupType::New();
  offset[0]=100;
  offset[1]=40;
  ellipse1copy->GetTransform()->SetOffset(offset);
  ellipse1copy->ComputeGlobalTransform();
 
  offset[0]=40;
  offset[1]=150;
  ellipse2copy->GetTransform()->SetOffset(offset);
  ellipse2copy->ComputeGlobalTransform();

  offset[0]=150;
  offset[1]=150;
  ellipse3copy->GetTransform()->SetOffset(offset);
  ellipse3copy->ComputeGlobalTransform();

  transformedGroup->AddSpatialObject(ellipse1copy);
  transformedGroup->AddSpatialObject(ellipse2copy);
  transformedGroup->AddSpatialObject(ellipse3copy);

  // Apply the initial transformation to the initial group of ellipses
  transform->SetParameters(initialParameters);
  transformedGroup->GetTransform()->SetMatrix(transform->GetRotationMatrix());
  transformedGroup->GetTransform()->SetOffset(transform->GetOffset());
  transformedGroup->ComputeGlobalTransform();

  GroupType::Pointer firstGroup = GroupType::New();
  firstGroup->AddSpatialObject(group);
  firstGroup->AddSpatialObject(transformedGroup);

  // Create an image to show it to the user
  imageFilter->SetInput(firstGroup);
  imageFilter->SetSize(size);
  imageFilter->Update();

  // Rescale the image
  typedef itk::RescaleIntensityImageFilter< 
                            ImageType, 
                            OutputImageType >    RescaleFilterType;

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();

  rescale->SetInput( imageFilter->GetOutput() );
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  // Write the image showing a set of three ellipses mis-registered and a set
  // of 3 ellipses that is used as the fixed image.
  WriterType::Pointer      writer =  WriterType::New();
  writer->SetFileName( "ModelToImageRegistration-Input.png" );
                
  writer->SetInput( rescale->GetOutput()   );
  try 
  { 
    writer->Update();
  } 
  catch( itk::ExceptionObject & err ) 
  { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
  } 


  // Start the registration
  registration->StartRegistration();

  RegistrationType::ParametersType finalParameters 
                                 = registration->GetLastTransformParameters();

  std::cout << "Final Solution is : " << finalParameters << std::endl;


  /** Show the registration */
  transformedGroup->GetTransform()->SetMatrix(transform->GetRotationMatrix());
  transformedGroup->GetTransform()->SetOffset(transform->GetOffset());

  transformedGroup->ComputeGlobalTransform();

  typedef itk::SpatialObjectToImageFilter<GroupType,ImageType> SpatialObjectToImageFilterType;
  SpatialObjectToImageFilterType::Pointer imageFilter2 = SpatialObjectToImageFilterType::New();

  imageFilter2->SetInput(group);
  imageFilter2->SetSize(size);
  imageFilter2->Update();

  writer->SetFileName( "ModelToImageRegistration-Output.png" );
                  
  rescale->SetInput( imageFilter2->GetOutput() );
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  writer->SetInput( rescale->GetOutput()   );
  try 
  { 
    writer->Update();
  } 
  catch( itk::ExceptionObject & err ) 
  { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return -1;
  } 
  return EXIT_SUCCESS;

}
