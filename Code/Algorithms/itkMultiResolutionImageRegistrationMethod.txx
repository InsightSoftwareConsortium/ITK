/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionImageRegistrationMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMultiResolutionImageRegistrationMethod_txx
#define _itkMultiResolutionImageRegistrationMethod_txx

#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"

namespace itk
{

/*
 * Constructor
 */
template < typename TFixedImage, typename TMovingImage >
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::MultiResolutionImageRegistrationMethod()
{
  this->SetNumberOfRequiredOutputs( 1 );  // for the Transform

  m_FixedImage   = 0; // has to be provided by the user.
  m_MovingImage  = 0; // has to be provided by the user.
  m_Transform    = 0; // has to be provided by the user.
  m_Interpolator = 0; // has to be provided by the user.
  m_Metric       = 0; // has to be provided by the user.
  m_Optimizer    = 0; // has to be provided by the user.

  // Use MultiResolutionPyramidImageFilter as the default
  // image pyramids.
  m_FixedImagePyramid  = FixedImagePyramidType::New(); 
  m_MovingImagePyramid = MovingImagePyramidType::New();

  m_NumberOfLevels = 1;
  m_CurrentLevel = 0;

  m_Stop = false;

  m_InitialTransformParameters = ParametersType(1);
  m_InitialTransformParametersOfNextLevel = ParametersType(1);
  m_LastTransformParameters = ParametersType(1);

  m_InitialTransformParameters.Fill( 0.0f );
  m_InitialTransformParametersOfNextLevel.Fill( 0.0f );
  m_LastTransformParameters.Fill( 0.0f );


  TransformOutputPointer transformDecorator = 
                 static_cast< TransformOutputType * >( 
                                  this->MakeOutput(0).GetPointer() );

  this->ProcessObject::SetNthOutput( 0, transformDecorator.GetPointer() );
}


/*
 * Initialize by setting the interconnects between components. 
 */
template < typename TFixedImage, typename TMovingImage >
void
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::Initialize() throw (ExceptionObject)
{

  // Sanity checks
  if ( !m_Metric )
    {
    itkExceptionMacro(<<"Metric is not present" );
    }

  if ( !m_Optimizer )
    {
    itkExceptionMacro(<<"Optimizer is not present" );
    }

  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform is not present");
    }

  if( !m_Interpolator )
    {
    itkExceptionMacro(<<"Interpolator is not present");
    }

  // Setup the metric
  m_Metric->SetMovingImage( m_MovingImagePyramid->GetOutput(m_CurrentLevel) );
  m_Metric->SetFixedImage( m_FixedImagePyramid->GetOutput(m_CurrentLevel) );
  m_Metric->SetTransform( m_Transform );
  m_Metric->SetInterpolator( m_Interpolator );
  m_Metric->SetFixedImageRegion( m_FixedImageRegionPyramid[ m_CurrentLevel ] );
  m_Metric->Initialize();

  // Setup the optimizer
  m_Optimizer->SetCostFunction( m_Metric );
  m_Optimizer->SetInitialPosition( m_InitialTransformParametersOfNextLevel );

  //
  // Connect the transform to the Decorator.
  //
  TransformOutputType * transformOutput =  
     static_cast< TransformOutputType * >( this->ProcessObject::GetOutput(0) );

  transformOutput->Set( m_Transform.GetPointer() );

}


/*
 * Stop the Registration Process
 */
template < typename TFixedImage, typename TMovingImage >
void
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::StopRegistration( void )
{
  m_Stop = true;
}





/*
 * Stop the Registration Process
 */
template < typename TFixedImage, typename TMovingImage >
void
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::PreparePyramids( void )
{

  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform is not present");
    }

  m_InitialTransformParametersOfNextLevel = m_InitialTransformParameters;

  if ( m_InitialTransformParametersOfNextLevel.Size() != 
       m_Transform->GetNumberOfParameters() )
    {
    itkExceptionMacro(<<"Size mismatch between initial parameter and transform"); 
    }

  // Sanity checks
  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
    }

  if( !m_FixedImagePyramid )
    {
    itkExceptionMacro(<<"Fixed image pyramid is not present");
    }

  if( !m_MovingImagePyramid )
    {
    itkExceptionMacro(<<"Moving image pyramid is not present");
    }

  // Setup the fixed image pyramid
  m_FixedImagePyramid->SetNumberOfLevels( m_NumberOfLevels );
  m_FixedImagePyramid->SetInput( m_FixedImage );
  m_FixedImagePyramid->UpdateLargestPossibleRegion();

  // Setup the moving image pyramid
  m_MovingImagePyramid->SetNumberOfLevels( m_NumberOfLevels );
  m_MovingImagePyramid->SetInput( m_MovingImage );
  m_MovingImagePyramid->UpdateLargestPossibleRegion();

  typedef typename FixedImageRegionType::SizeType         SizeType;
  typedef typename FixedImageRegionType::IndexType        IndexType;
  typedef typename FixedImagePyramidType::ScheduleType    ScheduleType;

  ScheduleType schedule = m_FixedImagePyramid->GetSchedule();

  SizeType  inputSize  = m_FixedImageRegion.GetSize();
  IndexType inputStart = m_FixedImageRegion.GetIndex();

  m_FixedImageRegionPyramid.reserve( m_NumberOfLevels );
  m_FixedImageRegionPyramid.resize( m_NumberOfLevels );

  // Compute the FixedImageRegion corresponding to each level of the 
  // pyramid. This uses the same algorithm of the ShrinkImageFilter 
  // since the regions should be compatible. 
  for ( unsigned int level=0; level < m_NumberOfLevels; level++ )
    {
    SizeType  size;
    IndexType start;
    for ( unsigned int dim = 0; dim < TFixedImage::ImageDimension; dim++)
      {
      const float scaleFactor = static_cast<float>( schedule[ level ][ dim ] );

      size[ dim ] = static_cast<typename SizeType::SizeValueType>(
        vcl_floor(static_cast<float>( inputSize[ dim ] ) / scaleFactor ) );
      if( size[ dim ] < 1 )
        {
        size[ dim ] = 1;
        }
      
      start[ dim ] = static_cast<typename IndexType::IndexValueType>(
        vcl_ceil(static_cast<float>( inputStart[ dim ] ) / scaleFactor ) ); 
      }
    m_FixedImageRegionPyramid[ level ].SetSize( size );
    m_FixedImageRegionPyramid[ level ].SetIndex( start );
    }

}





/*
 * Starts the Registration Process
 */
template < typename TFixedImage, typename TMovingImage >
void
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::StartRegistration( void )
{ 

  // StartRegistration is an old API from before
  // this egistrationMethod was a subclass of ProcessObject.
  // Historically, one could call StartRegistration() instead of
  // calling Update().  However, when called directly by the user, the
  // inputs to the RegistrationMethod may not be up to date.  This
  // may cause an unexpected behavior.
  //
  // Since we cannot eliminate StartRegistration for backward
  // compability reasons, we check whether StartRegistration was
  // called directly or whether Update() (which in turn called 
  // StartRegistration()).
  if (!m_Updating)
    {
    this->Update();
    }
  else
    {
    m_Stop = false;
    
    this->PreparePyramids();
    
    for ( m_CurrentLevel = 0; m_CurrentLevel < m_NumberOfLevels;
          m_CurrentLevel++ )
      {
      
      // Invoke an iteration event.
      // This allows a UI to reset any of the components between
      // resolution level.
      this->InvokeEvent( IterationEvent() );
      
      // Check if there has been a stop request
      if ( m_Stop ) 
        {
        break;
        }
      
      try
        {
        // initialize the interconnects between components
        this->Initialize();
        }
      catch( ExceptionObject& err )
        {
        m_LastTransformParameters = ParametersType(1);
        m_LastTransformParameters.Fill( 0.0f );
        
        // pass exception to caller
        throw err;
        }
      
      try
        {
        // do the optimization
        m_Optimizer->StartOptimization();
        }
      catch( ExceptionObject& err )
        {
        // An error has occurred in the optimization.
        // Update the parameters
        m_LastTransformParameters = m_Optimizer->GetCurrentPosition();
        
        // Pass exception to caller
        throw err;
        }
      
      // get the results
      m_LastTransformParameters = m_Optimizer->GetCurrentPosition();
      m_Transform->SetParameters( m_LastTransformParameters );
      
      // setup the initial parameters for next level
      if ( m_CurrentLevel < m_NumberOfLevels - 1 )
        {
        m_InitialTransformParametersOfNextLevel =
          m_LastTransformParameters;
        }
      }
    }

}


/*
 * PrintSelf
 */
template < typename TFixedImage, typename TMovingImage >
void
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Metric: " << m_Metric.GetPointer() << std::endl;
  os << indent << "Optimizer: " << m_Optimizer.GetPointer() << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "FixedImage: " << m_FixedImage.GetPointer() << std::endl;
  os << indent << "MovingImage: " << m_MovingImage.GetPointer() << std::endl;
  os << indent << "FixedImagePyramid: ";
  os << m_FixedImagePyramid.GetPointer() << std::endl;
  os << indent << "MovingImagePyramid: ";
  os << m_MovingImagePyramid.GetPointer() << std::endl;

  os << indent << "NumberOfLevels: ";
  os << m_NumberOfLevels << std::endl;

  os << indent << "CurrentLevel: ";
  os << m_CurrentLevel << std::endl;  

  os << indent << "InitialTransformParameters: ";
  os << m_InitialTransformParameters << std::endl;
  os << indent << "InitialTransformParametersOfNextLevel: ";
  os << m_InitialTransformParametersOfNextLevel << std::endl;
  os << indent << "LastTransformParameters: ";
  os << m_LastTransformParameters << std::endl;
  os << indent << "FixedImageRegion: ";
  os << m_FixedImageRegion << std::endl;
  for(unsigned int level=0; level< m_FixedImageRegionPyramid.size(); level++)
    {
    os << indent << "FixedImageRegion at level " << level << ": ";
    os << m_FixedImageRegionPyramid[level] << std::endl;
    }

}


/*
 * Generate Data
 */
template < typename TFixedImage, typename TMovingImage >
void
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::GenerateData()
{
  this->StartRegistration();
}



template < typename TFixedImage, typename TMovingImage >
unsigned long
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::GetMTime() const
{
  unsigned long mtime = Superclass::GetMTime();
  unsigned long m;


  // Some of the following should be removed once ivars are put in the
  // input and output lists
  
  if (m_Transform)
    {
    m = m_Transform->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_Interpolator)
    {
    m = m_Interpolator->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_Metric)
    {
    m = m_Metric->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_Optimizer)
    {
    m = m_Optimizer->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_FixedImage)
    {
    m = m_FixedImage->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  if (m_MovingImage)
    {
    m = m_MovingImage->GetMTime();
    mtime = (m > mtime ? m : mtime);
    }

  return mtime;
  
}

/*
 *  Get Output
 */
template < typename TFixedImage, typename TMovingImage >
const typename MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>::TransformOutputType *
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::GetOutput() const
{
  return static_cast< const TransformOutputType * >( this->ProcessObject::GetOutput(0) );
}



template < typename TFixedImage, typename TMovingImage >
DataObject::Pointer
MultiResolutionImageRegistrationMethod<TFixedImage,TMovingImage>
::MakeOutput(unsigned int output)
{
  switch (output)
    {
    case 0:
      return static_cast<DataObject*>(TransformOutputType::New().GetPointer());
      break;
    default:
      itkExceptionMacro("MakeOutput request for an output number larger than the expected number of outputs");
      return 0;
    }
}

} // end namespace itk


#endif
