/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetImageFilterTest_2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkShapeDetectionLevelSetImageFilter.h"

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkCastImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"

/* Uncomment to write out image files */
/*
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"
*/

namespace itk {
namespace SDLSIF {

template <class TImageType, class TFeatureImageType = TImageType>
class ITK_EXPORT TestFunction
  : public ShapeDetectionLevelSetFunction<TImageType,TFeatureImageType>
{
public:
  typedef TestFunction Self;
  typedef ShapeDetectionLevelSetFunction<TImageType,TFeatureImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro( Self );
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::VectorType VectorType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  virtual PixelType ComputeUpdate(const NeighborhoodType &it,
                                  void *gd,
                                  const FloatOffsetType& offset = FloatOffsetType(0.0)) const
  {

      // Find the center index of the neighborhood.
      ::size_t m_Center =  it.Size() / 2;

      // Get the stride length for each axis.
      ::size_t m_xStride[ImageDimension];
      for(unsigned int i = 0; i < ImageDimension; i++)
        {  m_xStride[i] = it.GetStride(i); }

      // print out info on the neighborhood
      std::cout << "------" << std::endl;
      std::cout << "Index: " << it.GetIndex() << std::endl;
      std::cout << "Values: ";
      for( unsigned int j = 0; j < it.Size(); j++ )
        {
        if ( !(j % 3) )
          { std::cout << std::endl << "\t"; }
          std::cout << "\t" << it.GetPixel(j);
        }
      std::cout << std::endl;
      /*
        ToDo:
        1. Implement vanishing viscosity for curvature term (replace epsilon)?
        2. Add in max_curvature term to calculation of dt.
      */
      unsigned int i;  
      const ScalarValueType ZERO = NumericTraits<ScalarValueType>::Zero;
      const ScalarValueType MIN_NORM = 1.0e-6;
      
      ScalarValueType curve, gradMag;
      ScalarValueType temp_value;
      ScalarValueType dx[ImageDimension];
      ScalarValueType dx_forward[ImageDimension], dx_backward[ImageDimension];
      ScalarValueType propagation_gradient;
      ScalarValueType propagation_term, curvature_term, advection_term;
      VectorType advection_field;
      ScalarValueType x_energy[ImageDimension];

      // Global data structure
      GlobalDataStruct *globalData = (GlobalDataStruct *)gd;

      temp_value  = it.GetCenterPixel();  

      // Calculate the mean curvature
      ScalarValueType dxx[ImageDimension];
      ScalarValueType dxy[ImageDimension * (ImageDimension-1)/2];

      for( i = 0 ; i < ImageDimension; i++)
        {
        const unsigned int positionA = 
          static_cast<unsigned int>( m_Center + m_xStride[i]);    
        const unsigned int positionB = 
          static_cast<unsigned int>( m_Center - m_xStride[i]);    
        dx[i] = 0.5 * (it.GetPixel( positionA ) - 
                      it.GetPixel( positionB )    );
          
        dx_forward[i]  = it.GetPixel( positionA ) - temp_value;
        dx_backward[i] = temp_value - it.GetPixel( positionB );
        }
      

      std::cout << "dx: ";
      for( unsigned int s = 0; s < ImageDimension; s++ )
        {
        std::cout << dx[s] << " ";
        }
      std::cout << std::endl;

      std::cout << "dx_forward: ";
      for( unsigned int s = 0; s < ImageDimension; s++ )
        {
        std::cout << dx_forward[s] << " ";
        }
      std::cout << std::endl;

      std::cout << "dx_backward: ";
      for( unsigned int s = 0; s < ImageDimension; s++ )
        {
        std::cout << dx_backward[s] << " ";
        }
      std::cout << std::endl;


      int k = 0;

      curve = ZERO;
      
      for (i = 0; i < ImageDimension; i++)
        {
        const unsigned int positionAI = 
          static_cast<unsigned int>( m_Center + m_xStride[i]);    
        const unsigned int positionBI = 
          static_cast<unsigned int>( m_Center - m_xStride[i]);    
          dxx[i] = it.GetPixel( positionAI )
                + it.GetPixel( positionBI ) - 2.0 * temp_value;
          
          for(unsigned int j = i+1; j < ImageDimension; j++)
            {
            const unsigned int positionA = static_cast<unsigned int>( 
                                    m_Center - m_xStride[i] - m_xStride[j] );    
            const unsigned int positionB = static_cast<unsigned int>( 
                                    m_Center - m_xStride[i] + m_xStride[j] );    
            const unsigned int positionC = static_cast<unsigned int>( 
                                    m_Center + m_xStride[i] - m_xStride[j] );    
            const unsigned int positionD = static_cast<unsigned int>( 
                                    m_Center + m_xStride[i] + m_xStride[j] );    
            dxy[k] = 0.25 *( it.GetPixel( positionA )
                          - it.GetPixel( positionB )
                          - it.GetPixel( positionC )
                          + it.GetPixel( positionD )  );
             
            curve -= 2.0 * dx[i] * dx[j] * dxy[k]; 
            k++;
            }
        }

      std::cout << "dxx: ";
      for( unsigned int s = 0; s < ImageDimension; s++ )
        {
        std::cout << dxx[s] << " ";
        }
      std::cout << std::endl;

      std::cout << "dxy: ";
      for( unsigned int s = 0; s < (ImageDimension * (ImageDimension-1)/2); s++ )
        {
        std::cout << dxy[s] << " ";
        }
      std::cout << std::endl;

      std::cout << "curve: " << curve << std::endl;

      gradMag = MIN_NORM;
      for (i = 0; i < ImageDimension; i++)
        {
          
          for(unsigned int j = 0; j < ImageDimension; j++)
            {
              
              if(j != i)
                curve += dxx[j] * dx[i] * dx[i];
            }
          gradMag += dx[i] * dx[i];
        }

      std::cout << "gradMag: " << gradMag << std::endl;

      curve /= gradMag * vcl_sqrt(gradMag);
      
      curvature_term = curve;
      curvature_term *= m_CurvatureWeight * this->CurvatureSpeed(it, offset);

      std::cout << "curve: " << curve << std::endl;
      std::cout << "curvature_term: " << curvature_term << std::endl;

      // Calculate the advection term.
      //  $\alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi $
      //
      // Here we can use a simple upwinding scheme since we know the
      // sign of each directional component of the advective force.
      //
      if (m_AdvectionWeight != ZERO)
        {

          advection_field = this->AdvectionField(it, offset);
          advection_term = ZERO;

          for(i = 0; i < ImageDimension; i++)
            {

              x_energy[i] = m_AdvectionWeight * advection_field[i];
              
              if (x_energy[i] > ZERO) advection_term += advection_field[i] * dx_backward[i];
              else                 advection_term += advection_field[i] * dx_forward[i];

            }

          advection_term *= m_AdvectionWeight;

              
          // Collect energy change from the advection term.  This will be used
          // in calculating the maximum time step that can be taken this iteration.

          PixelType totalEnergy = ZERO;
          
          for(unsigned int i = 0; i < ImageDimension; i++)
            totalEnergy += vnl_math_abs(x_energy[i]);
          
          globalData->m_MaxAdvectionChange
            = vnl_math_max(globalData->m_MaxAdvectionChange, totalEnergy); 
        }
      else advection_term = ZERO;

      if (m_PropagationWeight != ZERO)
        {
          // Get the propagation speed
          propagation_term = m_PropagationWeight * this->PropagationSpeed(it, offset);

          std::cout << "propagation_term: " << propagation_term << std::endl;
          
          //
          // Construct upwind gradient values for use in the propagation speed term:
          //  $\beta G(\mathbf{x})\mid\nabla\phi\mid$
          //
          // The following scheme for ``upwinding'' in the normal direction is taken
          // from Sethian, Ch. 6 as referenced above.
          //
          propagation_gradient = ZERO;

          if ( propagation_term > ZERO )
            {
              for(i = 0; i< ImageDimension; i++)
                propagation_gradient += vnl_math_sqr( vnl_math_max(dx_backward[i], ZERO) )
                  + vnl_math_sqr( vnl_math_min(dx_forward[i],  ZERO) );

            }
          else
            {
              for(i = 0; i< ImageDimension; i++)
                propagation_gradient += vnl_math_sqr( vnl_math_min(dx_backward[i], ZERO) )
                  + vnl_math_sqr( vnl_math_max(dx_forward[i],  ZERO) );

            }
          
          std::cout << "propagation_gradient: " << propagation_gradient << std::endl;

          // Collect energy change from propagation term.  This will be used in
          // calculating the maximum time step that can be taken for this iteration.
          globalData->m_MaxPropagationChange =
            vnl_math_max(globalData->m_MaxPropagationChange,
                                            vnl_math_abs(propagation_term));
          
          propagation_term *= vcl_sqrt( propagation_gradient );

          std::cout << "propagation_term: " << propagation_term << std::endl;
        }
      else propagation_term = ZERO;

      std::cout << "-------" << std::endl;
      // Return the combination of all the terms.
      return ( PixelType ) ( curvature_term - propagation_term - advection_term );

  }


protected:
  TestFunction() {};
  ~TestFunction() {};
  void PrintSelf( std::ostream& os, Indent indent ) const
    { Superclass::PrintSelf( os, indent ); }

};

template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float >
class ITK_EXPORT TestFilter
  : public ShapeDetectionLevelSetImageFilter<TInputImage,TFeatureImage,TOutputPixelType>
{
public:
  typedef TestFilter Self;
  typedef ShapeDetectionLevelSetImageFilter<TInputImage,TFeatureImage,TOutputPixelType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef TestFunction<OutputImageType,TFeatureImage> TestFunctionType;
  typedef typename TestFunctionType::Pointer TestFunctionPointer;
protected:
  ~TestFilter(){};
  TestFilter()
    {
    m_TestFunction = TestFunctionType::New();
    this->SetSegmentationFunction( m_TestFunction );
    this->SetUseNegativeFeatures( true );
    this->InterpolateSurfaceLocationOff();
    };
  
  virtual void PrintSelf( std::ostream &os, Indent indent ) const
    { Superclass::PrintSelf( os, indent ); }

private:
  TestFunctionPointer m_TestFunction;

};

} // namespace SDLSIF
} // namespace itk

int itkShapeDetectionLevelSetImageFilterTest_2(int, char* [] )
{

  const   unsigned int    ImageDimension = 2;
  typedef unsigned char   PixelType;
  typedef float           InternalPixelType;

  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<InternalPixelType,ImageDimension> InternalImageType;

  ImageType::SizeType imageSize;
  imageSize[0] = 128;
  imageSize[1] = 128;

  ImageType::RegionType imageRegion;
  imageRegion.SetSize( imageSize );

  /**
   * Create an input image.
   * A light square on a dark background.
   */
  PixelType background = 0;
  PixelType foreground = 190;

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions( imageRegion );
  inputImage->Allocate();
  inputImage->FillBuffer( background );

  ImageType::IndexType squareStart;
  squareStart.Fill( 20 );
  ImageType::SizeType squareSize;
  squareSize.Fill( 60 );
  ImageType::RegionType squareRegion;
  squareRegion.SetIndex( squareStart );
  squareRegion.SetSize( squareSize );

  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator it( inputImage, squareRegion );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( foreground );
    ++it;
    }

  /**
   * Create an edge potential map.
   * First compute the image gradient magnitude using a derivative of gaussian filter.
   * Then apply a sigmoid function to the gradient magnitude.
   */
  typedef itk::CastImageFilter< ImageType, InternalImageType > CastFilterType;
  CastFilterType::Pointer caster = CastFilterType::New();
  caster->SetInput( inputImage );

  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter< InternalImageType,
    InternalImageType > GradientImageType;

  GradientImageType::Pointer gradMagnitude = GradientImageType::New();
  gradMagnitude->SetInput( caster->GetOutput() );
  gradMagnitude->SetSigma( 1.0 );

  typedef itk::SigmoidImageFilter< InternalImageType, InternalImageType >
    SigmoidFilterType;
  SigmoidFilterType::Pointer sigmoid = SigmoidFilterType::New();
  sigmoid->SetOutputMinimum( 0.0 );
  sigmoid->SetOutputMaximum( 1.0 );
  sigmoid->SetAlpha( -0.4 );
  sigmoid->SetBeta( 2.5 );
  sigmoid->SetInput( gradMagnitude->GetOutput() );

  /**
   * Create an initial level.
   * Use fast marching to create an signed distance from a seed point.
   */
  typedef itk::FastMarchingImageFilter<InternalImageType> FastMarchingFilterType;
  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  typedef FastMarchingFilterType::NodeContainer NodeContainer;
  typedef FastMarchingFilterType::NodeType      NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();

  // Choose an initial contour that is wholly within the square to be segmented.
  InternalImageType::IndexType seedPosition;
  seedPosition[0] = 47;
  seedPosition[1] = 47;

  NodeType node;
  node.SetValue( -5.0 );
  node.SetIndex( seedPosition );

  seeds->Initialize();
  seeds->InsertElement( 0, node );

  fastMarching->SetTrialPoints( seeds );
  fastMarching->SetSpeedConstant( 1.0 );
  fastMarching->SetOutputSize( imageSize );

  /**
   * Set up and run the shape detection filter
   */
  typedef itk::SDLSIF::TestFilter<
    InternalImageType, InternalImageType > ShapeDetectionFilterType;
  
  ShapeDetectionFilterType::Pointer shapeDetection = ShapeDetectionFilterType::New();
  
  // set the initial level set
  shapeDetection->SetInput( fastMarching->GetOutput() );

  // set the edge potential image
  shapeDetection->SetFeatureImage( sigmoid->GetOutput() );

  // set the weights between the propagation and curvature terms
  shapeDetection->SetPropagationScaling( 1.0 );
  shapeDetection->SetCurvatureScaling( 0.1 );

  // set the convergence criteria
  shapeDetection->SetMaximumRMSError( 0.02 );
  shapeDetection->SetMaximumIterations( 1 );

  /**
   * Threshold the output level set to display the final contour.
   */
  typedef itk::BinaryThresholdImageFilter< InternalImageType, ImageType >
    ThresholdFilterType;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput( shapeDetection->GetOutput() );
  thresholder->SetLowerThreshold( -1e+10 );
  thresholder->SetUpperThreshold( 0.0 );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 255 );

  /**
   * Compute overlap between the true shape and the segmented shape
   */
  typedef itk::SimilarityIndexImageFilter< ImageType, ImageType >
    OverlapCalculatorType;
  OverlapCalculatorType::Pointer overlap = OverlapCalculatorType::New();

  overlap->SetInput1( inputImage );
  overlap->SetInput2( thresholder->GetOutput() );
  overlap->Update();
  
  /** Printout useful information from the shape detection filter. */
  std::cout << "Max. no. iterations: " << shapeDetection->GetMaximumIterations() << std::endl;
  std::cout << "Max. RMS error: " << shapeDetection->GetMaximumRMSError() << std::endl;
  std::cout << "No. elpased iterations: " << shapeDetection->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << shapeDetection->GetRMSChange() << std::endl;
  std::cout << "Overlap: " << overlap->GetSimilarityIndex() << std::endl;

/*
  if ( overlap->GetSimilarityIndex() > 0.90 )
    {
    std::cout << "Overlap exceed threshold." << std::endl;
    }
  else
    {
    std::cout << "Overlap below threshold." << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }
*/

  /**
   * Uncomment to write out image files.
   */
  /*
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  typedef itk::RescaleIntensityImageFilter< InternalImageType,
    ImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  writer->SetFileName( "inputImage.png" );
  writer->SetInput( inputImage );
  writer->Update();

  rescaler->SetInput( gradMagnitude->GetOutput() );
  rescaler->SetOutputMinimum( 0 );
  rescaler->SetOutputMaximum( 255 );
  writer->SetFileName( "gradMagnitude.png" );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  rescaler->SetInput( sigmoid->GetOutput() );
  writer->SetFileName( "edgePotential.png" );
  writer->Update();

  rescaler->SetInput( fastMarching->GetOutput() );
  writer->SetFileName( "initialLevelSet.png" );
  writer->Update();

  writer->SetInput( thresholder->GetOutput() );
  writer->SetFileName( "outputLevelSet.png" );
  writer->Update();
  */

  std::cout << "Test Passed. " << std::endl;
  return EXIT_SUCCESS;

}
