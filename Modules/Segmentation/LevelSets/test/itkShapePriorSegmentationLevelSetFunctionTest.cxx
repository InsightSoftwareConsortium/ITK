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

#include "itkShapePriorSegmentationLevelSetFunction.h"
#include "itkSphereSignedDistanceFunction.h"
#include "itkDenseFiniteDifferenceImageFilter.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"

/**
 * This module tests the base class ShapePriorSegmentationLevelSetFunction.
 *
 * In particular this test plugs a ShapePriorSegmentationLevelSetFunction object
 * into a simple test filter, derived from DenseFiniteDifferenceImageFilter.
 *
 * Note that this test only tests the shape prior term of the level set
 * evolution. The other terms are excerised in other tests.
 *
 * In this test an initial level set is generated that is perturbed from
 * the shape model used in the level set function. The level set is evolved
 * for a fixed number of iterations. The output level set should be closed to
 * model.
 *
 * The output segmentation is compared to the model using SimilarityIndexImageFilter.
 * The test fails if the overlap is below a certain threshold.
 *
 */
namespace itk {
namespace SPSLSF {

template<typename TImage>
class SimpleTestFilter : public DenseFiniteDifferenceImageFilter< TImage, TImage >
{
public:
  typedef SimpleTestFilter         Self;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkTypeMacro( SimpleTestFilter, DenseFiniteDifferenceImageFilter );
  itkNewMacro( Self );
  virtual void SetNumberOfIterations( const IdentifierType numberOfIterations ) ITK_OVERRIDE
    {
    if ( this->m_NumberOfIterations != numberOfIterations )
      {
      this->m_NumberOfIterations = numberOfIterations;
      this->Modified();
      }
    }

  typedef ShapePriorSegmentationLevelSetFunction<TImage,TImage> ShapePriorFunctionType;
  ShapePriorFunctionType * GetShapePriorFunction()
    { return m_ShapePriorFunction; }

protected:
  SimpleTestFilter()
    {
    typename ShapePriorFunctionType::Pointer function = ShapePriorFunctionType::New();
    function->SetPropagationWeight( 0.0 );
    function->SetAdvectionWeight( 0.0 );
    function->SetCurvatureWeight( 0.0 );
    function->SetShapePriorWeight( 1.0 );

    typename ShapePriorFunctionType::RadiusType radius;
    radius.Fill( 1 );
    function->Initialize( radius );

    this->SetDifferenceFunction( function );

    m_NumberOfIterations = 0;
    m_ShapePriorFunction = function;
    }

private:
  unsigned int                             m_NumberOfIterations;
  typename ShapePriorFunctionType::Pointer m_ShapePriorFunction;

  virtual bool Halt() ITK_OVERRIDE
    {
    if ( this->GetElapsedIterations() == m_NumberOfIterations ) return true;
    else return false;
    }

};

} // namespace SPSLSF
} // namespace itk

int itkShapePriorSegmentationLevelSetFunctionTest( int, char *[])
{

  typedef float PixelType;
  const unsigned int Dimension = 2;
  typedef itk::Image<PixelType,Dimension> ImageType;

  // create an input level set using the sphere signed distance function
  ImageType::SizeType size;
  size.Fill( 128 );
  ImageType::RegionType region;
  region.SetSize( size );

  ImageType::Pointer input = ImageType::New();
  input->SetRegions( region );
  input->Allocate();

  typedef itk::SphereSignedDistanceFunction<double,Dimension> ShapeFunctionType;
  ShapeFunctionType::Pointer shape = ShapeFunctionType::New();
  shape->Initialize();

  ShapeFunctionType::ParametersType parameters( shape->GetNumberOfParameters() );
  parameters[0] = 10.0;
  parameters[1] = 50.0;
  parameters[2] = 50.0;
  shape->SetParameters( parameters );

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator iter( input, region );
  iter.GoToBegin();

  while ( !iter.IsAtEnd() )
    {
    ImageType::IndexType index;
    ShapeFunctionType::PointType point;
    index = iter.GetIndex();
    input->TransformIndexToPhysicalPoint( index, point );
    iter.Set( shape->Evaluate( point ) );
    ++iter;
    }

  /**
   * Set up the simple test filter using itk::ShapePriorSegmentationLevelSetFunction.
   */
  typedef itk::SPSLSF::SimpleTestFilter<ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  try
    {
    filter->SetNumberOfIterations( 60 );
    filter->SetInput( input );
    filter->GetShapePriorFunction()->SetFeatureImage( input ); //dummy feature image

    // perturb the parameters
    parameters[0] += 0.5;
    parameters[1] += 10.0;
    parameters[2] += 10.0;

    shape->SetParameters( parameters );
    filter->GetShapePriorFunction()->SetShapeFunction( shape );

    filter->Update();
    }
  catch( itk::ExceptionObject & err )
    {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  /**
   * Threshold output and verify results.
   */
  typedef itk::Image<unsigned char,Dimension> CharImageType;
  typedef itk::BinaryThresholdImageFilter< ImageType, CharImageType >
    ThresholdFilterType;
  ThresholdFilterType::Pointer thresholder = ThresholdFilterType::New();

  thresholder->SetInput( filter->GetOutput() );
  thresholder->SetLowerThreshold( -1e+10 );
  thresholder->SetUpperThreshold( 0.0 );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 255 );

  CharImageType::Pointer target = CharImageType::New();
  target->SetRegions( region );
  target->Allocate();

  typedef itk::ImageRegionIteratorWithIndex<CharImageType> CharIterator;
  CharIterator citer( target, region );
  citer.GoToBegin();

  while( !citer.IsAtEnd() )
    {
    CharImageType::IndexType index;
    ShapeFunctionType::PointType point;
    index = citer.GetIndex();
    input->TransformIndexToPhysicalPoint( index, point );
    if ( shape->Evaluate(point) < 0.0 )
      {
      citer.Set( 255 );
      }
    else
      {
      citer.Set( 0 );
      }

    ++citer;
    }


  /**
   * Compute overlap between the true shape and the segmented shape
   */
  typedef itk::SimilarityIndexImageFilter< CharImageType, CharImageType >
    OverlapCalculatorType;
  OverlapCalculatorType::Pointer overlap = OverlapCalculatorType::New();

  overlap->SetInput1( target );
  overlap->SetInput2( thresholder->GetOutput() );
  overlap->Update();

  if ( overlap->GetSimilarityIndex() > 0.90 )
    {
    std::cout << "Overlap of "
      << overlap->GetSimilarityIndex() << " exceed threshold." << std::endl;
    }
  else
    {
    std::cout << "Overlap of "
      << overlap->GetSimilarityIndex() << " is below threshold." << std::endl;
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise other methods for coverage
  filter->GetDifferenceFunction()->Print( std::cout );

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;

}
