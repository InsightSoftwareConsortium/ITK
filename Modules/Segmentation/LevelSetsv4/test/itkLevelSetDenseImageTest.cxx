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


#include "itkLevelSetDenseImage.h"
#include "itkImageRegionIteratorWithIndex.h"

#include "itkLevelSetTestFunction.h"

/**
 * \class ToleranceChecker
 * \brief Compare values to see if they are within tolerance.
 */
template< typename RealType >
class ToleranceChecker
{
public:
  ToleranceChecker(): m_Tolerance( 1e-8 )
  {}

  bool IsOutsideTolerance( const RealType & value, const RealType & theoreticalValue ) const
    {
    // ignore if they are both effectively zero
    if( std::max( itk::Math::abs( value ), itk::Math::abs( theoreticalValue ) ) <  50 * itk::Math::eps )
      {
      return false;
      }
    if( this->GetFractionalError( value, theoreticalValue ) > m_Tolerance )
      {
      return true;
      }
    return false;
    }

  RealType GetFractionalError( const RealType & value, const RealType & theoreticalValue ) const
    {
    RealType fractionalError = itk::Math::abs( theoreticalValue - value ) /
      ( itk::Math::abs( theoreticalValue ) + 20* itk::Math::eps );
    return fractionalError;
    }

  /** Set fractional tolerance. */
  void SetTolerance( const RealType & tolerance )
    {
    m_Tolerance = tolerance;
    }

private:
  RealType m_Tolerance;

};

int itkLevelSetDenseImageTest( int , char* [] )
{
  const unsigned int Dimension = 2;

  typedef float PixelType;

  typedef itk::Image< PixelType, Dimension >   ImageType;
  typedef itk::LevelSetDenseImage< ImageType > LevelSetType;

  ImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  ImageType::SizeType size;
  size[0] = 10;
  size[1] = 20;

  ImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  PixelType zeroValue = 0.;

  ImageType::SpacingType spacing;
  spacing[0] = 0.02 / size[0];
  spacing[1] = 0.02 / size[1];

  ImageType::PointType origin;
  origin[0] = 3.99;
  origin[1] = 3.99;

  ImageType::Pointer input = ImageType::New();
  input->SetRegions( region );
  input->SetSpacing( spacing );
  input->SetOrigin(  origin );
  input->Allocate();
  input->FillBuffer( zeroValue );

  itk::ImageRegionIteratorWithIndex< ImageType > it( input,
                                              input->GetLargestPossibleRegion() );

  it.GoToBegin();

  ImageType::IndexType idx;
  ImageType::PointType pt;

  typedef itk::LevelSetTestFunction< PixelType > TestFunctionType;
  TestFunctionType::Pointer testFunction = TestFunctionType::New();

  while( !it.IsAtEnd() )
    {
    idx = it.GetIndex();
    input->TransformIndexToPhysicalPoint( idx, pt );

    PixelType tempValue = testFunction->Evaluate( pt );
    it.Set( tempValue );


    ++it;
    }

  LevelSetType::Pointer level_set = LevelSetType::New();
  level_set->SetImage( input );

  idx[0] = 9;
  idx[1] = 18;
  input->TransformIndexToPhysicalPoint( idx, pt );
  LevelSetType::OutputType theoreticalValue = testFunction->Evaluate( pt );
  LevelSetType::OutputType value = level_set->Evaluate( idx );

  ToleranceChecker< double > toleranceChecker;

  toleranceChecker.SetTolerance( 1e-8 );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    idx = it.GetIndex();
    input->TransformIndexToPhysicalPoint( idx, pt );

    theoreticalValue = testFunction->Evaluate( pt );
    value            = level_set->Evaluate( idx );
    if( toleranceChecker.IsOutsideTolerance( value, theoreticalValue ) )
      {
      std::cout << "Index:" << idx << " *EvaluateTestFail* " << value << " != "
                << theoreticalValue << std::endl;
      return EXIT_FAILURE;
      }

    if( level_set->IsInside( idx ) != ( theoreticalValue <= 0. ) )
      {
      std::cerr << "if( testFunction->IsInside( pt ) != ( theoreticalValue <= 0. ) )" << std::endl;
      std::cerr << "pt : " << pt << std::endl;
      std::cerr << "theoreticalValue: " << theoreticalValue << std::endl;
      return EXIT_FAILURE;
      }

    ++it;
    }

  LevelSetType::GradientType gradient;
  LevelSetType::GradientType theoreticalGradient;

  toleranceChecker.SetTolerance( 0.1 );
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    idx = it.GetIndex();
    input->TransformIndexToPhysicalPoint( idx, pt );

    theoreticalGradient = testFunction->EvaluateGradient( pt );
    gradient            = level_set->EvaluateGradient( idx );
    if( toleranceChecker.IsOutsideTolerance( gradient[0], theoreticalGradient[0] ) ||
        toleranceChecker.IsOutsideTolerance( gradient[1], theoreticalGradient[1] ) )
      {
      std::cout << "Index:" << idx << " Point: " << pt
        << " Error: [" << toleranceChecker.GetFractionalError( gradient[0], theoreticalGradient[0] )
        << ',' << toleranceChecker.GetFractionalError( gradient[1], theoreticalGradient[1] ) << "] "
        <<" *EvaluateGradientTestFail* " << gradient << " != " << theoreticalGradient << std::endl;
      return EXIT_FAILURE;
      }

    ++it;
    }

  /** \todo more thorough testing as with the gradient above for hessian,
   * laplacian, gradient norm. */
  idx[0] = 9;
  idx[1] = 18;
  input->TransformIndexToPhysicalPoint( idx, pt );
  LevelSetType::HessianType hessian = level_set->EvaluateHessian( idx );
  std::cout << "hessian = " << std::endl << hessian << std::endl;

  if ( itk::Math::abs( itk::Math::abs( hessian[0][0] ) - 499.998 ) / 499.998 > 5e-2 )
    {
    std::cout << idx << " *HessianTestFail* " << itk::Math::abs( hessian[0][0] ) << " != "
              << itk::Math::abs( hessian[1][1] ) << std::endl;
    return EXIT_FAILURE;
    }

  LevelSetType::OutputRealType laplacian = level_set->EvaluateLaplacian( idx );
  std::cout << "laplacian = " << laplacian << std::endl;

  LevelSetType::OutputRealType gradientnorm = level_set->EvaluateGradientNorm( idx );
  std::cout <<"gradient norm = " << gradientnorm << std::endl;

  if( itk::Math::abs( 1 - gradientnorm ) > 5e-2 )
    {
    std::cout << idx << " *GradientNormFail* " << gradientnorm << " != "
              << 1 << std::endl;
    return EXIT_FAILURE;
    }

  LevelSetType::OutputRealType meancurvature = level_set->EvaluateMeanCurvature( idx );
  std::cout <<"mean curvature = " << meancurvature << std::endl;

  return EXIT_SUCCESS;
}
