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

#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationCurvatureTerm.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkNumericTraits.h"

int itkLevelSetEquationCurvatureTermTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Program " << argv[0] << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned short                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >           InputImageType;
  typedef itk::IdentifierType                               IdentifierType;

  typedef unsigned short                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >           InputImageType;

  typedef float                                             PixelType;
  typedef itk::WhitakerSparseLevelSetImage< PixelType, Dimension >
                                                            SparseLevelSetType;
  typedef itk::BinaryImageToLevelSetImageAdaptor< InputImageType, SparseLevelSetType >
                                                            BinaryToSparseAdaptorType;

  typedef itk::LevelSetContainer< IdentifierType, SparseLevelSetType >  LevelSetContainerType;
  typedef itk::LevelSetEquationCurvatureTerm< InputImageType, LevelSetContainerType >
                                                                        CurvatureTermType;

  typedef std::list< IdentifierType >                       IdListType;
  typedef itk::Image< IdListType, Dimension >               IdListImageType;
  typedef itk::Image< short, Dimension >                    CacheImageType;
  typedef itk::LevelSetDomainMapImageFilter< IdListImageType, CacheImageType >
                                                            DomainMapImageFilterType;

  typedef SparseLevelSetType::OutputRealType                  LevelSetOutputRealType;
  typedef itk::SinRegularizedHeavisideStepFunction< LevelSetOutputRealType, LevelSetOutputRealType >
                                                            HeavisideFunctionBaseType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType > InputImageIteratorType;

  // load binary mask
  InputImageType::SizeType size;
  size.Fill( 50 );

  InputImageType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;

  InputImageType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;

  InputImageType::IndexType index;
  index.Fill( 0 );

  InputImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  // Binary initialization
  InputImageType::Pointer binary = InputImageType::New();
  binary->SetRegions( region );
  binary->SetSpacing( spacing );
  binary->SetOrigin( origin );
  binary->Allocate();
  binary->FillBuffer( itk::NumericTraits<InputPixelType>::Zero );

  index.Fill( 10 );
  size.Fill( 30 );

  region.SetIndex( index );
  region.SetSize( size );

  InputImageIteratorType iIt( binary, region );
  iIt.GoToBegin();
  while( !iIt.IsAtEnd() )
    {
    iIt.Set( itk::NumericTraits<InputPixelType>::One );
    ++iIt;
    }

  // Convert binary mask to sparse level set
  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage( binary );
  adaptor->Initialize();
  std::cout << "Finished converting to sparse format" << std::endl;

  SparseLevelSetType::Pointer level_set = adaptor->GetModifiableLevelSet();

  IdListType list_ids;
  list_ids.push_back( 1 );

  IdListImageType::Pointer id_image = IdListImageType::New();
  id_image->SetRegions( binary->GetLargestPossibleRegion() );
  id_image->Allocate();
  id_image->FillBuffer( list_ids );

  DomainMapImageFilterType::Pointer domainMapFilter = DomainMapImageFilterType::New();
  domainMapFilter->SetInput( id_image );
  domainMapFilter->Update();
  std::cout << "Domain map computed" << std::endl;

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 1.0 );

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );
  lscontainer->SetDomainMapFilter( domainMapFilter );

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet( 0, level_set, false );
  if ( !LevelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }

  // Create ChanAndVese External term for phi_{1}
  CurvatureTermType::Pointer term = CurvatureTermType::New();
  term->SetInput( binary );
  term->SetCoefficient( 1.0 );
  term->SetCurrentLevelSetId( 0 );
  term->SetLevelSetContainer( lscontainer );
  std::cout << "Curvature term created" << std::endl;

  // Initialize the ChanAndVese term here
  term->InitializeParameters();
  InputImageIteratorType it( binary, binary->GetLargestPossibleRegion() );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    term->Initialize( it.GetIndex() );
    ++it;
    }

  term->Update();

  index[0] = 10;
  index[1] = 20;

  CurvatureTermType::LevelSetOutputRealType value = term->Evaluate( index );
  if( vnl_math_abs( value ) >  5e-2 )
    {
    std::cerr << "( vnl_math_abs( " << value << " ) >  5e-2 )" << std::endl;
    return EXIT_FAILURE;
    }

  term->SetCurvatureImage( binary );

  if( term->GetCurvatureImage() != binary )
    {
    std::cerr << "term->GetCurvatureImage != binary" << std::endl;
    return EXIT_FAILURE;
    }

  term->InitializeParameters();
  term->Update();

  if( term->Evaluate( index ) != value * binary->GetPixel( index ) )
    {
    std::cerr << "term->Evaluate( index ) != value * binary->GetPixel( index )" << std::endl;
    std::cerr << "term->Evaluate( index ) = " << term->Evaluate( index ) << std::endl;
    std::cerr << "value = " << value << std::endl;
    std::cerr << "binary->GetPixel( index ) = " << binary->GetPixel( index ) << std::endl;
    return EXIT_FAILURE;
    }

  term->SetUseCurvatureImage( false );

  if( term->Evaluate( index ) != value )
    {
    std::cerr << "term->Evaluate( index ) != value" << std::endl;
    std::cerr << "term->Evaluate( index ) = " << term->Evaluate( index ) << std::endl;
    std::cerr << "value = " << value << std::endl;

    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
