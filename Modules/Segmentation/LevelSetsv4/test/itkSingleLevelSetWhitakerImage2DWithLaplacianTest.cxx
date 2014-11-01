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

#include "itkImageFileReader.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkLevelSetEvolution.h"
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkLevelSetEquationCurvatureTerm.h"
#include "itkLevelSetEquationPropagationTerm.h"
#include "itkLevelSetEquationLaplacianTerm.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"

int itkSingleLevelSetWhitakerImage2DWithLaplacianTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned short                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >           InputImageType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType >
                                                            InputIteratorType;
  typedef itk::ImageFileReader< InputImageType >            ReaderType;

  typedef float                                             PixelType;

  typedef itk::WhitakerSparseLevelSetImage< PixelType, Dimension >
                                                            SparseLevelSetType;
  typedef itk::BinaryImageToLevelSetImageAdaptor< InputImageType, SparseLevelSetType >
                                                            BinaryToSparseAdaptorType;

  typedef itk::IdentifierType                               IdentifierType;

  typedef itk::LevelSetContainer< IdentifierType, SparseLevelSetType >
                                                            LevelSetContainerType;

  typedef itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType >
                                                            ChanAndVeseInternalTermType;
  typedef itk::LevelSetEquationChanAndVeseExternalTerm< InputImageType, LevelSetContainerType >
                                                            ChanAndVeseExternalTermType;
  typedef itk::LevelSetEquationCurvatureTerm< InputImageType, LevelSetContainerType >
                                                            CurvatureTermType;
  typedef itk::LevelSetEquationPropagationTerm< InputImageType, LevelSetContainerType >
                                                            PropagationTermType;
  typedef itk::LevelSetEquationLaplacianTerm< InputImageType, LevelSetContainerType >
                                                            LaplacianTermType;
  typedef itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType >
                                                            TermContainerType;

  typedef itk::LevelSetEquationContainer< TermContainerType >
                                                            EquationContainerType;

  typedef itk::LevelSetEvolution< EquationContainerType, SparseLevelSetType >
                                                            LevelSetEvolutionType;

  typedef SparseLevelSetType::OutputRealType                      LevelSetOutputRealType;
  typedef itk::SinRegularizedHeavisideStepFunction< LevelSetOutputRealType, LevelSetOutputRealType >
                                                            HeavisideFunctionBaseType;
  typedef itk::ImageRegionIteratorWithIndex< InputImageType >     InputIteratorType;

  // load binary mask
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();
  InputImageType::Pointer input = reader->GetOutput();

  // Binary initialization
  InputImageType::Pointer binary = InputImageType::New();
  binary->SetRegions( input->GetLargestPossibleRegion() );
  binary->CopyInformation( input );
  binary->Allocate();
  binary->FillBuffer( itk::NumericTraits<InputPixelType>::ZeroValue() );

  InputImageType::RegionType region;
  InputImageType::IndexType index;
  InputImageType::SizeType size;

  index.Fill( 10 );
  size.Fill( 30 );

  region.SetIndex( index );
  region.SetSize( size );

  InputIteratorType iIt( binary, region );
  iIt.GoToBegin();
  while( !iIt.IsAtEnd() )
    {
    iIt.Set( itk::NumericTraits<InputPixelType>::OneValue() );
    ++iIt;
    }

  // Convert binary mask to sparse level set
  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage( binary );
  adaptor->Initialize();
  std::cout << "Finished converting to sparse format" << std::endl;

  SparseLevelSetType::Pointer level_set = adaptor->GetModifiableLevelSet();

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 1.0 );

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );

  bool LevelSetNotYetAdded = lscontainer->AddLevelSet( 0, level_set, false );
  if ( !LevelSetNotYetAdded )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Level set container created" << std::endl;

  // **************** CREATE ALL TERMS ****************

  // -----------------------------
  // *** 1st Level Set phi ***

  // Create ChanAndVese internal term for phi_{1}
  ChanAndVeseInternalTermType::Pointer cvInternalTerm0 = ChanAndVeseInternalTermType::New();
  cvInternalTerm0->SetInput( input );
  cvInternalTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: CV internal term created" << std::endl;

  // Create ChanAndVese external term for phi_{1}
  ChanAndVeseExternalTermType::Pointer cvExternalTerm0 = ChanAndVeseExternalTermType::New();
  cvExternalTerm0->SetInput( input );
  cvExternalTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: CV external term created" << std::endl;

  // Create ChanAndVese curvature term for phi_{1}
  CurvatureTermType::Pointer curvatureTerm0 = CurvatureTermType::New();
  curvatureTerm0->SetInput( input );
  curvatureTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: Curvature term created" << std::endl;

  // Create ChanAndVese curvature term for phi_{1}
  PropagationTermType::Pointer propagationTerm0 = PropagationTermType::New();
  propagationTerm0->SetInput( input );
  propagationTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: Propagation term created" << std::endl;

    // Create ChanAndVese curvature term for phi_{1}
  LaplacianTermType::Pointer laplacianTerm0 = LaplacianTermType::New();
  laplacianTerm0->SetInput( input );
  laplacianTerm0->SetCoefficient( 1.0 );
  std::cout << "LevelSet 1: Laplacian term created" << std::endl;

  // **************** CREATE ALL EQUATIONS ****************

  // Create Term Container
  TermContainerType::Pointer termContainer0 = TermContainerType::New();
  termContainer0->SetInput( input );
  termContainer0->SetCurrentLevelSetId( 0 );
  termContainer0->SetLevelSetContainer( lscontainer );

  termContainer0->AddTerm( 0, cvInternalTerm0 );
  termContainer0->AddTerm( 1, cvExternalTerm0 );
  termContainer0->AddTerm( 2, curvatureTerm0 );
  termContainer0->AddTerm( 3, propagationTerm0 );
  termContainer0->AddTerm( 4, laplacianTerm0 );

  std::cout << "Term container 0 created" << std::endl;

  EquationContainerType::Pointer equationContainer = EquationContainerType::New();
  equationContainer->AddEquation( 0, termContainer0 );

  typedef itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType >
      StoppingCriterionType;
  StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( 5 );

  LevelSetEvolutionType::Pointer evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer( equationContainer );
  evolution->SetStoppingCriterion( criterion );
  evolution->SetLevelSetContainer( lscontainer );

  try
    {
    evolution->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    }

  return EXIT_SUCCESS;
}
