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

#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkImageFileReader.h"
#include "itkLevelSetIterationUpdateCommand.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationContainer.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEvolution.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkLevelSetDenseImage.h"
#include "itkVTKVisualize2DLevelSetAsElevationMap.h"
#include "itkSinRegularizedHeavisideStepFunction.h"

template< typename TInputImage, typename TLevelSetType >
void
VisualizeLevelSetSurface( TInputImage * inputImage, const int numberOfIterations, const char * )
{
  // Basic type alias
  using InputImageType = TInputImage;

  using LevelSetType = TLevelSetType;
  using LevelSetOutputType = typename LevelSetType::OutputType;
  using LevelSetRealType = typename LevelSetType::OutputRealType;

  // Generate a binary mask that will be used as initialization for the level
  // set evolution.
  using BinaryImageType = typename itk::Image< LevelSetOutputType, InputImageType::ImageDimension >;
  typename BinaryImageType::Pointer binary = BinaryImageType::New();
  binary->SetRegions( inputImage->GetLargestPossibleRegion() );
  binary->CopyInformation( inputImage );
  binary->Allocate();
  binary->FillBuffer( itk::NumericTraits< LevelSetOutputType >::ZeroValue() );

  typename BinaryImageType::RegionType region;
  typename BinaryImageType::IndexType  index;
  typename BinaryImageType::SizeType   size;

  index.Fill( 5 );
  size.Fill( 120 );

  region.SetIndex( index );
  region.SetSize( size );

  using InputIteratorType = itk::ImageRegionIteratorWithIndex< BinaryImageType >;
  InputIteratorType iIt( binary, region );
  iIt.GoToBegin();
  while( !iIt.IsAtEnd() )
    {
    iIt.Set( itk::NumericTraits< LevelSetOutputType >::OneValue() );
    ++iIt;
    }

  using BinaryImageToLevelSetType = itk::BinaryImageToLevelSetImageAdaptor< BinaryImageType,
    LevelSetType >;

  typename BinaryImageToLevelSetType::Pointer adaptor = BinaryImageToLevelSetType::New();
  adaptor->SetInputImage( binary );
  adaptor->Initialize();
  typename LevelSetType::Pointer levelSet = adaptor->GetModifiableLevelSet();
  std::cout << "Finished converting to sparse format" << std::endl;

  // The Heaviside function
  using HeavisideFunctionType = typename itk::SinRegularizedHeavisideStepFunction< LevelSetRealType, LevelSetRealType >;
  typename HeavisideFunctionType::Pointer heaviside = HeavisideFunctionType::New();
  heaviside->SetEpsilon( 1.5 );
  std::cout << "Heaviside function created" << std::endl;

  // Create the level set container
  using LevelSetContainerType = typename itk::LevelSetContainer< itk::IdentifierType, LevelSetType >;
  typename LevelSetContainerType::Pointer levelSetContainer = LevelSetContainerType::New();
  levelSetContainer->SetHeaviside( heaviside );
  levelSetContainer->AddLevelSet( 0, levelSet );
  std::cout << "LevelSetContainer created" << std::endl;

  // Create the terms.
  //
  // // Chan and Vese internal term
  using ChanAndVeseInternalTermType = itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType >;
  typename ChanAndVeseInternalTermType::Pointer cvInternalTerm = ChanAndVeseInternalTermType::New();
  cvInternalTerm->SetInput( inputImage );
  cvInternalTerm->SetCoefficient( 0.5 );
  std::cout << "Chan and Vese internal term created" << std::endl;

  // // Chan and Vese external term
  using ChanAndVeseExternalTermType = typename itk::LevelSetEquationChanAndVeseExternalTerm< InputImageType, LevelSetContainerType >;
  typename ChanAndVeseExternalTermType::Pointer cvExternalTerm = ChanAndVeseExternalTermType::New();
  cvExternalTerm->SetInput( inputImage );
  std::cout << "Chan and Vese external term created" << std::endl;

  // Create term container (equation rhs)
  using TermContainerType = typename itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType >;
  typename TermContainerType::Pointer termContainer = TermContainerType::New();
  termContainer->SetLevelSetContainer( levelSetContainer );
  termContainer->SetInput( inputImage );
  termContainer->AddTerm( 0, cvInternalTerm );
  termContainer->AddTerm( 1, cvExternalTerm );
  std::cout << "Term container created" << std::endl;

  // Create equation container
  using EquationContainerType = typename itk::LevelSetEquationContainer< TermContainerType >;
  typename EquationContainerType::Pointer equationContainer = EquationContainerType::New();
  equationContainer->SetLevelSetContainer( levelSetContainer );
  equationContainer->AddEquation( 0, termContainer );
  std::cout << "Equation container created" << std::endl;

  // Create stopping criteria
  using StoppingCriterionType = typename itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType >;
  typename StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( numberOfIterations );
  std::cout << "Stopping criteria created" << std::endl;

  // Create the visualizer
  using VisualizationType = itk::VTKVisualize2DLevelSetAsElevationMap< InputImageType, LevelSetType >;
  typename VisualizationType::Pointer visualizer = VisualizationType::New();
  //! \todo the visualizer should get the input image from the level set
  visualizer->SetInputImage( inputImage );
  visualizer->SetLevelSet( levelSet );
  std::cout << "Visualizer created" << std::endl;

  // Create evolution class
  using LevelSetEvolutionType = typename itk::LevelSetEvolution< EquationContainerType, LevelSetType >;
  typename LevelSetEvolutionType::Pointer evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer( equationContainer );
  evolution->SetStoppingCriterion( criterion );
  evolution->SetLevelSetContainer( levelSetContainer );
  std::cout << "Evolution class created" << std::endl;

  using IterationUpdateCommandType = typename itk::LevelSetIterationUpdateCommand< LevelSetEvolutionType, VisualizationType >;
  typename IterationUpdateCommandType::Pointer iterationUpdateCommand = IterationUpdateCommandType::New();
  iterationUpdateCommand->SetFilterToUpdate( visualizer );
  iterationUpdateCommand->SetUpdatePeriod( 5 );
  evolution->AddObserver( itk::IterationEvent(), iterationUpdateCommand );
  std::cout << "Visualization IterationUpdateCommand created" << std::endl;

  std::cout << "Evolving the level set..." << std::endl;
  evolution->Update();

  //! \todo Write out the final visualization image.
}

int itkVTKVisualize2DCellsLevelSetSurfaceTest( int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << argv[0] << std::endl;
    std::cerr << "1- Input Image" << std::endl;
    std::cerr << "2- Number of Iterations" << std::endl;
    std::cerr << "3- LevelSet Representation (Dense, Whitaker, Shi, Malcolm)" << std::endl;
    std::cerr << "4- Output Image" << std::endl;

    return EXIT_FAILURE;
    }

  // Image Dimension
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;
  using InputImageType = itk::Image< InputPixelType, Dimension >;

  // Read input image (to be processed).
  using ReaderType = itk::ImageFileReader< InputImageType >;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();
  InputImageType::Pointer input = reader->GetOutput();
  std::cout << "Input image read" << std::endl;

  int numberOfIterations;
  std::istringstream istrm( argv[2] );
  istrm >> numberOfIterations;

  std::string levelSetRepresentation = argv[3];
  if( levelSetRepresentation.compare( "Dense" ) == 0 )
    {
    using LevelSetPixelType = float;
    using LevelSetImageType = itk::Image< LevelSetPixelType, Dimension >;
    using LevelSetType = itk::LevelSetDenseImage< LevelSetImageType >;
    try
      {
      VisualizeLevelSetSurface< InputImageType, LevelSetType >( input,
                                                                numberOfIterations,
                                                                argv[4] );
      }
    catch ( itk::ExceptionObject& err )
      {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }
  else if( levelSetRepresentation.compare( "Whitaker" ) == 0 )
    {
    using LevelSetType = itk::WhitakerSparseLevelSetImage< double, Dimension >;
    try
      {
      VisualizeLevelSetSurface< InputImageType, LevelSetType >( input,
                                                                numberOfIterations,
                                                                argv[4] );
      }
    catch ( itk::ExceptionObject& err )
      {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }
  else if( levelSetRepresentation.compare( "Shi" ) == 0 )
    {
    using LevelSetType = itk::ShiSparseLevelSetImage< Dimension >;
    try
      {
      VisualizeLevelSetSurface< InputImageType, LevelSetType >( input,
                                                                numberOfIterations,
                                                                argv[4] );
      }
    catch ( itk::ExceptionObject& err )
      {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }
  else if( levelSetRepresentation.compare( "Malcolm" ) == 0 )
    {
    using LevelSetType = itk::MalcolmSparseLevelSetImage< Dimension >;
    try
      {
      VisualizeLevelSetSurface< InputImageType, LevelSetType >( input,
                                                                numberOfIterations,
                                                                argv[4] );
      }
    catch ( itk::ExceptionObject& err )
      {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }
  else
    {
    std::cerr << "Unknown level set representation " << levelSetRepresentation << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
