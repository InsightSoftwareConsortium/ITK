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
#include "itkVTKVisualize2DSparseLevelSetLayers.h"
#include "itkSinRegularizedHeavisideStepFunction.h"

template< typename TInputImage, typename TLevelSetType >
void
visualizeLevelSet( TInputImage * inputImage,
                   const int numberOfIterations,
                   double epsilon )
{
  // Basic typedefs
  typedef TInputImage InputImageType;

  typedef TLevelSetType                         LevelSetType;
  typedef typename LevelSetType::OutputType     LevelSetOutputType;
  typedef typename LevelSetType::OutputRealType LevelSetRealType;

  // Generate a binary mask that will be used as initialization for the level
  // set evolution.
  typedef typename itk::Image< LevelSetOutputType, InputImageType::ImageDimension > BinaryImageType;
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

  typedef itk::ImageRegionIteratorWithIndex< BinaryImageType > InputIteratorType;
  InputIteratorType iIt( binary, region );
  iIt.GoToBegin();
  while( !iIt.IsAtEnd() )
    {
    iIt.Set( itk::NumericTraits< LevelSetOutputType >::OneValue() );
    ++iIt;
    }

  typedef itk::BinaryImageToLevelSetImageAdaptor< BinaryImageType,
    LevelSetType > BinaryImageToLevelSetType;

  typename BinaryImageToLevelSetType::Pointer adaptor = BinaryImageToLevelSetType::New();
  adaptor->SetInputImage( binary );
  adaptor->Initialize();
  typename LevelSetType::Pointer levelSet = adaptor->GetLevelSet();
  std::cout << "Finished converting to sparse format" << std::endl;

  // The Heaviside function
  typedef typename itk::SinRegularizedHeavisideStepFunction< LevelSetRealType, LevelSetRealType > HeavisideFunctionType;
  typename HeavisideFunctionType::Pointer heaviside = HeavisideFunctionType::New();
  heaviside->SetEpsilon( epsilon );
  std::cout << "Heaviside function created" << std::endl;

  // Create the level set container
  typedef typename itk::LevelSetContainer< itk::IdentifierType, LevelSetType > LevelSetContainerType;
  typename LevelSetContainerType::Pointer levelSetContainer = LevelSetContainerType::New();
  levelSetContainer->SetHeaviside( heaviside );
  levelSetContainer->AddLevelSet( 0, levelSet );
  std::cout << "LevelSetContainer created" << std::endl;

  // Create the terms.
  //
  // // Chan and Vese internal term
  typedef itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType > ChanAndVeseInternalTermType;
  typename ChanAndVeseInternalTermType::Pointer cvInternalTerm = ChanAndVeseInternalTermType::New();
  cvInternalTerm->SetInput( inputImage );
  cvInternalTerm->SetCoefficient( 0.5 );
  std::cout << "Chan and Vese internal term created" << std::endl;

  // // Chan and Vese external term
  typedef typename itk::LevelSetEquationChanAndVeseExternalTerm< InputImageType, LevelSetContainerType > ChanAndVeseExternalTermType;
  typename ChanAndVeseExternalTermType::Pointer cvExternalTerm = ChanAndVeseExternalTermType::New();
  cvExternalTerm->SetInput( inputImage );
  std::cout << "Chan and Vese external term created" << std::endl;

  // Create term container (equation rhs)
  typedef typename itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType > TermContainerType;
  typename TermContainerType::Pointer termContainer = TermContainerType::New();
  termContainer->SetLevelSetContainer( levelSetContainer );
  termContainer->SetInput( inputImage );
  termContainer->AddTerm( 0, cvInternalTerm );
  termContainer->AddTerm( 1, cvExternalTerm );
  std::cout << "Term container created" << std::endl;

  // Create equation container
  typedef typename itk::LevelSetEquationContainer< TermContainerType > EquationContainerType;
  typename EquationContainerType::Pointer equationContainer = EquationContainerType::New();
  equationContainer->SetLevelSetContainer( levelSetContainer );
  equationContainer->AddEquation( 0, termContainer );
  std::cout << "Equation container created" << std::endl;

  // Create stopping criteria
  typedef typename itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType > StoppingCriterionType;
  typename StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( numberOfIterations );
  std::cout << "Stopping criteria created" << std::endl;

  // Create the visualizer
  typedef itk::VTKVisualize2DSparseLevelSetLayers< InputImageType, LevelSetType > VisualizationType;
  typename VisualizationType::Pointer visualizer = VisualizationType::New();
  //! \todo the visualizer should get the input image from the level set
  visualizer->SetInputImage( inputImage );
  visualizer->SetLevelSet( levelSet );
  visualizer->SetScreenCapture( true );
  std::cout << "Visualizer created" << std::endl;

  // Create evolution class
  typedef typename itk::LevelSetEvolution< EquationContainerType, LevelSetType > LevelSetEvolutionType;
  typename LevelSetEvolutionType::Pointer evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer( equationContainer );
  evolution->SetStoppingCriterion( criterion );
  evolution->SetLevelSetContainer( levelSetContainer );
  std::cout << "Evolution class created" << std::endl;

  typedef typename itk::LevelSetIterationUpdateCommand< LevelSetEvolutionType, VisualizationType > IterationUpdateCommandType;
  typename IterationUpdateCommandType::Pointer iterationUpdateCommand = IterationUpdateCommandType::New();
  iterationUpdateCommand->SetFilterToUpdate( visualizer );
  iterationUpdateCommand->SetUpdatePeriod( 4 );
  evolution->AddObserver( itk::IterationEvent(), iterationUpdateCommand );
  std::cout << "Visualization IterationUpdateCommand created" << std::endl;

  std::cout << "Evolving the level set..." << std::endl;
  evolution->Update();

  //! \todo Write out the final visualization image.
}

int itkVTKVisualize2DCellsLevelSetLayersTest( int argc, char* argv[] )
{
  if( argc != 4 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << argv[0] << std::endl;
    std::cerr << "1- Input Image" << std::endl;
    std::cerr << "2- Number of Iterations" << std::endl;
    std::cerr << "3- LevelSet Representation (Dense, Whitaker, Shi, Malcolm)" << std::endl;

    return EXIT_FAILURE;
    }

  // Image Dimension
  const unsigned int Dimension = 2;

  typedef unsigned char                            InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >  InputImageType;

  // Read input image (to be processed).
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();
  InputImageType::Pointer input = reader->GetOutput();
  std::cout << "Input image read" << std::endl;

  // Convert the binary mask into a level set function.
  // Here the output level-set will be a "Whitaker" sparse level-set;
  // i.e. only few layers {-2, -1, 0, +1, +2 } around the zero-set are
  // maintained, the rest of the domain is either -3 or +3.

  int numberOfIterations;
  std::istringstream istrm( argv[2] );
  istrm >> numberOfIterations;

  std::string levelSetRepresentation = argv[3];
  if( levelSetRepresentation.compare( "Whitaker" ) == 0 )
    {
    typedef itk::WhitakerSparseLevelSetImage< double, 2 > LevelSetType;
    try
      {
      visualizeLevelSet< InputImageType, LevelSetType >( input,
                                                         numberOfIterations,
                                                         1.5 );
      }
    catch ( itk::ExceptionObject& err )
      {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }
  else if( levelSetRepresentation.compare( "Shi" ) == 0 )
    {
    typedef itk::ShiSparseLevelSetImage< 2 > LevelSetType;
    try
      {
      visualizeLevelSet< InputImageType, LevelSetType >( input,
                                                         numberOfIterations,
                                                         1.5 );
      }
    catch ( itk::ExceptionObject& err )
      {
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
    }
  else if( levelSetRepresentation.compare( "Malcolm" ) == 0 )
    {
    typedef itk::MalcolmSparseLevelSetImage< 2 > LevelSetType;
    try
      {
      visualizeLevelSet< InputImageType, LevelSetType >( input,
                                                         numberOfIterations,
                                                         1.5);
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
