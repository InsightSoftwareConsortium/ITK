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
#include "itkImageFileWriter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEquationContainer.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkLevelSetEvolution.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"

int itkSingleLevelSetDenseImage2DTest( int argc, char* argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned short                                      InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >             InputImageType;
  typedef itk::ImageFileReader< InputImageType >              ReaderType;

  typedef float                                          PixelType;
  typedef itk::Image< PixelType, Dimension >             ImageType;
  typedef itk::LevelSetDenseImage< ImageType >           LevelSetType;
  typedef LevelSetType::OutputRealType                   LevelSetOutputRealType;
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;

  typedef itk::IdentifierType                                     IdentifierType;
  typedef itk::LevelSetContainer< IdentifierType, LevelSetType >  LevelSetContainerType;
  typedef itk::LevelSetEquationChanAndVeseInternalTerm< InputImageType, LevelSetContainerType >
                                                                      ChanAndVeseInternalTermType;
  typedef itk::LevelSetEquationChanAndVeseExternalTerm< InputImageType, LevelSetContainerType >
                                                                      ChanAndVeseExternalTermType;
  typedef itk::LevelSetEquationTermContainer< InputImageType, LevelSetContainerType >
                                                                      TermContainerType;

  typedef itk::LevelSetEquationContainer< TermContainerType >     EquationContainerType;

  typedef itk::LevelSetEvolution< EquationContainerType, LevelSetType > LevelSetEvolutionType;

  typedef itk::AtanRegularizedHeavisideStepFunction<
      LevelSetOutputRealType, LevelSetOutputRealType >          HeavisideFunctionBaseType;

  typedef  itk::FastMarchingImageFilter< ImageType, ImageType > FastMarchingFilterType;
  typedef FastMarchingFilterType::NodeContainer                 NodeContainer;
  typedef FastMarchingFilterType::NodeType                      NodeType;

  // Read the image to be segmented
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();
  InputImageType::Pointer input = reader->GetOutput();

  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();

  NodeContainer::Pointer seeds = NodeContainer::New();

  ImageType::IndexType  seedPosition;
  seedPosition[0] = atoi( argv[2] );
  seedPosition[1] = atoi( argv[3] );

  const double initialDistance = atof( argv[4] );
  const double seedValue = - initialDistance;

  NodeType node;
  node.SetValue( seedValue );
  node.SetIndex( seedPosition );

  //  The list of nodes is initialized and then every node is inserted using
  //  the \code{InsertElement()}.
  //
  seeds->Initialize();
  seeds->InsertElement( 0, node );

  //  The set of seed nodes is passed now to the
  //  FastMarchingImageFilter with the method
  //  \code{SetTrialPoints()}.
  //
  fastMarching->SetTrialPoints(  seeds  );

  //  Since the FastMarchingImageFilter is used here just as a
  //  Distance Map generator. It does not require a speed image as input.
  //  Instead the constant value $1.0$ is passed using the
  //  \code{SetSpeedConstant()} method.
  //
  fastMarching->SetSpeedConstant( 1.0 );

  //  The FastMarchingImageFilter requires the user to specify the
  //  size of the image to be produced as output. This is done using the
  //  \code{SetOutputSize()}. Note that the size is obtained here from the
  //  output image of the smoothing filter. The size of this image is valid
  //  only after the \code{Update()} methods of this filter has been called
  //  directly or indirectly.
  //
  fastMarching->SetOutputSize( input->GetBufferedRegion().GetSize() );
  fastMarching->Update();

  // Define the Heaviside function
  HeavisideFunctionBaseType::Pointer heaviside = HeavisideFunctionBaseType::New();
  heaviside->SetEpsilon( 1.0 );

  // Map of levelset bases
  LevelSetType::Pointer  level_set = LevelSetType::New();
  level_set->SetImage( fastMarching->GetOutput() );

  // Insert the levelsets in a levelset container
  LevelSetContainerType::Pointer lscontainer = LevelSetContainerType::New();
  lscontainer->SetHeaviside( heaviside );

  bool levelSetNotYetAdded = lscontainer->AddLevelSet( 0, level_set, false );
  if ( !levelSetNotYetAdded )
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

  // **************** CREATE ALL EQUATIONS ****************

  // Create Term Container
  TermContainerType::Pointer termContainer0 = TermContainerType::New();
  termContainer0->SetInput( input );
  termContainer0->SetCurrentLevelSetId( 0 );
  termContainer0->SetLevelSetContainer( lscontainer );

  termContainer0->AddTerm( 0, cvInternalTerm0 );
  termContainer0->AddTerm( 1, cvExternalTerm0 );
  std::cout << "Term container 0 created" << std::endl;

  EquationContainerType::Pointer equationContainer = EquationContainerType::New();
  equationContainer->SetLevelSetContainer( lscontainer );
  equationContainer->AddEquation( 0, termContainer0 );

  typedef itk::LevelSetEvolutionNumberOfIterationsStoppingCriterion< LevelSetContainerType >
      StoppingCriterionType;
  StoppingCriterionType::Pointer criterion = StoppingCriterionType::New();
  criterion->SetNumberOfIterations( 50 );

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
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  ImageType::Pointer outputImage = ImageType::New();
  outputImage->SetRegions( input->GetLargestPossibleRegion() );
  outputImage->CopyInformation( input );
  outputImage->Allocate();
  outputImage->FillBuffer( 0 );

  IteratorType oIt( outputImage, outputImage->GetLargestPossibleRegion() );
  oIt.GoToBegin();

  ImageType::IndexType idx;

  while( !oIt.IsAtEnd() )
    {
    idx = oIt.GetIndex();
    oIt.Set( level_set->Evaluate( idx ) );
    ++oIt;
    }

  typedef itk::ImageFileWriter< ImageType > OutputWriterType;
  OutputWriterType::Pointer writer = OutputWriterType::New();
  writer->SetFileName( argv[5] );
  writer->SetInput( outputImage );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    }

  PixelType mean = cvInternalTerm0->GetMean();
  if ( ( mean < 24900 ) || ( mean > 24910 ) )
    {
    std::cerr << "( ( mean < 24900 ) || ( mean > 24910 ) )" <<std::endl;
    std::cerr << "mean = " <<mean <<std::endl;
    return EXIT_FAILURE;
    }

  mean = cvExternalTerm0->GetMean();
  if ( ( mean < 1350 ) || ( mean > 1360 ) )
    {
    std::cerr << "( ( mean < 1350 ) || ( mean > 1360 ) )" <<std::endl;
    std::cerr << "mean = " <<mean <<std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
