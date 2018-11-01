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
#include "itkCommand.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationContainer.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEvolution.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkLevelSetDenseImage.h"
#include <mutex>
#include <condition_variable>
#include "itkVTKVisualizeImageLevelSetIsoValues.h"
#include "itkSinRegularizedHeavisideStepFunction.h"

#include "vtkCommand.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkInteractorStyleImage.h"

#include <thread>

struct NeedToPauseInformation
{
  std::mutex              m_Mutex;
  std::condition_variable m_ConditionVariable;
  bool                    m_NeedToPause = false;
  bool                    m_NeedToUpdateViz = false;
};

/** \class ProcessingPauseCommand
 * Pause level sets processing so that the user can examine it or to render the
 * data. */
class ProcessingPauseCommand: public itk::Command
{
public:
  using Self = ProcessingPauseCommand;
  using Superclass = Command;
  using Pointer = itk::SmartPointer< Self >;

  itkNewMacro( Self );

  ProcessingPauseCommand(){}

  void Execute( const itk::Object* caller, const itk::EventObject& event ) override
    {
    this->Execute( const_cast< itk::Object* >( caller ), event );
    }

  void Execute( itk::Object* itkNotUsed(caller), const itk::EventObject& event ) override
    {
    if( itk::IterationEvent().CheckEvent( &event ))
      {
      std::unique_lock< std::mutex > mutexHolder( this->m_NeedToPauseInformation->m_Mutex );
      std::cout << "An iteration occurred, checking if we need to pause...." << std::endl;
      if( this->m_NeedToPauseInformation->m_NeedToPause || this->m_NeedToPauseInformation->m_NeedToUpdateViz )
        {
        this->m_NeedToPauseInformation->m_ConditionVariable.wait( mutexHolder );
        std::cout << "Done pausing..." << std::endl;
        }
      }
    }

  void SetNeedToPauseInformation( NeedToPauseInformation * pauseInfo )
    {
    this->m_NeedToPauseInformation = pauseInfo;
    }

private:
  NeedToPauseInformation * m_NeedToPauseInformation;
};

/** \class KeypressPauseCommand
 * The VTK class to detect the keypress and signal that a pause is needed.
 * */
template <typename TInputImage>
class KeypressPauseCommand: public vtkCommand
{
public:
  using LevelSetPixelType = float;
  static constexpr unsigned int Dimension = TInputImage::ImageDimension;
  using LevelSetImageType = itk::Image< LevelSetPixelType, Dimension >;
  using LevelSetType = itk::LevelSetDenseImage< LevelSetImageType >;
  using VisualizationType = itk::VTKVisualizeImageLevelSetIsoValues< TInputImage, LevelSetType >;

  KeypressPauseCommand(){}

  static KeypressPauseCommand * New()
    {
    // Create the visualizer
    auto * keypressPauseCommand = new KeypressPauseCommand;
    keypressPauseCommand->m_Visualizer = VisualizationType::New();
    keypressPauseCommand->m_Visualizer->SetNumberOfLevels( 5 );
    keypressPauseCommand->m_Visualizer->SetLevelLimit( 4.0 );

    std::cout << "Visualizer created" << std::endl;
    return keypressPauseCommand;
    }

  void Execute( vtkObject * vtkNotUsed(caller), unsigned long eventId, void * vtkNotUsed(callData) ) override
    {
    if( vtkCommand::TimerEvent == eventId )
      {
      bool weArePaused;
      bool weWantToUpdateViz;
      this->m_NeedToPauseInformation->m_Mutex.lock();
      //std::cout << "We got a timer event" << std::endl;
      weArePaused = this->m_NeedToPauseInformation->m_NeedToPause;
      weWantToUpdateViz = this->m_NeedToPauseInformation->m_NeedToUpdateViz;
      if( !weArePaused || (weArePaused && weWantToUpdateViz) )
        {
        // do a render
        // of the modified level set
        if( weWantToUpdateViz )
          {
          std::cout << "Updating the Visualization.." << std::endl;
          this->m_Visualizer->Update();
          this->m_Visualizer->GetRenderer()->ResetCamera();
          this->m_NeedToPauseInformation->m_NeedToUpdateViz = false;
          this->m_NeedToPauseInformation->m_ConditionVariable.notify_one();
          }
        else // do it the next time around
          {
          this->m_NeedToPauseInformation->m_NeedToUpdateViz = true;
          }
        }
      this->m_NeedToPauseInformation->m_Mutex.unlock();
      }
    else if( vtkCommand::KeyPressEvent == eventId )
      {
      this->m_NeedToPauseInformation->m_Mutex.lock();
      std::cout << "Got a keypress event..." << std::endl;
      bool weArePaused = this->m_NeedToPauseInformation->m_NeedToPause;
      if( weArePaused )
        {
        this->m_NeedToPauseInformation->m_NeedToPause = false;
        this->m_NeedToPauseInformation->m_ConditionVariable.notify_one();
        }
      else
        {
        this->m_NeedToPauseInformation->m_NeedToPause = true;
        }
      this->m_NeedToPauseInformation->m_Mutex.unlock();
      }
    }

  void SetNeedToPauseInformation( NeedToPauseInformation * pauseInfo )
    {
    this->m_NeedToPauseInformation = pauseInfo;
    }

  VisualizationType * GetVisualizer()
    {
    return this->m_Visualizer.GetPointer();
    }

private:
  NeedToPauseInformation *   m_NeedToPauseInformation;
  typename VisualizationType::Pointer m_Visualizer;
};

template< typename TInputImage >
void
visualizeLevelSet( TInputImage * inputImage,
  const int numberOfIterations,
  ProcessingPauseCommand * pauseCommand,
  KeypressPauseCommand<TInputImage> * keypressCommand )
{
  using LevelSetType = typename KeypressPauseCommand<TInputImage>::LevelSetType;
  using LevelSetOutputType = typename LevelSetType::OutputType;
  using LevelSetRealType = typename LevelSetType::OutputRealType;

  // Generate a binary mask that will be used as initialization for the level
  // set evolution.
  using BinaryImageType = typename itk::Image< LevelSetOutputType, TInputImage::ImageDimension >;
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
  using ChanAndVeseInternalTermType = itk::LevelSetEquationChanAndVeseInternalTerm< TInputImage, LevelSetContainerType >;
  typename ChanAndVeseInternalTermType::Pointer cvInternalTerm = ChanAndVeseInternalTermType::New();
  cvInternalTerm->SetInput( inputImage );
  cvInternalTerm->SetCoefficient( 0.5 );
  std::cout << "Chan and Vese internal term created" << std::endl;

  // // Chan and Vese external term
  using ChanAndVeseExternalTermType = typename itk::LevelSetEquationChanAndVeseExternalTerm< TInputImage, LevelSetContainerType >;
  typename ChanAndVeseExternalTermType::Pointer cvExternalTerm = ChanAndVeseExternalTermType::New();
  cvExternalTerm->SetInput( inputImage );
  std::cout << "Chan and Vese external term created" << std::endl;

  // Create term container (equation rhs)
  using TermContainerType = typename itk::LevelSetEquationTermContainer< TInputImage, LevelSetContainerType >;
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


  // Create evolution class
  using LevelSetEvolutionType = typename itk::LevelSetEvolution< EquationContainerType, LevelSetType >;
  typename LevelSetEvolutionType::Pointer evolution = LevelSetEvolutionType::New();
  evolution->SetEquationContainer( equationContainer );
  evolution->SetStoppingCriterion( criterion );
  evolution->SetLevelSetContainer( levelSetContainer );
  std::cout << "Evolution class created" << std::endl;
  evolution->AddObserver( itk::IterationEvent(), pauseCommand );
  keypressCommand->GetVisualizer()->SetLevelSet( levelSet );
  //! \todo the visualizer should get the input image from the level set
  keypressCommand->GetVisualizer()->SetInputImage( inputImage );
  std::cout << "ProcessingPauseCommand observing" << std::endl;

  std::cout << "Evolving the level set..." << std::endl;
  evolution->Update();
}


class ExitOnTimer: public vtkCommand
{
public:
  ExitOnTimer(){}

  static ExitOnTimer * New()
    {
    return new ExitOnTimer;
    }

  void Execute( vtkObject * caller, unsigned long eventId, void * callData ) override
    {
    if( vtkCommand::TimerEvent == eventId )
      {
      int timerId = * static_cast<int *>( callData );
      if( timerId == this->m_TimerId )
        {
        vtkRenderWindowInteractor * renderWindowInteractor = vtkRenderWindowInteractor::SafeDownCast( caller );
        renderWindowInteractor->ExitCallback();
        }
      }
    }

  void SetTimerId( const int id )
    {
    this->m_TimerId = id;
    }

private:
  int m_TimerId;
};

int itkVTKVisualizeLevelSetsInteractivePauseTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << argv[0] << std::endl;
    std::cerr << "1- Input Image" << std::endl;
    std::cerr << "2- Number of Iterations" << std::endl;

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

  NeedToPauseInformation needToPauseInformation;

  ProcessingPauseCommand::Pointer processingPauseCommand = ProcessingPauseCommand::New();
  processingPauseCommand->SetNeedToPauseInformation( &needToPauseInformation );

  using KPCType = KeypressPauseCommand<InputImageType>;
  vtkSmartPointer< KPCType > keypressPauseCommand = vtkSmartPointer< KPCType >::New();
  keypressPauseCommand->SetNeedToPauseInformation( &needToPauseInformation );
  vtkSmartPointer< vtkRenderWindow > renderWindow = vtkSmartPointer< vtkRenderWindow >::New();
  keypressPauseCommand->GetVisualizer()->SetRenderWindow( renderWindow );

  vtkSmartPointer< vtkRenderWindowInteractor > renderWindowInteractor = vtkSmartPointer< vtkRenderWindowInteractor >::New();
  renderWindowInteractor->SetRenderWindow( renderWindow );
  renderWindowInteractor->Initialize();
  renderWindowInteractor->CreateRepeatingTimer( 50 );

  vtkSmartPointer< vtkInteractorStyleImage > interactorStyle = vtkSmartPointer< vtkInteractorStyleImage >::New();
  renderWindowInteractor->SetInteractorStyle( interactorStyle );

  int timerId = renderWindowInteractor->CreateOneShotTimer( 10000 );
  vtkSmartPointer< ExitOnTimer > exitOnTimer = vtkSmartPointer< ExitOnTimer >::New();
  exitOnTimer->SetTimerId( timerId );

  renderWindowInteractor->AddObserver( vtkCommand::TimerEvent, exitOnTimer );
  renderWindowInteractor->AddObserver( vtkCommand::TimerEvent, keypressPauseCommand );
  renderWindowInteractor->AddObserver( vtkCommand::KeyPressEvent, keypressPauseCommand );

  try
    {
    std::thread visThread(
        visualizeLevelSet< InputImageType >,
        input.GetPointer(),
        numberOfIterations,
        processingPauseCommand.GetPointer(),
        keypressPauseCommand.GetPointer()
        );
    renderWindowInteractor->Start();
    std::cout << "The spawned thread was: " << visThread.get_id() << std::endl;
    std::cout << "TerminatingThread..." << std::endl;
    visThread.join();
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
