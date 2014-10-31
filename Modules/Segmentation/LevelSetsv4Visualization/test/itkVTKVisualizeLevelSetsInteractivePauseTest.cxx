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
#include "itkConditionVariable.h"
#include "itkLevelSetContainer.h"
#include "itkLevelSetEquationChanAndVeseInternalTerm.h"
#include "itkLevelSetEquationChanAndVeseExternalTerm.h"
#include "itkLevelSetEquationContainer.h"
#include "itkLevelSetEquationTermContainer.h"
#include "itkLevelSetEvolution.h"
#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"
#include "itkLevelSetDenseImage.h"
#include "itkMultiThreader.h"
#include "itkMutexLock.h"
#include "itkVTKVisualizeImageLevelSetIsoValues.h"
#include "itkSinRegularizedHeavisideStepFunction.h"

#include "vtkCommand.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkInteractorStyleImage.h"

const unsigned int Dimension = 2;
typedef unsigned char                                    InputPixelType;
typedef itk::Image< InputPixelType, Dimension >          InputImageType;
typedef float                                            LevelSetPixelType;
typedef itk::Image< LevelSetPixelType, Dimension >       LevelSetImageType;
typedef itk::LevelSetDenseImage< LevelSetImageType >     LevelSetType;

struct NeedToPauseInformation
{
  itk::SimpleMutexLock            m_Mutex;
  itk::ConditionVariable::Pointer m_ConditionVariable;
  bool                            m_NeedToPause;
  bool                            m_NeedToUpdateViz;

  NeedToPauseInformation():
      m_NeedToPause( false ),
      m_NeedToUpdateViz( false )
    {
    m_ConditionVariable = itk::ConditionVariable::New();
    }
};

/** \class ProcessingPauseCommand
 * Pause level sets processing so that the user can examine it or to render the
 * data. */
class ProcessingPauseCommand: public itk::Command
{
public:
  typedef ProcessingPauseCommand    Self;
  typedef Command                   Superclass;
  typedef itk::SmartPointer< Self > Pointer;

  itkNewMacro( Self );

  ProcessingPauseCommand(){}

  virtual void Execute( const itk::Object* caller, const itk::EventObject& event )
    {
    this->Execute( const_cast< itk::Object* >( caller ), event );
    }

  virtual void Execute( itk::Object* itkNotUsed(caller), const itk::EventObject& event )
    {
    if( itk::IterationEvent().CheckEvent( &event ))
      {
      this->m_NeedToPauseInformation->m_Mutex.Lock();
      std::cout << "An iteration occurred, checking if we need to pause...." << std::endl;
      if( this->m_NeedToPauseInformation->m_NeedToPause || this->m_NeedToPauseInformation->m_NeedToUpdateViz )
        {
        this->m_NeedToPauseInformation->m_ConditionVariable->Wait( &(this->m_NeedToPauseInformation->m_Mutex));
        std::cout << "Done pausing..." << std::endl;
        }
      this->m_NeedToPauseInformation->m_Mutex.Unlock();
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
class KeypressPauseCommand: public vtkCommand
{
public:
  typedef itk::VTKVisualizeImageLevelSetIsoValues< InputImageType, LevelSetType > VisualizationType;

  KeypressPauseCommand(){}

  static KeypressPauseCommand * New()
    {
    // Create the visualizer
    KeypressPauseCommand * keypressPauseCommand = new KeypressPauseCommand;
    keypressPauseCommand->m_Visualizer = VisualizationType::New();
    keypressPauseCommand->m_Visualizer->SetNumberOfLevels( 5 );
    keypressPauseCommand->m_Visualizer->SetLevelLimit( 4.0 );

    std::cout << "Visualizer created" << std::endl;
    return keypressPauseCommand;
    }

  virtual void Execute( vtkObject * vtkNotUsed(caller), unsigned long eventId, void * vtkNotUsed(callData) )
    {
    if( vtkCommand::TimerEvent == eventId )
      {
      bool weArePaused;
      bool weWantToUpdateViz;
      this->m_NeedToPauseInformation->m_Mutex.Lock();
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
          this->m_NeedToPauseInformation->m_ConditionVariable->Signal();
          }
        else // do it the next time around
          {
          this->m_NeedToPauseInformation->m_NeedToUpdateViz = true;
          }
        }
      this->m_NeedToPauseInformation->m_Mutex.Unlock();
      }
    else if( vtkCommand::KeyPressEvent == eventId )
      {
      this->m_NeedToPauseInformation->m_Mutex.Lock();
      std::cout << "Got a keypress event..." << std::endl;
      bool weArePaused = this->m_NeedToPauseInformation->m_NeedToPause;
      if( weArePaused )
        {
        this->m_NeedToPauseInformation->m_NeedToPause = false;
        this->m_NeedToPauseInformation->m_ConditionVariable->Signal();
        }
      else
        {
        this->m_NeedToPauseInformation->m_NeedToPause = true;
        }
      this->m_NeedToPauseInformation->m_Mutex.Unlock();
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
  VisualizationType::Pointer m_Visualizer;
};

template< typename TInputImage, typename TLevelSetType >
void
visualizeLevelSet( TInputImage * inputImage,
  const int numberOfIterations,
  ProcessingPauseCommand * pauseCommand,
  KeypressPauseCommand * keypressCommand )
{
  typedef typename LevelSetType::OutputType                LevelSetOutputType;
  typedef typename LevelSetType::OutputRealType            LevelSetRealType;

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
  heaviside->SetEpsilon( 1.5 );
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


  // Create evolution class
  typedef typename itk::LevelSetEvolution< EquationContainerType, LevelSetType > LevelSetEvolutionType;
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

struct VisualizationThreadData
{
  InputImageType *          m_InputImage;
  unsigned int              m_NumberOfIterations;
  ProcessingPauseCommand *  m_ProcessingPauseCommand;
  KeypressPauseCommand *    m_KeypressPauseCommand;
};

ITK_THREAD_RETURN_TYPE visualizationThreadRunner( void * threadInfo )
{
  itk::MultiThreader::ThreadInfoStruct* info =
    static_cast<itk::MultiThreader::ThreadInfoStruct*>( threadInfo );

  VisualizationThreadData * visualizationThreadData = static_cast< VisualizationThreadData * >( info->UserData );
  visualizeLevelSet< InputImageType, LevelSetType >( visualizationThreadData->m_InputImage,
    visualizationThreadData->m_NumberOfIterations,
    visualizationThreadData->m_ProcessingPauseCommand,
    visualizationThreadData->m_KeypressPauseCommand );

  return ITK_THREAD_RETURN_VALUE;
}

class ExitOnTimer: public vtkCommand
{
public:
  ExitOnTimer(){}

  static ExitOnTimer * New()
    {
    return new ExitOnTimer;
    }

  virtual void Execute( vtkObject * caller, unsigned long eventId, void * callData )
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

  // Read input image (to be processed).
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
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

  vtkSmartPointer< KeypressPauseCommand > keypressPauseCommand = vtkSmartPointer< KeypressPauseCommand >::New();
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


  VisualizationThreadData visualizationThreadData;
  visualizationThreadData.m_InputImage = input.GetPointer();
  visualizationThreadData.m_NumberOfIterations = numberOfIterations;
  visualizationThreadData.m_ProcessingPauseCommand = processingPauseCommand.GetPointer();
  visualizationThreadData.m_KeypressPauseCommand = keypressPauseCommand.GetPointer();


  itk::MultiThreader::Pointer threader = itk::MultiThreader::New();
  try
    {
    itk::ThreadIdType threadId = threader->SpawnThread( visualizationThreadRunner, &visualizationThreadData );
    renderWindowInteractor->Start();
    std::cout << "The spawned thread was: " << threadId << std::endl;
    std::cout << "TerminatingThread..." << std::endl;
    threader->TerminateThread( threadId );
    }
  catch ( itk::ExceptionObject& err )
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
