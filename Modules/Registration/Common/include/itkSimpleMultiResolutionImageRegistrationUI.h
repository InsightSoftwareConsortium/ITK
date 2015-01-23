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
#ifndef itkSimpleMultiResolutionImageRegistrationUI_h
#define itkSimpleMultiResolutionImageRegistrationUI_h

#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkCommand.h"
#include "itkArray.h"
#include "itkGradientDescentOptimizer.h"

// The following classes are examples of simple user interface
// that controls a MultiResolutionImageRegistrationMethod process

template <typename TRegistrator>
class SimpleMultiResolutionImageRegistrationUI
{
public:
  SimpleMultiResolutionImageRegistrationUI( TRegistrator * ptr ):
    m_Tag(0)
    {

    if ( !ptr ) return;
    m_Registrator = ptr;
    typename itk::SimpleMemberCommand<SimpleMultiResolutionImageRegistrationUI>::Pointer
      iterationCommand =
    itk::SimpleMemberCommand<SimpleMultiResolutionImageRegistrationUI>::New();

    iterationCommand->SetCallbackFunction( this,
      &SimpleMultiResolutionImageRegistrationUI::StartNewLevel );

    m_Tag = m_Registrator->AddObserver( itk::IterationEvent(), iterationCommand );

    }

  virtual ~SimpleMultiResolutionImageRegistrationUI()
    {
    if( m_Registrator ) { m_Registrator->RemoveObserver( m_Tag ); }
    }

   virtual void StartNewLevel()
    {
    std::cout << "--- Starting level " << m_Registrator->GetCurrentLevel()
              << std::endl;
    }

protected:
  typename TRegistrator::Pointer  m_Registrator;
  unsigned long                   m_Tag;

};


// This UI supports registration methods with gradient descent
// type optimizers.
// This UI allows the number of iterations and learning rate
// to be changes at each resolution level.
template <typename TRegistration>
class SimpleMultiResolutionImageRegistrationUI2 :
  public SimpleMultiResolutionImageRegistrationUI<TRegistration>
{
public:

  typedef SimpleMultiResolutionImageRegistrationUI<TRegistration>
    Superclass;

  SimpleMultiResolutionImageRegistrationUI2( TRegistration * ptr ) :
    Superclass(ptr) {};
  virtual ~SimpleMultiResolutionImageRegistrationUI2(){}

  void SetNumberOfIterations( itk::Array<unsigned int> & iter )
    {
    m_NumberOfIterations = iter;
    }

  void SetLearningRates( itk::Array<double> & rates )
    {
    m_LearningRates = rates;
    }

  virtual void StartNewLevel()
    {

    // call the superclass's implementation
    this->Superclass::StartNewLevel();

    if ( !this->m_Registrator ) return;

    // Try to cast the optimizer to a gradient descent type,
    // return if casting didn't work.
    itk::GradientDescentOptimizer::Pointer optimizer = dynamic_cast< itk::GradientDescentOptimizer * >(
      this->m_Registrator->GetModifiableOptimizer() );
    if ( !optimizer ) return;

    unsigned int level = this->m_Registrator->GetCurrentLevel();
    if ( m_NumberOfIterations.Size() >= level + 1 )
      {
      optimizer->SetNumberOfIterations( m_NumberOfIterations[level] );
      }

    if ( m_LearningRates.Size() >= level + 1 )
      {
      optimizer->SetLearningRate( m_LearningRates[level] );
      }

    std::cout << " No. Iterations: "
              << optimizer->GetNumberOfIterations()
              << " Learning rate: "
              << optimizer->GetLearningRate()
              << std::endl;
    }

private:
   itk::Array<unsigned int> m_NumberOfIterations;
   itk::Array<double>       m_LearningRates;

};


#endif
