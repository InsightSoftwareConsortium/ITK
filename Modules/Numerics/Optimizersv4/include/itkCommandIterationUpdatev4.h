/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkCommandIterationUpdatev4_h
#define itkCommandIterationUpdatev4_h

#include "itkCommand.h"
#include "itkWeakPointer.h"

namespace itk
{

/**
 * \class CommandIterationUpdatev4
 * \brief Implementation of the Command Pattern to be invoked every iteration
 * \ingroup ITKOptimizersv4
 */
template <typename TOptimizer>
class CommandIterationUpdatev4 : public Command
{
public:
  /**
   * Standard "Self" typedef.
   */
  using Self = CommandIterationUpdatev4;


  /**
   * Standard "Superclass" typedef.
   */
  using Superclass = itk::Command;


  /**
   * Smart pointer type alias support
   */
  using Pointer = itk::SmartPointer<Self>;

  /**
   * ConstSmart pointer type alias support
   */
  using ConstPointer = itk::SmartPointer<const Self>;

  /**
   * Execute method will print data at each iteration
   */
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object *, const itk::EventObject & event) override
  {
    if (typeid(event) == typeid(itk::StartEvent))
    {
      std::cout << std::endl << "Position              Value";
      std::cout << std::endl << std::endl;
    }
    else if (typeid(event) == typeid(itk::IterationEvent))
    {
      std::cout << m_Optimizer->GetCurrentIteration() << " = ";
      std::cout << m_Optimizer->GetValue();
      if (m_PrintParameters)
      {
        std::cout << " : " << m_Optimizer->GetCurrentPosition();
      }
      std::cout << std::endl;
    }
    else if (typeid(event) == typeid(itk::EndEvent))
    {
      std::cout << std::endl << std::endl;
      std::cout << "After " << m_Optimizer->GetCurrentIteration();
      std::cout << "  iterations " << std::endl;
      if (m_PrintParameters)
      {
        std::cout << " Solution is    = " << m_Optimizer->GetCurrentPosition();
        std::cout << std::endl;
      }
      std::cout << "Solution value     = " << m_Optimizer->GetValue();
      std::cout << std::endl;
      std::cout << "Stop condition = " << m_Optimizer->GetStopConditionDescription();
      std::cout << std::endl;
    }
  }


  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CommandIterationUpdatev4, ::itk::Command);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


  /**
   * Type defining the optimizer
   */
  using OptimizerType = TOptimizer;


  /**
   * Set Optimizer
   */
  void
  SetOptimizer(OptimizerType * optimizer)
  {
    m_Optimizer = optimizer;
    m_Optimizer->AddObserver(itk::IterationEvent(), this);
  }

  /**
   * Print parameters at each iteration
   */
  itkSetMacro(PrintParameters, bool);
  itkGetMacro(PrintParameters, bool);
  itkBooleanMacro(PrintParameters);


protected:
  /**
   * Constructor
   */
  CommandIterationUpdatev4(){};

private:
  /**
   *  WeakPointer to the Optimizer
   */
  WeakPointer<OptimizerType> m_Optimizer;

  bool m_PrintParameters{ false };
};

} // end namespace itk

#endif
