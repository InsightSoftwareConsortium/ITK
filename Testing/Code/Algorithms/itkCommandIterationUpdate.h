/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkCommandIterationUpdate.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#ifndef __itkCommandIterationUpdate_h
#define __itkCommandIterationUpdate_h

#include <itkCommand.h>
#include "itkWeakPointer.h"

namespace itk {

/**
 *  Implementation of the Command Pattern to be invoked every iteration
 */
template < class TOptimizer >
class ITK_EXPORT CommandIterationUpdate : public Command 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CommandIterationUpdate   Self;


  /**
   * Standard "Superclass" typedef.
   */
  typedef itk::Command  Superclass;


  /**
   * Smart pointer typedef support.
   */
  typedef itk::SmartPointer<Self>  Pointer;

  /** 
   * ConstSmart pointer typedef support.
   */
  typedef itk::SmartPointer<const Self>  ConstPointer;

  /**
   * Execute method will print data at each iteration
   */
  void Execute(itk::LightObject *caller, unsigned long event)
  {
    Execute( (const itk::LightObject *)caller, event);
  }

  void Execute(const itk::LightObject *caller, unsigned long event)
  {
    switch( event )
    {
      case  Command::StartEvent:
        std::cout << std::endl << "Position              Value";
        std::cout << std::endl << std::endl;
        break;
      case  Command::IterationEvent:
        std::cout << m_Optimizer->GetCurrentPosition() << "  ";
        std::cout << m_Optimizer->GetValue() << std::endl;
        break;
      case  Command::EndEvent:
        std::cout << std::endl << std::endl;
        std::cout << "After " << m_Optimizer->GetCurrentIteration();
        std::cout << "  iterations " << std::endl;
        std::cout << "Solution is    = " << m_Optimizer->GetCurrentPosition();
        std::cout << std::endl;
        std::cout << "With value     = " << m_Optimizer->GetValue();
        std::cout << std::endl;
        std::cout << "Stop condition = " << m_Optimizer->GetStopCondition();
        std::cout << std::endl;
        break;
    }

  }


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( CommandIterationUpdate, ::itk::Command );


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro( Self );

  
  /**
   * Type defining the optimizer
   */
  typedef    TOptimizer     OptimizerType;


  /**
   * Set Optimizer
   */
  void SetOptimizer( OptimizerType * optimizer )
                          { m_Optimizer = optimizer; }



protected:

  /**
   * Constructor
   */
  CommandIterationUpdate() {};

private:

  /**
   *  WeakPointer to the Optimizer
   */
  WeakPointer<OptimizerType>   m_Optimizer;
 

  
};


} // end namespace itk


#endif

