/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCommandVnlIterationUpdate.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
class ITK_EXPORT CommandVnlIterationUpdate : public Command 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CommandVnlIterationUpdate   Self;


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
  void Execute(itk::Object *caller, const itk::EventObject & event)
  {
    Execute( (const itk::Object *)caller, event);
  }

  void Execute(const itk::Object * itkNotUsed(caller), const itk::EventObject & event)
  {
    if( typeid( event ) == typeid( itk::StartEvent ) )
      {
      std::cout << std::endl << "Position              Value";
      std::cout << std::endl << std::endl;
      }    
    else if( typeid( event ) == typeid( itk::IterationEvent ) )
      {
      std::cout << m_Optimizer->GetCurrentIteration() << " = ";
      std::cout << m_Optimizer->GetCurrentPosition() << std::endl;
      }
    else if( typeid( event ) == typeid( itk::EndEvent ) )
      {
      std::cout << std::endl << std::endl;
      std::cout << "After " << m_Optimizer->GetCurrentIteration();
      std::cout << "  iterations " << std::endl;
      std::cout << "Solution is    = " << m_Optimizer->GetCurrentPosition();
      std::cout << std::endl;
      std::cout << "vnl report = " << std::endl;
      m_Optimizer->GetOptimizer()->diagnose_outcome( std::cout );
      }

  }


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro( CommandVnlIterationUpdate, ::itk::Command );


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
    { 
    m_Optimizer = optimizer;
    m_Optimizer->AddObserver( itk::IterationEvent(), this );
    }



protected:

  /**
   * Constructor
   */
  CommandVnlIterationUpdate() {};

private:

  /**
   *  WeakPointer to the Optimizer
   */
  WeakPointer<OptimizerType>   m_Optimizer;
 

  
};


} // end namespace itk


#endif

