/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOptimizer_h
#define __itkOptimizer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkExceptionObject.h"
#include "itkArray.h"


namespace itk
{
  
/** \class Optimizer
 * \brief Generic representation for an optimization method.
 *
 *  This class is a base for a hierarchy of optimizers. 
 *  It is not intended to be instantiated.
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT Optimizer : public Object 
{
public:
  /** Standard class typedefs. */
  typedef Optimizer                 Self;
  typedef Object                    Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(Optimizer, Object);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef Array<double>             ParametersType;

  /**  Scale type.
   *  This array defines scale to be applied to parameters before 
   *  being evaluated in the cost function. This allows to 
   *  map to a more convenient space. In particular this is
   *  used to normalize parameter spaces in which some parameters
   *  have a different dynamic range.   */
  typedef Array<double>             ScalesType;

  /**  Set the position to initialize the optimization. */
  virtual void SetInitialPosition( const ParametersType & param );

  /** Get the position to initialize the optimization. */
  itkGetConstMacro(InitialPosition, ParametersType);

  /** Set current parameters scaling. */
  void SetScales(const ScalesType & scales);

  /** Get current parameters scaling. */
  itkGetConstReferenceMacro( Scales, ScalesType );

  /** Get current position of the optimization. */
  itkGetConstReferenceMacro( CurrentPosition, ParametersType );

  /** Start optimization. */
  virtual void StartOptimization() {};

protected:
  Optimizer();
  virtual ~Optimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Set the current position. */
  virtual void SetCurrentPosition( const ParametersType & param );

  bool                    m_ScalesInitialized;

private:
  Optimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ParametersType          m_InitialPosition;
  ParametersType          m_CurrentPosition;
  ScalesType              m_Scales;

};

} // end namespace itk


#endif



