/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryThresholdSpatialFunction_h
#define __itkBinaryThresholdSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkImageBase.h"

namespace itk
{

/** \class BinaryThresholdSpatialFunction
 * \brief A spatial functions that returns if the internal spatial function
 * is within user specified thresholds.
 *
 * BinaryThresholdSpatialFunction is a wrapper class for an internal
 * spatial function and returns true if it is within user specified
 * thresholds and false otherwise.
 *
 * This class is templated over the internal spatial function type.
 *
 * \sa SpatialFunction
 * 
 * 
*/
template <typename TFunction>
class ITK_EXPORT BinaryThresholdSpatialFunction : 
  public SpatialFunction< bool, 
                          ::itk::GetImageDimension<TFunction>::ImageDimension, 
                          ITK_TYPENAME TFunction::InputType >
{
public:
  /** Standard class typedefs. */
  typedef BinaryThresholdSpatialFunction                       Self;
  typedef SpatialFunction< bool, 
                           ::itk::GetImageDimension<TFunction>::ImageDimension, 
                           ITK_TYPENAME TFunction::InputType > Superclass;
  typedef SmartPointer<Self>                                   Pointer;
  typedef SmartPointer<const Self>                             ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdSpatialFunction, SpatialFunction);

  /** New macro for creation of through the object factory.*/
  itkNewMacro( Self );

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputType typedef support. */
  typedef typename TFunction::InputType InputType;
  
  /** Underlying function type. */
  typedef TFunction FunctionType;

  /** Underlying function output type. */
  typedef typename TFunction::OutputType FunctionOutputType;

  /** Set/Get the lower threshold. */
  itkSetMacro( LowerThreshold, FunctionOutputType );
  itkGetConstReferenceMacro( LowerThreshold, FunctionOutputType );

  /** Set/Get the upper threshold. */
  itkSetMacro( UpperThreshold, FunctionOutputType );
  itkGetConstReferenceMacro( UpperThreshold, FunctionOutputType );

  /** Set/Get the underlying function. */
  itkSetObjectMacro( Function, FunctionType );
  itkGetConstObjectMacro( Function, FunctionType );

  /** Evaluate the function at a given position. */
  virtual OutputType Evaluate( const InputType& point ) const;

protected:

  BinaryThresholdSpatialFunction();
  ~BinaryThresholdSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

  FunctionOutputType                m_LowerThreshold;
  FunctionOutputType                m_UpperThreshold;
  typename FunctionType::Pointer    m_Function;

private:
  BinaryThresholdSpatialFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryThresholdSpatialFunction.txx"
#endif

#endif
