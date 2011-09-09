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

#ifndef __itkLevelSetImageBase_h
#define __itkLevelSetImageBase_h

#include "itkLevelSetBase.h"
#include "itkObjectFactory.h"
#include "itkIndex.h"
#include "itkImageBase.h"

namespace itk
{
/**
 *  \class LevelSetImageBase
 *  \brief Abstract class for a level-set function on one Image.
 *
 *  \tparam TOutput OutputType of the level-set function value
 *  \tparam VDimension Dimension of the underlying Image.
 *
 *  \ingroup ITKLevelSetsv4
 */
template< class TInput, unsigned int VDimension, typename TOutput >
class LevelSetImageBase :
  public LevelSetBase< TInput,
                       VDimension,
                       TOutput,
                       ImageBase< VDimension > >
{
public:
  typedef ImageBase< VDimension >         ImageBaseType;

  typedef LevelSetImageBase                       Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef LevelSetBase<
    TInput, VDimension, TOutput, ImageBaseType >  Superclass;

  /** Run-time type information */
  itkTypeMacro ( LevelSetImageBase, LevelSetBase );

  itkStaticConstMacro ( Dimension, unsigned int, Superclass::Dimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

protected:
  LevelSetImageBase();

  virtual ~LevelSetImageBase();

  typedef GradientType ScalingType;
  ScalingType m_NeighborhoodScales;

  virtual bool IsInside( const InputType& iP ) const = 0;

private:

  LevelSetImageBase( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented
  };
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetImageBase.hxx"
#endif

#endif // __itkLevelSetImageBase_h
