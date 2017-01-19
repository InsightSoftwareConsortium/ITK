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
#ifndef itkLevelSetTestFunction_h
#define itkLevelSetTestFunction_h

#include "itkLightObject.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class LevelSetTestFunction
 *
 * \brief A simple function to compare the numerical methods of the level set classes
 * to the analytical values.
 *
 * \f$ f(x,y) = \sqrt{ (x-5)(x-5) + (y-4)(y-4) } - 3 \f$
 */
template< typename TPixel >
class ITK_TEMPLATE_EXPORT LevelSetTestFunction: public LightObject
{
public:
  typedef LevelSetTestFunction       Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  itkTypeMacro( LevelSetTestFunction, LightObject );

  itkNewMacro( Self );

  itkStaticConstMacro(Dimension, unsigned int, 2);

  typedef TPixel                                        PixelType;

  typedef Image< PixelType, Dimension >                 ImageType;
  typedef typename ImageType::IndexType                 IndexType;
  typedef typename ImageType::PointType                 PointType;

  typedef typename NumericTraits< PixelType >::RealType OutputRealType;
  typedef CovariantVector< OutputRealType, Dimension >  GradientType;
  typedef Matrix< OutputRealType, Dimension >           HessianType;

  OutputRealType Evaluate( const PointType & point ) const;

  GradientType   EvaluateGradient( const PointType & point ) const;

protected:
  LevelSetTestFunction() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetTestFunction);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetTestFunction.hxx"
#endif

#endif
