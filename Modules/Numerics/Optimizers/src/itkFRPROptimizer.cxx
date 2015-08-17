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
#ifndef _itkFRPROptimizer_cxx
#define _itkFRPROptimizer_cxx

#include "itkFRPROptimizer.h"

namespace itk
{
const double FRPR_TINY = 1e-20;

FRPROptimizer
::FRPROptimizer()
{
  m_UseUnitLengthGradient = false;
  m_OptimizationType = PolakRibiere;
}

FRPROptimizer
::~FRPROptimizer()
{}

void
FRPROptimizer
::GetValueAndDerivative(ParametersType & p, double *val,
                        ParametersType *xi)
{
  this->m_CostFunction->GetValueAndDerivative(p, *val, *xi);
  if ( this->GetMaximize() )
    {
    ( *val ) = -( *val );
    for ( unsigned int i = 0; i < this->GetSpaceDimension(); i++ )
      {
      ( *xi )[i] = -( *xi )[i];
      }
    }
  if ( this->GetUseUnitLengthGradient() )
    {
    double len = ( *xi )[0] * ( *xi )[0];
    for ( unsigned int i = 1; i < this->GetSpaceDimension(); i++ )
      {
      len += ( *xi )[i] * ( *xi )[i];
      }
    len = std::sqrt( len / this->GetSpaceDimension() );
    for ( unsigned int i = 0; i < this->GetSpaceDimension(); i++ )
      {
      ( *xi )[i] /= len;
      }
    }
}

void
FRPROptimizer
::LineOptimize(ParametersType *p, ParametersType & xi, double *val)
{
  ParametersType tempCoord( this->GetSpaceDimension() );

  this->LineOptimize(p, xi, val, tempCoord);
}

void
FRPROptimizer
::LineOptimize(ParametersType *p, ParametersType & xi, double *val,
               ParametersType & tempCoord)
{
  this->SetLine(*p, xi);

  double ax = 0.0;
  double fa = ( *val );
  double xx = this->GetStepLength();
  double fx;
  double bx;
  double fb;

  this->LineBracket(&ax, &xx, &bx, &fa, &fx, &fb, tempCoord);
  this->SetCurrentLinePoint(xx, fx);

  double extX = 0;
  double extVal = 0;

  this->BracketedLineOptimize(ax, xx, bx, fa, fx, fb, &extX, &extVal,
                              tempCoord);
  this->SetCurrentLinePoint(extX, extVal);

  ( *p ) = this->GetCurrentPosition();
  ( *val ) = extVal;
}

void
FRPROptimizer
::StartOptimization()
{
  unsigned int i;

  if ( m_CostFunction.IsNull() )
    {
    return;
    }

  this->InvokeEvent( StartEvent() );
  this->SetStop(false);

  this->SetSpaceDimension( m_CostFunction->GetNumberOfParameters() );

  FRPROptimizer::ParametersType tempCoord( this->GetSpaceDimension() );

  double                        gg, gam, dgg;
  FRPROptimizer::ParametersType g( this->GetSpaceDimension() );
  FRPROptimizer::ParametersType h( this->GetSpaceDimension() );
  FRPROptimizer::ParametersType xi( this->GetSpaceDimension() );

  FRPROptimizer::ParametersType p( this->GetSpaceDimension() );
  p = this->GetInitialPosition();
  this->SetCurrentPosition(p);

  double fp;
  this->GetValueAndDerivative(p, &fp, &xi);

  for ( i = 0; i < this->GetSpaceDimension(); i++ )
    {
    g[i] = -xi[i];
    xi[i] = g[i];
    h[i] = g[i];
    }

  unsigned int limitCount = 0;

  for ( unsigned int currentIteration = 0;
        currentIteration <= this->GetMaximumIteration();
        currentIteration++ )
    {
    this->SetCurrentIteration(currentIteration);

    double fret;
    fret = fp;
    this->LineOptimize(&p, xi, &fret, tempCoord);

    if ( 2.0 * std::abs(fret - fp) <=
         this->GetValueTolerance() * ( std::abs(fret) + std::abs(fp) + FRPR_TINY ) )
      {
      if ( limitCount <  this->GetSpaceDimension() )
        {
        this->GetValueAndDerivative(p, &fp, &xi);
        xi[limitCount] = 1;
        limitCount++;
        }
      else
        {
        this->SetCurrentPosition(p);
        this->InvokeEvent( EndEvent() );
        return;
        }
      }
    else
      {
      limitCount = 0;
      this->GetValueAndDerivative(p, &fp, &xi);
      }

    gg = 0.0;
    dgg = 0.0;

    if ( m_OptimizationType == PolakRibiere )
      {
      for ( i = 0; i < this->GetSpaceDimension(); i++ )
        {
        gg += g[i] * g[i];
        dgg += ( xi[i] + g[i] ) * xi[i];
        }
      }
    if ( m_OptimizationType == FletchReeves )
      {
      for ( i = 0; i < this->GetSpaceDimension(); i++ )
        {
        gg += g[i] * g[i];
        dgg += xi[i] * xi[i];
        }
      }

    if ( gg == 0.0 )
      {
      this->SetCurrentPosition(p);
      this->InvokeEvent( EndEvent() );
      return;
      }

    gam = dgg / gg;
    for ( i = 0; i < this->GetSpaceDimension(); i++ )
      {
      g[i] = -xi[i];
      xi[i] = g[i] + gam * h[i];
      h[i] = xi[i];
      }

    this->SetCurrentPosition(p);
    this->InvokeEvent( IterationEvent() );
    }

  this->InvokeEvent( EndEvent() );
}

/**
 *
 */
void
FRPROptimizer
::SetToPolakRibiere()
{
  m_OptimizationType = PolakRibiere;
}

/**
 *
 */
void
FRPROptimizer
::SetToFletchReeves()
{
  m_OptimizationType = FletchReeves;
}

/**
 *
 */
void
FRPROptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Optimization Type = " << m_OptimizationType << std::endl;
  os << indent << "0=FletchReeves, 1=PolakRibiere" << std::endl;
  os << indent << "Use unit length gradient = " << m_UseUnitLengthGradient << std::endl;
}
} // end of namespace itk
#endif
