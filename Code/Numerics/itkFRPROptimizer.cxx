/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFRPROptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFRPROptimizer_cxx
#define _itkFRPROptimizer_cxx

#include "itkFRPROptimizer.h"


namespace itk
{

const double FRPR_TINY = 1e-20;

FRPROptimizer
::FRPROptimizer()
{
  m_OptimizationType = PolakRibiere;
}

FRPROptimizer
::~FRPROptimizer() 
{
}

void
FRPROptimizer
::GetValueAndDerivative(ParametersType p, double * val,
                        ParametersType * xi)
{
  this->m_CostFunction->GetValueAndDerivative(p, *val, *xi);
  if(this->GetMaximize())
    {
    (*val) = -(*val);
    for(unsigned int i=0; i<this->GetSpaceDimension(); i++)
      {
      (*xi)[i] = -(*xi)[i];
      }
    }
}

void
FRPROptimizer
::LineOptimize(ParametersType * p, ParametersType xi, double * val)
{
  this->SetLine(*p, xi);

  double ax = 0.0;
  double fa = (*val);
  double xx = this->GetStepLength();
  double fx;
  double bx;
  double fb;

  ParametersType pp = (*p);

  this->LineBracket(&ax, &xx, &bx, &fa, &fx, &fb);
  this->SetCurrentLinePoint(xx, fx);

  double extX = 0;
  double extVal = 0;

  this->BracketedLineOptimize(ax, xx, bx, fa, fx, fb, &extX, &extVal);
  this->SetCurrentLinePoint(extX, extVal);

  (*p) = this->GetCurrentPosition();
  (*val) = extVal;
}

void
FRPROptimizer
::StartOptimization()
{
  unsigned int i;

  if( m_CostFunction.IsNull() )
    {
    return ;
    }

  this->InvokeEvent( StartEvent() );
  this->SetStop(false);

  this->SetSpaceDimension(m_CostFunction->GetNumberOfParameters());

  const unsigned int SpaceDimension = this->GetSpaceDimension();

  double gg, gam, dgg;
  FRPROptimizer::ParametersType g( SpaceDimension );
  FRPROptimizer::ParametersType h( SpaceDimension );
  FRPROptimizer::ParametersType xi( SpaceDimension );

  FRPROptimizer::ParametersType p( SpaceDimension );
  p = this->GetInitialPosition();

  double fp;
  this->GetValueAndDerivative(p, &fp, &xi);

  for( i = 0; i < SpaceDimension ; i++ )
    {
    g[i] = -xi[i];
    xi[i] = g[i];
    h[i] = g[i];
    }

  unsigned int limitCount = 0;

  for (unsigned int currentIteration = 0;
       currentIteration <= this->GetMaximumIteration();
       currentIteration++)
    {
    this->SetCurrentIteration(currentIteration);

    double fret;
    fret = fp;
    this->LineOptimize(&p, xi, &fret);

    if ( 2.0 * vcl_abs(fret - fp) <= 
      this->GetValueTolerance() * (vcl_abs(fret)+ vcl_abs(fp) + FRPR_TINY) )
    {
      if( limitCount <  SpaceDimension )
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
    
    if( m_OptimizationType == PolakRibiere )
      {
      for( i=0; i< SpaceDimension ; i++ )
        {
        gg += g[i] * g[i];
        dgg += (xi[i] + g[i]) * xi[i];    
        }
      } 
    if( m_OptimizationType == FletchReeves )
      {
      for( i=0; i< SpaceDimension ; i++ )
        {
        gg += g[i] * g[i];
        dgg += xi[i] * xi[i];  
        }
      }

    if(gg == 0)
      {
      this->SetCurrentPosition(p);
      this->InvokeEvent( EndEvent() );
      return;
      }

    gam = dgg/gg;
    for( i = 0; i < SpaceDimension ; i++)
      {
      g[i] = -xi[i];
      xi[i] = g[i] + gam * h[i];
      h[i] = xi[i];
      }

    this->SetCurrentPosition(p);
    this->InvokeEvent( IterationEvent() );
    }

  this->SetCurrentPosition(p);
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
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Optimization Type = " << m_OptimizationType << std::endl;
  os << indent << "0=FletchReeves, 1=PolakRibiere" << std::endl;
}

} // end of namespace itk
#endif
