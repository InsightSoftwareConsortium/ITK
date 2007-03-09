/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPowellOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPowellOptimizer_cxx
#define _itkPowellOptimizer_cxx

#include "itkPowellOptimizer.h"

namespace itk
{

const double POWELL_BRACKET_GOLD = 1.618034;
const double POWELL_BRENT_GOLD = 0.3819660;
const double POWELL_GLIMIT = 100.0;
const double POWELL_TINY = 1.0e-20;

PowellOptimizer
::PowellOptimizer()
{
  m_Maximize = false;

  m_StepLength = 1.0; 
  m_StepTolerance = 0.00001; 
  m_ValueTolerance = 0.00001;

  m_Stop = false ;

  m_CurrentCost = 0;
  m_CurrentIteration = 0;
  m_CurrentLineIteration = 0;

  m_MaximumIteration = 100 ;

  m_MaximumLineIteration = 100 ;
  m_SpaceDimension = 0;
}

PowellOptimizer
::~PowellOptimizer() 
{
}

void
PowellOptimizer
::SetLine(const PowellOptimizer::ParametersType & origin,
          const vnl_vector<double> & direction)
{
  m_LineOrigin = origin;
  m_LineDirection = direction;
  for(unsigned int i=0; i<m_SpaceDimension; i++)
    {
    m_LineDirection[i] = m_LineDirection[i] / this->GetScales()[i];
    }
}

double
PowellOptimizer
::GetLineValue(double x) const
{
  PowellOptimizer::ParametersType xCoord( m_SpaceDimension );
  for(unsigned int i=0; i<m_SpaceDimension; i++)
    {
    xCoord[i] = this->m_LineOrigin[i] + x * this->m_LineDirection[i];
    }
  if(m_Maximize)
    {
    return -(this->m_CostFunction->GetValue(xCoord));
    }
  else
    {
    return this->m_CostFunction->GetValue(xCoord);
    }
}

void
PowellOptimizer
::SetCurrentLinePoint(double x, double fx) 
{
  PowellOptimizer::ParametersType xCoord( m_SpaceDimension );
  for(unsigned int i=0; i<m_SpaceDimension; i++)
    {
    xCoord[i] = this->m_LineOrigin[i] + x * this->m_LineDirection[i];
    }
  this->SetCurrentPosition(xCoord);
  if(m_Maximize)
    {
    this->SetCurrentCost(-fx);
    }
  else
    {
    this->SetCurrentCost(fx);
    }
}

void
PowellOptimizer
::Swap(double * a, double  * b) const
{
  double tf;
  tf = *a;
  *a = *b;
  *b = tf;
}

void
PowellOptimizer
::Shift(double *a, double *b, double *c, double d) const
{
  *a = *b;
  *b = *c;
  *c = d;
}

void
PowellOptimizer
::LineBracket(double * ax, double * bx, double * cx,
              double * fa, double * fb, double * fc) 
{
  double ulim,u,r,q,fu,dq;

  //*fa = this->GetLineValue(*ax);
  *fb = this->GetLineValue(*bx);
  if (*fb > *fa) 
    {
    this->Swap(ax, bx);
    this->Swap(fa, fb);
    }

  *cx = (*bx) + POWELL_BRACKET_GOLD * (*bx - *ax);
  *fc = this->GetLineValue(*cx);
  while (*fb > *fc) 
    {
    r = (*bx - *ax) * (*fb - *fc);
    q = (*bx - *cx) * (*fb - *fa);

    dq = q - r;
    if (vcl_abs(dq) < POWELL_TINY)
      {
      dq = vnl_math_sgn(dq) * POWELL_TINY;
      }
    u = (*bx) - ( (*bx - *cx) * q - (*bx - *ax) * r ) / (2.0 * dq);
    ulim = (*bx) + POWELL_GLIMIT * (*cx - *bx);

    if ( (*bx - u) * (u - *cx) > 0.0 ) 
      {
      fu = this->GetLineValue(u);
      if (fu < *fc) 
        {
        *ax = (*bx);
        *bx = u;
        *fa = (*fb);
        *fb = fu;
        return; 
        } 
      else if (fu > *fb) 
        {
        *cx=u;
        *fc=fu;
        return;
        }
      u = (*cx) + POWELL_BRACKET_GOLD * (*cx - *bx);
      fu = this->GetLineValue(u);
      } 
    else if ( (*cx - u) * (u - ulim) > 0.0 ) 
      {
      fu = this->GetLineValue(u);
      if (fu < *fc) 
        {
        //SHFT(bx,cx,&u,*cx+GOLD*(*cx-*bx)); awf dumped -- c is useless
        this->Shift(bx, cx, &u, u + POWELL_BRACKET_GOLD * (u - *cx));
        this->Shift(fb, fc, &fu, this->GetLineValue(u));
        }
      } 
    else if ( (u - ulim) * (ulim - *cx) >= 0.0) 
      {
      u = ulim;
      fu = this->GetLineValue(u); 
      } 
    else 
      {
      u = (*cx) + POWELL_BRACKET_GOLD * (*cx - *bx);
      fu = this->GetLineValue(u);
      }

    this->Shift(ax, bx, cx, u);
    this->Shift(fa, fb, fc, fu);
    }

  this->SetCurrentLinePoint(*bx, *fb);

}

void
PowellOptimizer
::BracketedLineOptimize(double ax, double bx, double cx,
                        double itkNotUsed(fa), double functionValueOfb, double itkNotUsed(fc),
                        double * extX, double * extVal) 
{
  double x;
  double v;
  double w;        /* Abscissae, descr. see above  */
  double a;
  double b;

  a = (ax < cx ? ax : cx);
  b = (ax > cx ? ax : cx);

  x = bx;
  w = bx;

  const double goldenSectionRatio = (3.0-sqrt(5.0))/2;  /* Gold section ratio    */

  double functionValueOfX;        /* f(x)        */
  double functionValueOfV;        /* f(v)        */
  double functionValueOfW;        /* f(w)        */

  functionValueOfV   =    functionValueOfb;
  functionValueOfX   =    functionValueOfV;
  functionValueOfW   =    functionValueOfV;

  for (m_CurrentLineIteration = 0;
       m_CurrentLineIteration < m_MaximumLineIteration;
       m_CurrentLineIteration++)
    {
  
    double range = b-a;      /* Range over which the minimum */
    double middle_range = (a+b)/2;

    double new_step;          /* Step at this iteration       */

    double tolerance1;
    double tolerance2;

    tolerance1 = m_StepTolerance * vcl_fabs(x) + POWELL_TINY;
    tolerance2 = 2.0 * tolerance1;

    if (vcl_fabs(x-middle_range) <= (tolerance2 - 0.5*(b-a))
        || 0.5*(b-a) < m_StepTolerance)
      {
      *extX = x; 
      *extVal = functionValueOfX;
      this->SetCurrentLinePoint(x, functionValueOfX);
      return ;        /* Acceptable approx. is found  */
      }

    /* Obtain the gold section step  */
    new_step = goldenSectionRatio * ( x<middle_range ? b-x : a-x );


    /* Decide if the interpolation can be tried  */
    if( fabs(x-w) >= tolerance1  )    /* If x and w are distinct      */
      {    
      double t;
      t = (x-w) * (functionValueOfX-functionValueOfV);

      double q;    /* ted as p/q; division operation*/
      q = (x-v) * (functionValueOfX-functionValueOfW);

       double p;     /* Interpolation step is calcula-*/
      p = (x-v)*q - (x-w)*t;

      q = 2*(q-t);

      if( q>(double)0 )    /* q was calculated with the op-*/
        {
        p = -p;      /* posite sign; make q positive  */
        }
      else        /* and assign possible minus to  */
        {
        q = -q;      /* p        */
        }

      /* Chec if x+p/q falls in [a,b] and  not too close to a and b
           and isn't too large */
      if( fabs(p) < fabs(new_step*q) &&  
          p > q*(a-x+2*tolerance1) &&    
          p < q*(b-x-2*tolerance1)  )            
          {
          new_step = p/q;      /* it is accepted         */
          }

      /* If p/q is too large then the  gold section procedure can   
         reduce [a,b] range to more  extent      */
      }

     /* Adjust the step to be not less than tolerance*/
    if( fabs(new_step) < tolerance1 ) 
      {
      if ( new_step > 0.0 )  
        {
        new_step = tolerance1;
        }
      else
        {
        new_step = -tolerance1;
        }
      }

    /* Obtain the next approximation to min  */
    /* and reduce the enveloping range  */
    double t = x + new_step;  /* Tentative point for the min  */

    double functionValueOft;

    functionValueOft = this->GetLineValue(t);

    if( functionValueOft <= functionValueOfX )
    {                     
      if( t < x )      /* Reduce the range so that  */
        {
        b = x;        /* t would fall within it  */
        }
      else
        {
        a = x;
        }
   
     /* assing the best approximation to x */ 
    v = w;
    w = x;
    x = t;    

    functionValueOfV = functionValueOfW;
    functionValueOfW = functionValueOfX;
    functionValueOfX = functionValueOft;
    }
    else                              /* x remains the better approx  */
    {                         
    if( t < x )      /* Reduce the range enclosing x  */
      {
      a = t;                   
      }
    else
      {
      b = t;
      }
    
      if( functionValueOft <= functionValueOfW || w==x )
        {
        v = w;
        w = t;
        functionValueOfV = functionValueOfW;
        functionValueOfW = functionValueOft;
        }
      else if( functionValueOft<=functionValueOfV || v==x || v==w )
        {
        v = t;
        functionValueOfV=functionValueOft;
        }
     }       
  }    

  *extX = x; 
  *extVal = functionValueOfX;

  this->SetCurrentLinePoint(x, functionValueOfX);

}

void
PowellOptimizer
::StartOptimization()
{
  if( m_CostFunction.IsNull() )
    {
    return ;
    }

  this->InvokeEvent( StartEvent() );
  m_Stop = false ;

  m_SpaceDimension = m_CostFunction->GetNumberOfParameters();
  m_LineOrigin.set_size(m_SpaceDimension);
  m_LineDirection.set_size(m_SpaceDimension);

  vnl_matrix<double> xi(m_SpaceDimension, m_SpaceDimension);
  vnl_vector<double> xit(m_SpaceDimension);
  xi.set_identity();
  xit.fill(0);
  xit[0] = 1;

  PowellOptimizer::ParametersType p(m_SpaceDimension);
  PowellOptimizer::ParametersType pt(m_SpaceDimension);
  PowellOptimizer::ParametersType ptt(m_SpaceDimension);
  p = this->GetInitialPosition();
  pt = p;

  unsigned int ibig;
  double fp, del, fptt;
  double ax, xx, bx;
  double fa, fx, fb;

  xx = 0;
  this->SetLine(p, xit);
  fx = this->GetLineValue(0);

  for (m_CurrentIteration = 0;
       m_CurrentIteration <= m_MaximumIteration;
       m_CurrentIteration++)
    {
    fp = fx;
    ibig = 0;
    del = 0.0;

    for (unsigned int i = 0; i < m_SpaceDimension; i++)
      {
      for (unsigned int j = 0; j < m_SpaceDimension; ++j)
        {
        xit[j] = xi[j][i];
        }
      fptt = fx;

      this->SetLine(p, xit);

      ax = 0.0;
      fa = fx;
      xx = m_StepLength;
      this->LineBracket(&ax, &xx, &bx, &fa, &fx, &fb);
      this->BracketedLineOptimize(ax, xx, bx, fa, fx, fb, &xx, &fx);
      this->SetCurrentLinePoint(xx, fx);
      p = this->GetCurrentPosition();

      if (vcl_fabs(fptt-fx) > del) 
        {
        del = vcl_fabs(fptt-fx);
        ibig = i;
        }
      }

    if (2.0*vcl_fabs(fp-fx) 
        <= m_ValueTolerance*(vcl_fabs(fp)+vcl_fabs(fx)))
      {
      this->InvokeEvent( EndEvent() );
      return;
      }

    for (unsigned int j = 0; j < m_SpaceDimension; ++j)
      {
      ptt[j] = 2.0*p[j] - pt[j];
      xit[j] = (p[j] - pt[j]) * this->GetScales()[j];
      pt[j] = p[j];
      }

    this->SetLine(ptt, xit);
    fptt = this->GetLineValue(0);
    if (fptt < fp)
      {
      double t = 2.0 * (fp - 2.0*fx + fptt)
                     * vnl_math_sqr(fp-fx-del)
                     - del * vnl_math_sqr(fp-fptt);
      if (t < 0.0)
        {
        this->SetLine(p, xit);

        ax = 0.0;
        fa = fx;
        xx = 1;
        this->LineBracket(&ax, &xx, &bx, &fa, &fx, &fb);
        this->BracketedLineOptimize(ax, xx, bx, fa, fx, fb, &xx, &fx);
        this->SetCurrentLinePoint(xx, fx);
        p = this->GetCurrentPosition();

        for (unsigned int j = 0; j < m_SpaceDimension; j++) 
          {
          xi[j][ibig] = xx * xit[j];
          }
        }
      }

    this->InvokeEvent( IterationEvent() );

    }

  this->InvokeEvent( EndEvent() );

}

/**
 *
 */
void
PowellOptimizer
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Space Dimension   " << m_SpaceDimension   << std::endl;
  os << indent << "Maximum Iteration " << m_MaximumIteration << std::endl;
  os << indent << "Current Iteration " << m_CurrentIteration << std::endl;
  os << indent << "Maximize On/Off   " << m_Maximize         << std::endl;
  os << indent << "StepLength        " << m_StepLength       << std::endl;
  os << indent << "StepTolerance     " << m_StepTolerance    << std::endl;
  os << indent << "ValueTolerance    " << m_ValueTolerance   << std::endl;
  os << indent << "LineOrigin        " << m_LineOrigin       << std::endl;
  os << indent << "LineDirection     " << m_LineDirection    << std::endl;
  os << indent << "Current Cost      " << m_CurrentCost      << std::endl;
  os << indent << "Maximum Line Iteration " << m_MaximumLineIteration << std::endl;
  os << indent << "Current Line Iteration " << m_CurrentLineIteration << std::endl;
  os << indent << "Stop              " << m_Stop             << std::endl;
}

} // end of namespace itk
#endif
