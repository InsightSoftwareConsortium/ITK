/*=========================================================================

  Author: Christine Xu

=========================================================================*/
#ifndef __itkSphericalHarmonicPolynomial_txx
#define __itkSphericalHarmonicPolynomial_txx

#include "itkSphericalHarmonicPolynomial.h"

#include <math.h>
#include <iostream>

#ifndef M_PI
#define M_PI 3.1415926535897932
#endif
#ifndef M_PI_2
#define M_PI_2 1.5707963267948966
#endif

namespace itk 
{

template< unsigned int TDimension >
SphericalHarmonicPolynomial< TDimension >::SphericalHarmonicPolynomial()
{
  //m_Dimension = 2;
}

template< unsigned int TDimension >
SphericalHarmonicPolynomial< TDimension >::~SphericalHarmonicPolynomial()
{
  
}

template< unsigned int TDimension >
void SphericalHarmonicPolynomial< TDimension >::SetCoefs(SphericalHarmonicPolynomial< TDimension >::CoefListType& coeflist)
{
  if(coeflist.empty())
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The input coefficient list is empty!");
  unsigned int d = coeflist[0].GetPointDimension();
  if(!(TDimension == d))
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The dimension of the input coefficient list does not match the specified dimension!");
  m_Coefs = coeflist;
}

template< unsigned int TDimension >
void SphericalHarmonicPolynomial< TDimension >::Evaluate(unsigned int from_l,
                                    unsigned int to_l, double phi, double theta,
                                    double* sum)
{
  //if(m_Dimension != 2)
  //  throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "Only Spherical Harmonics of dimension two is currently implemented.");
  //if(size > TDimension)
  //   throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "Dimension of the output sum exceeds the dimension of the coefficients.");
  if(m_Coefs.empty())
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "Coefficients must be specified.");
  if(from_l > to_l)
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The starting degree should be smaller or equal to the ending degree.");
  if(to_l > m_Degree)
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The evalueated degree mustn't exceed the size of the coefficients.");
  if(m_Coefs.size() < (to_l+1)*(to_l+1))
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "Coefficients size mismatch.");
  
  unsigned int l, m, coord;
  
  for (coord= 0; coord < TDimension; coord++) 
  {
     sum[coord]= 0.0;
     
     for (l= from_l; l <= to_l; l++) //for the from_l degree to to_l degree
     {
         double* plm = new double[l+1];
         for (m= 0; m <= l; m++)
         {
           double coef = sqrt((2*l+1.0)/4.0/M_PI / fac_quot(l+m, l-m));
           double row = plgndr_row(l, m, cos(theta));
           plm[m] = coef * row;
         }
         
         sum[coord]+= m_Coefs[l*l][coord] * plm[0];
         
        for (m= 1; m <= l; m++)
        {
          double sin_t = sin(m*phi);
          double cos_t = cos(m*phi);
          sum[coord]+= ( cos_t * m_Coefs[l*l+2*m-1][coord]//real part
            +sin_t * m_Coefs[l*l+2*m ][coord] ) //imaginary part
            * plm[m];
        }
        delete []plm;
     }
  }
}


/*
output:
p_ptr:  the value of specified list of Associated Legendre polynomials (vertically, columnly)

input:
m:      the order of the polynomials
l:      the maximal degree of the polynomials
x:      the input parameter of the Associated Legendre polynomials

Legendre Polynomials come from the Sturm-Liouville Boundary Value Problem:
(1-x^2)y'' - 2xy' + n(n+1)y = 0

The Legendre Polynomials have the recurrence relation of:
(n+1)P_n+1(x) = (2n+1)xP_n(x) - n P_n-1(x)

The associate Legendre Polynomials are defined using Legendre Polynomials,
and they also obey the following recurrence relations
(l-m)P^m_l(x) = x(2l-1)P^m_l-1(x) - (l+m-1)P^m_l-2(x)

More about Legendre Polynomials:
http://mathworld.wolfram.com/LegendrePolynomial.html
*/
template< unsigned int TDimension >
double SphericalHarmonicPolynomial< TDimension >::plgndr_row(int l, int m, double x)
{
   double fact,pll,pmm,pmmp1,somx2;
   int i,ll;
   
   if (m < 0 || m > l || fabs(x) > 1.0)
   {
      std::cout<<"Bad arguments in routine PLGNDR"<<std::endl;
      exit(1);
   }
   pmm=1.0;
   if (m > 0) 
   {
      somx2=sqrt((1.0-x)*(1.0+x));
      fact=1.0;
      for (i=1;i<=m;i++) 
      {
         pmm *= -fact*somx2;
         fact += 2.0;
      }
   }
   //*p_ptr++= pmm;
   if (l == m)
      return pmm;
   else 
   {
      pmmp1=x*(2*m+1)*pmm;
      //*p_ptr++= pmmp1;
      if (l == (m+1))
         return pmmp1;
      else 
      {
         for (ll=(m+2);ll<=l;ll++) 
         {
          pll=(x*(2*ll-1)*pmmp1-(ll+m-1)*pmm)/(ll-m);
          //*p_ptr++= pll;
          pmm=pmmp1;
          pmmp1=pll;
         }
         return pll;
       }
   }
}

/* factorial quotient a!/b!*/
template< unsigned int TDimension >
double SphericalHarmonicPolynomial< TDimension >::fac_quot(int a, int b)
{
   double res= 1;
   while (a > b) res*= a--;
   return res;
}

} // end namespace itk

#endif
