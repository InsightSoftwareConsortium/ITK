/** \class SphericalHarmonicPolynomial
 *
 *  \brief This class provides 2D(phi, theta) Spherical Harmonics basis functions.
 *
 *  \author Christine Xu
 */

#ifndef __itkSphericalHarmonicPolynomial_h
#define __itkSphericalHarmonicPolynomial_h

#include <vector>

#include <itkExceptionObject.h>
#include <itkPoint.h>

namespace itk
{

class SphericalHarmonicPolynomialException : public ExceptionObject 
{
public:

  /** Run-time information. */
  itkTypeMacro( SphericalHarmonicPolynomialException, ExceptionObject );
  

  /** Constructor. */
  SphericalHarmonicPolynomialException(const char *file, unsigned int line, 
                           const char* message = "Error in evaluate Spherical Harmonic polynomials") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }

  /** Constructor. */
  SphericalHarmonicPolynomialException(const std::string &file, unsigned int line, 
                           const char* message = "Error in evaluate Spherical Harmonic polynomials") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }

};



template < unsigned int TDimension = 3 >
class SphericalHarmonicPolynomial
{

public:
  typedef SphericalHarmonicPolynomial Self;

  typedef double ScalarType;
  typedef Point<ScalarType, TDimension> CoefType;
  typedef std::vector<CoefType> CoefListType;

 

  /*typedef SPHARMCoefSpatialObject::ScalarType ScalarType;
  typedef SPHARMCoefSpatialObject::CoefType CoefType;
  typedef SPHARMCoefSpatialObject::CoefListType CoefListType;*/


  /*unsigned int GetDimension(void) const
  {return m_Dimension;}
  void SetDimension(unsigned int d)
  {m_Dimension = d;}*/

  

  unsigned int GetDegree(void) const
  {return m_Degree;}

  void SetDegree(unsigned int d)
  {m_Degree = d;}

  void SetCoefs(CoefListType& coeflist);

  void Evaluate(unsigned int from_l, unsigned int to_l, double phi, double theta, double* sum);

  SphericalHarmonicPolynomial();

  ~SphericalHarmonicPolynomial();  

protected:

  double plgndr_row(int l, int m, double x);
  double fac_quot(int a, int b);


private:

  //unsigned int m_Dimension;

  unsigned int m_Degree;//m_Degree >= 0

  CoefListType m_Coefs;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkSphericalHarmonicPolynomial.txx" 
#endif 

#endif//__itkSphericalHarmonicPolynomial_h

