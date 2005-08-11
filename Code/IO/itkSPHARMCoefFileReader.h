/** \class SPHARMCoefFileReader
 *
 *  \brief This class reads a coefficient file and returns a list of spherical harmonic coefficients.
 *
 *  \author Christine Xu
 */

#ifndef __itkSPHARMCoefFileReader_h
#define __itkSPHARMCoefFileReader_h

#include "itkSPHARMCoefSpatialObject.h"

#include <string.h>

namespace itk
{

class SPHARMCoefFileReaderException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( ImageFileReaderException, ExceptionObject );
  
  /** Constructor. */
  SPHARMCoefFileReaderException(const char *file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }

  /** Constructor. */
  SPHARMCoefFileReaderException(const std::string &file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }
};

class SPHARMCoefFileReader : public Object
{
public:
  /** SmartPointer typedef support */
  typedef SPHARMCoefFileReader Self;
  typedef SmartPointer<Self> Pointer;
  
  typedef SPHARMCoefSpatialObject::ScalarType ScalarType;
  typedef SPHARMCoefSpatialObject::CoefType CoefType;
  typedef SPHARMCoefSpatialObject::CoefListType CoefListType;
  
  /** Method for creation through the object factory */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(SPHARMCoefFileReader, Object);
  
  /** Load a coef file. */
  void Update(void);
  
  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);
  
  void GetOutput(CoefListType& coeflist){coeflist = m_Coefs;}
  
protected:
  std::string m_FileName;

  SPHARMCoefFileReader();
  virtual ~SPHARMCoefFileReader();
  
private:
  CoefListType m_Coefs;

};

} // end namespace itk

#endif
