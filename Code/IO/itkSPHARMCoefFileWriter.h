/** \class SPHARMCoefFileReader
 *
 *  \brief This class writes out a list of spherical harmonic coefficients.
 *
 *  \author Christine Xu
 */

#ifndef __itkSPHARMCoefFileWriter_h
#define __itkSPHARMCoefFileWriter_h

#include "itkSPHARMCoefSpatialObject.h"

namespace itk
{

class SPHARMCoefFileWriterException : public ExceptionObject 
{
public:
  /** Run-time information. */
  itkTypeMacro( SPHARMCoefFileWriterException, ExceptionObject );
  
  /** Constructor. */
  SPHARMCoefFileWriterException(const char *file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }

  /** Constructor. */
  SPHARMCoefFileWriterException(const std::string &file, unsigned int line, 
                           const char* message = "Error in IO") : 
    ExceptionObject(file, line)
  {
    SetDescription(message);
  }
};

class SPHARMCoefFileWriter : public Object
{
public:
    /** SmartPointer typedef support */
  typedef SPHARMCoefFileWriter Self;
  typedef SmartPointer<Self> Pointer;
  
  typedef SPHARMCoefSpatialObject::ScalarType ScalarType;
  typedef SPHARMCoefSpatialObject::CoefType CoefType;
  typedef SPHARMCoefSpatialObject::CoefListType CoefListType;
  
  /** Method for creation through the object factory */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(SPHARMCoefFileWriter, Object);
  
  /** Write out a coef file. */
  void Update(void);
  
  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);
  
  void SetInput(CoefListType& coeflist){m_Coefs = coeflist;}
  
protected:
  std::string m_FileName;

  SPHARMCoefFileWriter();
  virtual ~SPHARMCoefFileWriter();
  
private:
  CoefListType m_Coefs;
  
};

} // end namespace itk

#endif
