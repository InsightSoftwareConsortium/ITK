/*=========================================================================

  Author: Christine Xu

=========================================================================*/

#include "itkSPHARMCoefFileWriter.h"

#include <stdio.h>

namespace itk
{

SPHARMCoefFileWriter::SPHARMCoefFileWriter()
{
  m_FileName = "";
}

SPHARMCoefFileWriter::~SPHARMCoefFileWriter()
{
  
}

void SPHARMCoefFileWriter::Update()
{
  
  if ( m_FileName == "" )
  {
    throw SPHARMCoefFileWriterException(__FILE__, __LINE__, "FileName must be specified");
  }
  
  FILE* file = fopen(m_FileName.c_str(), "w");
  
  if(file == NULL)
  {
    throw SPHARMCoefFileWriterException(__FILE__, __LINE__, "Coef file couldn't be created");
  }
  
  if(m_Coefs.empty())
  {
    fclose(file);
    return;
  }
  
  int count = m_Coefs.size();
  
  fprintf(file, "{ %d,", count);
  
  CoefListType::const_iterator iter = m_Coefs.begin();
  while( iter != m_Coefs.end())
  {
    CoefType elem = *iter;
    fprintf(file, "{%lf, %lf, %lf}", elem[0], elem[1], elem[2]);
    iter++;
    if(iter != m_Coefs.end())
      fprintf(file, ",\n");
  }
  
  fprintf(file, "}");
  
  fclose(file);
}

} // end namespace itk
