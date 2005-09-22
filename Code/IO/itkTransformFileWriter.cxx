/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileWriter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileWriter_cxx
#define __itkTransformFileWriter_cxx

#include "itkTransformFileWriter.h"

namespace itk
{

TransformFileWriter
::TransformFileWriter()
{
  m_FileName = "";
  this->m_AppendMode = false;
}

TransformFileWriter
::~TransformFileWriter()
{
}

/** Set the writer to append to the specified file */
void TransformFileWriter::SetAppendOn( )
{
  this->m_AppendMode = true;
}

/** Set the writer to overwrite the specified file - This is the default mode*/
void TransformFileWriter::SetAppendOff( )
{
  this->m_AppendMode = false;
}

/** Set the writer mode (append/overwrite). */
void TransformFileWriter::SetAppendMode( bool mode)
{
  this->m_AppendMode = mode;
}

/** Get the writer mode*/
bool TransformFileWriter::GetAppendMode( )
{
  return ( this->m_AppendMode );
}

/** Set the input transform and reinitialize the list of transforms */
void TransformFileWriter::SetInput(const TransformType* transform)
{
  m_TransformList.clear();
  m_TransformList.push_back(transform);
}

/** Add a transform to be written */
void TransformFileWriter::AddTransform(const TransformType* transform)
{
  m_TransformList.push_back(transform);
}



/** Update the writer */
void TransformFileWriter
::Update()
{  
  std::list<const TransformType *>::iterator it = m_TransformList.begin();
  vnl_vector<double> TempArray;
#ifdef __sgi
  // Create the file. This is required on some older sgi's
  if (this->m_AppendMode)
    {
    std::ofstream tFile(m_FileName.c_str(),std::ios::out | std::ios::app);
    tFile.close();   
    }
  else
    {
    std::ofstream tFile(m_FileName.c_str(),std::ios::out);
    tFile.close(); 
    }
#endif
  std::ofstream out;;
  if (this->m_AppendMode)
    {
    out.open(m_FileName.c_str(), std::ios::out | std::ios::app); 
    }
  else
    {
    out.open(m_FileName.c_str(), std::ios::out);
    }
  out << "#Insight Transform File V1.0" << std::endl;
  int count = 0;
  while(it != m_TransformList.end())
    {
    out << "# Transform " << count << std::endl;
    out << "Transform: " << (*it)->GetTransformTypeAsString() << std::endl;

    TempArray = (*it)->GetParameters();
    out << "Parameters: " << TempArray << std::endl;
    TempArray = (*it)->GetFixedParameters();
    out << "FixedParameters: " << TempArray << std::endl;
    it++;
    count++;
    }
  out.close();
}


} // namespace itk

#endif
