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
#include "itkBSplineDeformableTransform.h"
#include "itkRigid2DTransform.h"

namespace itk
{

TransformFileWriter
::TransformFileWriter()
{
  m_FileName = "";
}

TransformFileWriter
::~TransformFileWriter()
{
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
  std::ofstream out;;
  out.open(m_FileName.c_str());
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
