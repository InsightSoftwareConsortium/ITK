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
#include "itkTransformFileReader.h"
#include "itkTransformIOFactory.h"
#include "itkCompositeTransformIOHelper.h"

namespace itk
{
/** Constructor */
TransformFileReader
::TransformFileReader()
{
  m_FileName = "";
  /* to be removed soon. See .h */
}

/** Destructor */
TransformFileReader
::~TransformFileReader()
{}

void TransformFileReader
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }
  TransformIOBase::Pointer transformIO =
    TransformIOFactory::CreateTransformIO(m_FileName.c_str(),
                                          TransformIOFactory::ReadMode);
  if ( transformIO.IsNull() )
    {
    itkExceptionMacro("Can't Create IO object for file "
                      << m_FileName);
    }

  transformIO->SetFileName(m_FileName);
  transformIO->Read();

  TransformIOBase::TransformListType &ioTransformList =
    transformIO->GetTransformList();

  for ( TransformListType::iterator it =
          ioTransformList.begin();
        it != ioTransformList.end(); ++it )
    {
    this->m_TransformList.push_back( TransformPointer(*it) );
    }

  //
  // in the case where the first transform in the list is a
  // CompositeTransform, add all the transforms to that first
  // transform.
  std::string transformName =
    ioTransformList.front()->GetNameOfClass();
  if(transformName.find("CompositeTransform") != std::string::npos)
    {

    TransformListType::const_iterator tit = ioTransformList.begin();
    TransformType *composite = (*tit).GetPointer();
    //
    // CompositeTransformIOHelper knows how to assign to the composite
    // transform's internal list
    CompositeTransformIOHelper helper;
    helper.SetTransformList(composite,ioTransformList);
    }
}

void TransformFileReader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk
