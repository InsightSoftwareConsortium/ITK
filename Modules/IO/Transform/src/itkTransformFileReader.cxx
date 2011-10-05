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
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"

namespace itk
{
/** Constructor */
TransformFileReader
::TransformFileReader()
{
  m_FileName = "";
  TransformFactoryBase::RegisterDefaultTransforms();
  /* to be removed soon. See .h */
  m_ReadingCompositeTransform = false;
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

  /* Check for a CompositeTransform. This check can be removed
   * once the CompositeTransform IO is moved into this class and
   * TranformFileWriter */
  if( ! m_ReadingCompositeTransform )
    {
    std::string transformName =
                   transformIO->GetTransformList().front()->GetNameOfClass();
    if( transformName.find("CompositeTransform") != std::string::npos )
      {
      itkExceptionMacro("Cannot read a file of type CompositeTransform. Use "
                        " CompositeTransformReader instead. ");
      }
    }
  for ( TransformListType::iterator it =
          transformIO->GetTransformList().begin();
        it != transformIO->GetTransformList().end(); ++it )
    {
    this->m_TransformList.push_back( TransformPointer(*it) );
    }
}

void TransformFileReader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk
