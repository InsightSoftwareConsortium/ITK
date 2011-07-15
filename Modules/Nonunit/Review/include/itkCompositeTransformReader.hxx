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
#ifndef __itkCompositeTransformReader_hxx
#define __itkCompositeTransformReader_hxx

#include "itkCompositeTransformReader.h"
#include "itkTransformFactory.h"
#include "itksys/SystemTools.hxx"
#include <stdio.h>
#include <sstream>

namespace itk
{
//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
CompositeTransformReader<TScalar, NDimensions>
::CompositeTransformReader()
{
  m_CompositeTransform = NULL;
  TransformFactory<CompositeTransformType>::RegisterTransform();

  /* temporary. to be removed when final CompositeTransform IO is developed. */
  m_ReadingCompositeTransform = true;
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
CompositeTransformReader<TScalar, NDimensions>
::~CompositeTransformReader()
{}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformReader<TScalar, NDimensions>
::Update()
{
  CompositeTransformPointer compositeTransform;

  /* Call TransformFileReader::Update to read the file. It will
   * place the CompositeTransform and each component transform
   * in m_TransformList, as pointers to TransformBase */
  Superclass::Update();
  TransformListType* list = GetTransformList();
  TransformListType::const_iterator tit = list->begin();
  /* Cast first transform to CompositeTransform */
  if( strcmp( (*tit)->GetNameOfClass(), "CompositeTransform" ) )
    {
    itkExceptionMacro(
      "First transform in list must be of type CompositeTransform."
      << " Instead, found: " << (*tit)->GetNameOfClass() );
    }
  m_CompositeTransform =
    dynamic_cast< CompositeTransformType * > ( (*tit).GetPointer() );
  if( m_CompositeTransform.IsNull() )
    {
    itkExceptionMacro("Error converting to CompositeTransform.");
    }
  /* Cast each transform to ComponentTransformType so it can be
   * used in the CompositeTransform. */
  tit++;
  while( tit != list->end() )
    {
    ComponentTransformPointer component =
      dynamic_cast<ComponentTransformType *> ( (*tit).GetPointer() );
    if( component.IsNull() )
      {
      itkExceptionMacro(
        "Error converting component transform to proper type. Expected "
        << component->GetTransformTypeAsString() << ", but have "
        << (*tit)->GetTransformTypeAsString() << "." );
      }
    m_CompositeTransform->AddTransform( component );
    tit++;
    }
}

//---------------------------------------------------------
template<class TScalar, unsigned int NDimensions>
void
CompositeTransformReader<TScalar, NDimensions>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Composite Transform: ";
  if ( m_CompositeTransform.IsNull() )
    {
    os << indent << "(none)\n";
    }
  else
    {
    os << indent << m_CompositeTransform << "\n";
    }
}
} // end namespace itk

#endif
