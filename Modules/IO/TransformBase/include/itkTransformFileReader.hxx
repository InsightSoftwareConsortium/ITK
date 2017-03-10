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
#ifndef itkTransformFileReader_hxx
#define itkTransformFileReader_hxx

#include "itkTransformFileReader.h"
#include "itkTransformIOFactory.h"
#include "itkCompositeTransformIOHelper.h"
#include "itkKernelTransform.h"

#include "itksys/SystemTools.hxx"


#ifndef ITK_TRANSFORM_FACTORY_MAX_DIM
#define ITK_TRANSFORM_FACTORY_MAX_DIM 9
#endif


namespace itk
{

namespace
{
// Class to initialize kernel transform for dimension D and lower
template< typename TParameterType, unsigned int Dimension>
struct KernelTransformHelper
{
  static int InitializeWMatrix(const typename TransformBaseTemplate<TParameterType>::Pointer &transform)
    {
    if( transform->GetInputSpaceDimension() == Dimension)
      {
      typedef typename itk::KernelTransform< TParameterType, Dimension > KernelTransformType;
      KernelTransformType* kernelTransform = static_cast<KernelTransformType*>(transform.GetPointer());
      kernelTransform->ComputeWMatrix();
      return 0;
      }
    else
      {
      // try one less dimension
      return KernelTransformHelper<TParameterType, Dimension-1>::InitializeWMatrix(transform);
      }
    }
};

// Template specialized class to stop initializing Kernel Transfoms.
template<typename TParameterType>
struct KernelTransformHelper<TParameterType, 0>
{
  static int InitializeWMatrix(const typename TransformBaseTemplate<TParameterType>::Pointer &itkNotUsed(transform))
    {
    return 1;
    }
};
}


template<typename TParametersValueType>
TransformFileReaderTemplate<TParametersValueType>
::TransformFileReaderTemplate() :
  m_FileName("") /* to be removed soon. See .h */
{
}


template<typename TParametersValueType>
TransformFileReaderTemplate<TParametersValueType>
::~TransformFileReaderTemplate()
{
}


template<typename TParametersValueType>
void TransformFileReaderTemplate<TParametersValueType>
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }

  if( m_TransformIO.IsNull() )
    {
    typedef TransformIOFactoryTemplate< TParametersValueType > TransformFactoryIOType;
    m_TransformIO = TransformFactoryIOType::CreateTransformIO( m_FileName.c_str(), /*TransformIOFactoryTemplate<TParametersValueType>::*/ ReadMode );
    if ( m_TransformIO.IsNull() )
      {
      std::ostringstream msg;
      msg << "Could not create Transform IO object for reading file "  << this->GetFileName() << std::endl;

      if ( !itksys::SystemTools::FileExists( m_FileName.c_str() ) )
        {
        msg << " File does not exists!";
        }

      std::list< LightObject::Pointer > allobjects =  ObjectFactoryBase::CreateAllInstance("itkTransformIOBaseTemplate");

      if (allobjects.size() > 0)
        {
        msg << "  Tried to create one of the following:" << std::endl;
        for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
              i != allobjects.end(); ++i )
          {
          const Object *obj = dynamic_cast<Object*>(i->GetPointer());
          msg << "    " << obj->GetNameOfClass() << std::endl;
          }
        msg << "  You probably failed to set a file suffix, or" << std::endl;
        msg << "    set the suffix to an unsupported type." << std::endl;
        }
      else
        {
        msg << "  There are no registered Transform IO factories." << std::endl;
        msg << "  Please visit https://www.itk.org/Wiki/ITK/FAQ#NoFactoryException to diagnose the problem." << std::endl;
        }

      itkExceptionMacro( << msg.str().c_str() );
      }


    }

  typename TransformIOType::TransformListType & ioTransformList = m_TransformIO->GetTransformList();
  // Clear old results.
  ioTransformList.clear();

  m_TransformIO->SetFileName(m_FileName);
  m_TransformIO->Read();

  // Clear old results.
  this->m_TransformList.clear();
  // If the transform is derived from itk::KernelTransform, the internal matrices
  // need to be initialized using the transform parameters.
  // kernelTransform->ComputeWMatrix() has to be called after the transform is read but
  // before the transform is used.
  std::string transformTypeName = ioTransformList.front()->GetNameOfClass();
  const size_t len = strlen("KernelTransform");// Computed at compile time in most cases
  if (transformTypeName.size() >= len
      && !transformTypeName.compare(transformTypeName.size()-len , len, "KernelTransform"))
    {
    if( KernelTransformHelper<TParametersValueType,
        ITK_TRANSFORM_FACTORY_MAX_DIM>::InitializeWMatrix(ioTransformList.front().GetPointer()))
      {
      itkDebugMacro(<< "KernelTransform with dimension "
                    << ioTransformList.front()->GetInputSpaceDimension()
                    << " is not automatically initialized. \"ComputeWMatrix()\""
                       " method has to be called."
                   );
      }
    }

  // In the case where the first transform in the list is a
  // CompositeTransform, add all the transforms to that first
  // transform. and return a single composite item on the
  // m_TransformList
  const std::string firstTransformName = ioTransformList.front()->GetNameOfClass();
  if(firstTransformName.find("CompositeTransform") != std::string::npos)
    {
    typename TransformListType::const_iterator tit = ioTransformList.begin();
    typename TransformType::Pointer composite = (*tit).GetPointer();

    // CompositeTransformIOHelperTemplate knows how to assign to the composite
    // transform's internal list
    CompositeTransformIOHelperTemplate<TParametersValueType> helper;
    helper.SetTransformList(composite.GetPointer(),ioTransformList);

    this->m_TransformList.push_back( composite.GetPointer() );
    }
  else  //Just return the entire list of elements
    {
    for ( typename TransformListType::iterator it =
         ioTransformList.begin();
         it != ioTransformList.end(); ++it )
      {
      this->m_TransformList.push_back( (*it).GetPointer() );
      }
    }
}


template<typename TParametersValueType>
void TransformFileReaderTemplate<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk

#endif
