/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#define ITK_TEMPLATE_EXPLICIT_TransformFileWriter
#include "itkTransformFileWriter.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"
#include "itkCompositeTransformIOHelper.h"
#include <string>

namespace itk
{


template <typename TParametersValueType>
TransformFileWriterTemplate<TParametersValueType>::TransformFileWriterTemplate()
  : m_FileName{ "" }
{
  TransformFactoryBase::RegisterDefaultTransforms();
}

template <typename TParametersValueType>
TransformFileWriterTemplate<TParametersValueType>::~TransformFileWriterTemplate() = default;

/** Set the writer to append to the specified file */
template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::SetAppendOn()
{
  this->SetAppendMode(true);
}

/** Set the writer to overwrite the specified file - This is the
 * default mode. */
template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::SetAppendOff()
{
  this->SetAppendMode(false);
}

/** Set the writer mode (append/overwrite). */
template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::SetAppendMode(bool mode)
{
  this->m_AppendMode = mode;
}

/** Get the writer mode. */
template <typename TParametersValueType>
bool
TransformFileWriterTemplate<TParametersValueType>::GetAppendMode()
{
  return (this->m_AppendMode);
}

template <>
void
TransformFileWriterTemplate<double>::PushBackTransformList(const Object * transObj);

template <>
void
TransformFileWriterTemplate<float>::PushBackTransformList(const Object * transObj);

/** Set the input transform and reinitialize the list of transforms */
template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::SetInput(const Object * transform)
{
  m_TransformList.clear();
  this->PushBackTransformList(transform);
}

template <typename TParametersValueType>
const typename TransformFileWriterTemplate<TParametersValueType>::TransformType *
TransformFileWriterTemplate<TParametersValueType>::GetInput()
{
  ConstTransformPointer res = *(m_TransformList.begin());
  return res.GetPointer();
}

/** Add a transform to be written */
template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::AddTransform(const Object * transform)
{
  /* Check for a CompositeTransform.
   * The convention is that there should be one, and it should
   * be the first transform in the file
   */
  const std::string transformName = transform->GetNameOfClass();
  if (transformName.find("CompositeTransform") != std::string::npos)
  {
    if (!this->m_TransformList.empty())
    {
      itkExceptionMacro("Can only write a transform of type CompositeTransform "
                        "as the first transform in the file.");
    }
  }

  this->PushBackTransformList(transform);
}

template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::Update()
{
  if (m_FileName.empty())
  {
    itkExceptionMacro("No file name given");
  }

  if (m_TransformIO.IsNull())
  {
    using TransformFactoryIOType = TransformIOFactoryTemplate<TParametersValueType>;
    m_TransformIO = TransformFactoryIOType::CreateTransformIO(m_FileName.c_str(), IOFileModeEnum::WriteMode);

    if (m_TransformIO.IsNull())
    {
      std::ostringstream msg;
      msg << "Could not create Transform IO object for writing file " << this->GetFileName() << std::endl;

      std::list<LightObject::Pointer> allobjects = ObjectFactoryBase::CreateAllInstance("itkTransformIOBaseTemplate");

      if (!allobjects.empty())
      {
        msg << "  Tried to create one of the following:" << std::endl;
        for (auto & allobject : allobjects)
        {
          const Object * obj = dynamic_cast<Object *>(allobject.GetPointer());
          msg << "    " << obj->GetNameOfClass() << std::endl;
        }
        msg << "  You probably failed to set a file suffix, or" << std::endl;
        msg << "    set the suffix to an unsupported type." << std::endl;
      }
      else
      {
        msg << "  There are no registered Transform IO factories." << std::endl;
        msg << "  Please visit https://www.itk.org/Wiki/ITK/FAQ#NoFactoryException to diagnose the problem."
            << std::endl;
      }

      itkExceptionMacro(<< msg.str().c_str());
    }
  }
  m_TransformIO->SetAppendMode(this->m_AppendMode);
  m_TransformIO->SetUseCompression(this->m_UseCompression);
  m_TransformIO->SetFileName(this->m_FileName);
  m_TransformIO->SetTransformList(this->m_TransformList);
  m_TransformIO->Write();
}

template <typename TParametersValueType>
void
TransformFileWriterTemplate<TParametersValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

namespace
{

/*
 This helper is used to:
 Create and set a new type transform that have requested output precision type.
 */
template <typename TInputTransformType, typename TOutputTransformType>
struct TransformIOHelper
{
  using OutputTransformPointer = typename TOutputTransformType::Pointer;
  using InputParameterValueType = typename TInputTransformType::ParametersValueType;
  using InputFixedParameterValueType = typename TInputTransformType::FixedParametersValueType;
  using OutputParameterValueType = typename TOutputTransformType::ParametersValueType;
  using OutputFixedParameterValueType = typename TOutputTransformType::FixedParametersValueType;

  /*
   This function gets the type name of the input transform and creates
   a new transform object based on the requested precision type.
   */
  static OutputTransformPointer
  CreateNewTypeTransform(std::string transformName)
  {
    // Transform name is modified to have the output precision type.
    TransformIOBaseTemplate<OutputParameterValueType>::CorrectTransformPrecisionType(transformName);

    // Instantiate the transform
    LightObject::Pointer   i = ObjectFactoryBase::CreateInstance(transformName.c_str());
    OutputTransformPointer convertedTransform = dynamic_cast<TOutputTransformType *>(i.GetPointer());
    if (convertedTransform.IsNull())
    {
      itkGenericExceptionMacro(<< "Could not create an instance of " << transformName);
    }
    convertedTransform->UnRegister();
    return convertedTransform;
  }

  /* Converts the value type of transform parameters to the output precision type */
  static OptimizerParameters<OutputParameterValueType>
  ConvertParametersType(const OptimizerParameters<InputParameterValueType> & sourceParams)
  {
    OptimizerParameters<OutputParameterValueType> outputParams;
    outputParams.SetSize(sourceParams.GetSize());
    for (SizeValueType i = 0; i < sourceParams.GetSize(); ++i)
    {
      outputParams[i] = static_cast<OutputParameterValueType>(sourceParams[i]);
    }
    return outputParams;
  }

  /* Converts the value type of transform parameters to the output precision type */
  static OptimizerParameters<OutputFixedParameterValueType>
  ConvertFixedParametersType(const OptimizerParameters<InputFixedParameterValueType> & sourceParams)
  {
    OptimizerParameters<OutputFixedParameterValueType> outputParams;
    outputParams.SetSize(sourceParams.GetSize());
    for (SizeValueType i = 0; i < sourceParams.GetSize(); ++i)
    {
      outputParams[i] = static_cast<OutputFixedParameterValueType>(sourceParams[i]);
    }
    return outputParams;
  }

  /* Set fixed parameters and parameters of the new type created transform */
  static void
  SetAllParameters(const TInputTransformType * transform, OutputTransformPointer & convertedTransform)
  {
    // The precision type of the input transform parameters should be converted to the requested output precision
    convertedTransform->SetFixedParameters(ConvertFixedParametersType(transform->GetFixedParameters()));
    convertedTransform->SetParameters(ConvertParametersType(transform->GetParameters()));
  }
};

} // namespace

namespace
{
/* Changes the precision type of input transform to the requested precision type */
template <typename TInputTransformType, typename TOutputTransformType>
inline void
AddToTransformList(typename TInputTransformType::ConstPointer &             transform,
                   std::list<typename TOutputTransformType::ConstPointer> & transformList)
{
  using InputParameterValueType = typename TInputTransformType::ParametersValueType;
  using OutputParameterValueType = typename TOutputTransformType::ParametersValueType;

  /* Pushes the converted transform to the input transform list */
  using InputTransformType = TransformBaseTemplate<InputParameterValueType>;
  using InputTransformConstPointer = typename InputTransformType::ConstPointer;
  using InputConstTransformListType = std::list<InputTransformConstPointer>;

  using OutputTransformType = TransformBaseTemplate<OutputParameterValueType>;
  using OutputTransformPointer = typename OutputTransformType::Pointer;
  using OutputTransformConstPointer = typename OutputTransformType::ConstPointer;
  using OutputTransformListType = std::list<OutputTransformPointer>;

  const std::string      transformName = transform->GetTransformTypeAsString();
  OutputTransformPointer convertedTransform;

  using IOhelper = TransformIOHelper<InputTransformType, OutputTransformType>;

  // Composite and DisplacementFieldTransform transforms should be treated differently.
  if (transformName.find("CompositeTransform") == std::string::npos)
  {
    convertedTransform = IOhelper::CreateNewTypeTransform(transformName);
    IOhelper::SetAllParameters(transform, convertedTransform);
  }
  else
  {
    /*
     Following steps are needed to process a composite transform:
     1) Use the compositeTransformIOHelper to get the input transforms list.
     2) Iterate through the input transform list, convert each sub transform and put them in the output transform list.
     3) Use a composite IO Helper agian to set the output transform list into the converted composite transform.
    */
    CompositeTransformIOHelperTemplate<InputParameterValueType> inputHelper;
    InputConstTransformListType inputTransformList = inputHelper.GetTransformList(transform);

    // create output transform list
    OutputTransformListType compositeTransformList;
    /*
     The first transform of the output transform list should be a composite transform
     we push back just an empty composite transform as it will be skipped by the outputHelper.
    */
    OutputTransformPointer outputComposite = IOhelper::CreateNewTypeTransform(transformName);
    compositeTransformList.push_back(outputComposite);

    // Now we iterate through input list and convert each sub transform to a new transform with requested precision
    // type.
    auto it = inputTransformList.begin();
    // composite transform is the first transform of the input transform list
    ++it; // skip the composite transform
    for (; it != inputTransformList.end(); ++it)
    {
      // get the input sub transform
      const auto * inSub = dynamic_cast<const InputTransformType *>((*it).GetPointer());
      // convert each sub transform and push them to the output transform list
      std::string            inSubName = inSub->GetTransformTypeAsString();
      OutputTransformPointer convertedSub = IOhelper::CreateNewTypeTransform(inSubName);
      IOhelper::SetAllParameters(inSub, convertedSub);
      // push back the converted sub transform to the composite transform list
      compositeTransformList.push_back(convertedSub);
    }

    convertedTransform = IOhelper::CreateNewTypeTransform(transformName); // new composite transform
    CompositeTransformIOHelperTemplate<OutputParameterValueType> outputHelper;
    // set the output transform list into the new composite transform
    outputHelper.SetTransformList(convertedTransform, compositeTransformList);
  }

  transformList.push_back(OutputTransformConstPointer(convertedTransform));
}

template <> // If types match, no conversion
void
AddToTransformList<TransformBaseTemplate<double>, TransformBaseTemplate<double>>(
  TransformBaseTemplate<double>::ConstPointer &            transform,
  std::list<TransformBaseTemplate<double>::ConstPointer> & transformList)
{
  transformList.push_back(transform);
}

template <> // If types match, no conversion
void
AddToTransformList<TransformBaseTemplate<float>, TransformBaseTemplate<float>>(
  TransformBaseTemplate<float>::ConstPointer &            transform,
  std::list<TransformBaseTemplate<float>::ConstPointer> & transformList)
{
  transformList.push_back(transform);
}


} // namespace

template <>
void
TransformFileWriterTemplate<double>::PushBackTransformList(const Object * transObj)
{
  TransformBaseTemplate<double>::ConstPointer dblptr = dynamic_cast<const TransformBaseTemplate<double> *>(transObj);
  if (dblptr.IsNotNull())
  {
    AddToTransformList<TransformBaseTemplate<double>, TransformBaseTemplate<double>>(dblptr, m_TransformList);
  }
  else
  {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>(transObj);
    if (fltptr.IsNotNull())
    {
      AddToTransformList<TransformBaseTemplate<float>, TransformBaseTemplate<double>>(fltptr, m_TransformList);
    }
    else
    {
      itkExceptionMacro("The input of writer should be either a double precision "
                        "or a single precision transform type. Called from "
                        "TransformFileWriterTemplate<double,double>::PushBackTransformList(...) ");
    }
  }
}

template <>
void
TransformFileWriterTemplate<float>::PushBackTransformList(const Object * transObj)
{
  TransformBaseTemplate<double>::ConstPointer dblptr = dynamic_cast<const TransformBaseTemplate<double> *>(transObj);
  if (dblptr.IsNotNull())
  {
    AddToTransformList<TransformBaseTemplate<double>, TransformBaseTemplate<float>>(dblptr, m_TransformList);
  }
  else
  {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>(transObj);
    if (fltptr.IsNotNull())
    {
      AddToTransformList<TransformBaseTemplate<float>, TransformBaseTemplate<float>>(fltptr, m_TransformList);
    }
    else
    {
      itkExceptionMacro("The input of writer should be whether a double precision "
                        "or a single precision transform type. Called from "
                        "TransformFileWriterTemplate<float,double>::PushBackTransformList(...) ");
    }
  }
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformBase_EXPORT TransformFileWriterTemplate<double>;
template class ITKIOTransformBase_EXPORT TransformFileWriterTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // namespace itk
