/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#define ITK_TEMPLATE_EXPLICIT_TxtTransformIO
#include "itkTxtTransformIO.h"
#include "itksys/SystemTools.hxx"
#include "vnl/vnl_matlab_read.h"
#include "vnl/vnl_matlab_write.h"
#include "itkTransformFileReader.h"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformIOHelper.h"
#include "itkNumberToString.h"
#include <sstream>
namespace itk
{
template <typename TParametersValueType>
TxtTransformIOTemplate<TParametersValueType>::TxtTransformIOTemplate() = default;

template <typename TParametersValueType>
TxtTransformIOTemplate<TParametersValueType>::~TxtTransformIOTemplate() = default;

template <typename TParametersValueType>
bool
TxtTransformIOTemplate<TParametersValueType>::CanReadFile(const char * fileName)
{
  bool recognizedExtension = false;

  recognizedExtension |= (itksys::SystemTools::GetFilenameLastExtension(fileName) == ".txt");
  recognizedExtension |= (itksys::SystemTools::GetFilenameLastExtension(fileName) == ".tfm");
  return recognizedExtension;
}

template <typename TParametersValueType>
bool
TxtTransformIOTemplate<TParametersValueType>::CanWriteFile(const char * fileName)
{
  bool recognizedExtension = false;

  recognizedExtension |= (itksys::SystemTools::GetFilenameLastExtension(fileName) == ".txt");
  recognizedExtension |= (itksys::SystemTools::GetFilenameLastExtension(fileName) == ".tfm");
  return recognizedExtension;
}

template <typename TParametersValueType>
std::string
TxtTransformIOTemplate<TParametersValueType>::trim(std::string const & source, char const * delims)
{
  std::string            result(source);
  std::string::size_type index = result.find_last_not_of(delims);

  if (index != std::string::npos)
  {
    result.erase(++index);
  }

  index = result.find_first_not_of(delims);
  if (index != std::string::npos)
  {
    result.erase(0, index);
  }
  else
  {
    result.erase();
  }
  return result;
}

template <typename TParametersValueType>
void
TxtTransformIOTemplate<TParametersValueType>::ReadComponentFile(std::string Value)
{
  /* Used for reading component files listed in a composite transform
   * file, which should be read by CompositeTransformReader for assembly
   * into a CompositeTransform.
   * The component filenames are listed w/out paths, and are expected
   * to be in the same path as the master file. */
  std::string filePath = itksys::SystemTools::GetFilenamePath(this->GetFileName()) + "/";

  /* Use TransformFileReader to read each component file. */
  typename TransformFileReaderTemplate<TParametersValueType>::Pointer reader =
    TransformFileReaderTemplate<TParametersValueType>::New();
  std::string componentFullPath = filePath + Value;
  reader->SetFileName(componentFullPath);
  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & ex)
  {
    itkExceptionMacro("Error reading component file: " << Value << std::endl << ex);
  }
  TransformPointer transform = reader->GetTransformList()->front().GetPointer();
  this->GetReadTransformList().push_back(transform);
}

template <typename TParametersValueType>
void
TxtTransformIOTemplate<TParametersValueType>::Read()
{
  TransformPointer transform;
  std::ifstream    in;

  in.open(this->GetFileName(), std::ios::in | std::ios::binary);
  if (in.fail())
  {
    in.close();
    itkExceptionMacro("The file could not be opened for read access " << std::endl
                                                                      << "Filename: \"" << this->GetFileName() << "\"");
  }

  itkDebugMacro("Read file transform Data");

  // Read line by line
  typename TransformType::ParametersType VectorBuffer;

  typename TransformType::ParametersType TmpParameterArray;
  TmpParameterArray.clear();
  typename TransformType::FixedParametersType TmpFixedParameterArray;
  TmpFixedParameterArray.clear();
  bool haveFixedParameters = false;
  bool haveParameters = false;

  std::string line;

  while (std::getline(in, line))
  {
    line = trim(line);
    itkDebugMacro("Found line: \"" << line << "\"");

    if (line.length() == 0)
    {
      continue;
    }
    if (line[0] == '#' || std::string::npos == line.find_first_not_of(" \t"))
    {
      // Skip lines beginning with #, or blank lines
      continue;
    }

    // Get the name
    const std::string::size_type end = line.find(":");
    if (end == std::string::npos)
    {
      // Throw an error
      itkExceptionMacro("Tags must be delimited by :");
    }
    std::string Name = trim(line.substr(0, end));
    std::string Value = trim(line.substr(end + 1, line.length()));
    // Push back
    itkDebugMacro("Name: \"" << Name << "\"");
    itkDebugMacro("Value: \"" << Value << "\"");
    std::istringstream parse(Value);
    VectorBuffer.clear();
    if (Name == "Transform")
    {
      // Transform name should be modified to have the output precision type.
      Superclass::CorrectTransformPrecisionType(Value);

      this->CreateTransform(transform, Value);
      this->GetReadTransformList().push_back(transform);
    }
    else if (Name == "ComponentTransformFile")
    {
      /* Used by CompositeTransform file */
      ReadComponentFile(Value);
    }
    else if (Name == "Parameters" || Name == "FixedParameters")
    {
      VectorBuffer.clear();

      // Read them
      parse >> VectorBuffer;
      itkDebugMacro("Parsed: " << VectorBuffer);
      if (Name == "Parameters")
      {
        TmpParameterArray = VectorBuffer;
        itkDebugMacro("Setting Parameters: " << TmpParameterArray);
        if (haveFixedParameters)
        {
          transform->SetFixedParameters(TmpFixedParameterArray);
          itkDebugMacro("Set Transform Fixed Parameters");
          transform->SetParametersByValue(TmpParameterArray);
          itkDebugMacro("Set Transform Parameters");
          TmpParameterArray.clear();
          TmpFixedParameterArray.clear();
          haveFixedParameters = false;
          haveParameters = false;
        }
        else
        {
          haveParameters = true;
        }
      }
      else if (Name == "FixedParameters")
      {
        TmpFixedParameterArray = VectorBuffer;
        itkDebugMacro("Setting Fixed Parameters: " << TmpFixedParameterArray);
        if (!transform)
        {
          itkExceptionMacro("Please set the transform before parameters"
                            "or fixed parameters");
        }
        if (haveParameters)
        {
          transform->SetFixedParameters(TmpFixedParameterArray);
          itkDebugMacro("Set Transform Fixed Parameters");
          transform->SetParametersByValue(TmpParameterArray);
          itkDebugMacro("Set Transform Parameters");
          TmpParameterArray.clear();
          TmpFixedParameterArray.clear();
          haveFixedParameters = false;
          haveParameters = false;
        }
        else
        {
          haveFixedParameters = true;
        }
      }
    }
  }
}

namespace itk_impl_details
{
template <typename TParametersValueType>
inline void
print_vector(std::ofstream & s, vnl_vector<TParametersValueType> const & v)
{
  for (unsigned int i = 0; i + 1 < v.size(); ++i)
  {
    s << ConvertNumberToString(v[i]) << ' ';
  }
  if (!v.empty())
  {
    s << ConvertNumberToString(v.back());
  }
}
} // namespace itk_impl_details

template <typename TParametersValueType>
void
TxtTransformIOTemplate<TParametersValueType>::Write()
{
  ConstTransformListType & transformList = this->GetWriteTransformList();

  std::ofstream out;
  this->OpenStream(out, false);

  out << "#Insight Transform File V1.0" << std::endl;

  const std::string CompositeTransformTypeName = transformList.front()->GetTransformTypeAsString();
  //
  // if the first transform in the list is a
  // composite transform, use its internal list
  // instead of the IO
  CompositeTransformIOHelperTemplate<TParametersValueType> helper;
  if (CompositeTransformTypeName.find("CompositeTransform") != std::string::npos)
  {
    transformList = helper.GetTransformList(transformList.front().GetPointer());
  }
  int count = 0;

  typename ConstTransformListType::const_iterator end = transformList.end();

  for (typename ConstTransformListType::const_iterator it = transformList.begin(); it != end; ++it, ++count)
  {
    const std::string TransformTypeName = (*it)->GetTransformTypeAsString();
    out << "#Transform " << count << std::endl;
    out << "Transform: " << (*it)->GetTransformTypeAsString() << std::endl;
    //
    // Composite Transforms are not written out with parameters;
    // their parameters are the union of all their component
    // transforms' parameters.
    if (TransformTypeName.find("CompositeTransform") != std::string::npos)
    {
      if (count > 0)
      {
        itkExceptionMacro(<< "Composite Transform can only be 1st transform in a file");
      }
    }
    else
    {
      {
        vnl_vector<ParametersValueType> TempArray = (*it)->GetParameters();
        out << "Parameters: "; // << TempArray << std::endl;
        itk_impl_details::print_vector(out, TempArray);
        out << std::endl;
      }
      {
        vnl_vector<FixedParametersValueType> FixedTempArray = (*it)->GetFixedParameters();
        out << "FixedParameters: "; // << FixedTempArray << std::endl;
        itk_impl_details::print_vector(out, FixedTempArray);
        out << std::endl;
      }
    }
  }
  out.close();
}

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformInsightLegacy_EXPORT TxtTransformIOTemplate<double>;
template class ITKIOTransformInsightLegacy_EXPORT TxtTransformIOTemplate<float>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
