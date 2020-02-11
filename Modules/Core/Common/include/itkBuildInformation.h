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
#ifndef itkBuildInformation_h
#define itkBuildInformation_h

#include "itkMacro.h"
#include "itkObject.h"
#include <mutex>
#include "itkSingletonMacro.h"

namespace itk
{
/** \class BuildInformation
 * \brief Static information about this build
 *
 * This class provides a singleton interfacer to a static
 * std::map< std::string, std::string> of key value pairs
 * that describe this version of ITK.
 *
 * Values in in the map provide information about
 * the ITK (i.e. it's version, the build options,
 * configuration options, the home URL for the project,
 * and other static information) that can be gathered
 * at configuration time.
 *
 * \ingroup Common
 * \ingroup ITKCommon
 *
 */
struct BuildInformationGlobals;

class ITKCommon_EXPORT BuildInformation final : public Object
{
public:
  // Using the `rule of zero` to this aggregate type
  // C++20 changes the definition of aggregate such that classes with any user-declared ctors are no longer aggregates.
  ITK_DISALLOW_COPY_AND_ASSIGN(BuildInformation);

  /** Standard class type aliases. */
  using Self = BuildInformation;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using MapKeyType = std::string;
  using MapValueType = std::string;
  using MapValueDescriptionType = MapValueType;

private:
  /**
   * \class InformationValueType
   * An inner struct to organize dictionary values with descriptions
   * \ingroup Common
   * \ingroup ITKCommon
   */
  class InformationValueType
  {
  public:
    const MapValueType            m_Value;
    const MapValueDescriptionType m_Description;
  };

public:
  using MapStorageType = InformationValueType;
  using MapType = std::map<MapKeyType, MapStorageType>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BuildInformation, Object);

  /** Returns the global instance */
  static Pointer
  New();

  /** Returns the global singleton instance of the BuildInformation */
  static Pointer
  GetInstance();
  static const MapType &
  GetMap();
  static const MapValueType
  GetValue(const MapKeyType &&);
  static const MapValueDescriptionType
  GetDescription(const MapKeyType &&);
  static const std::vector<MapKeyType>
  GetAllKeys();

private:
  BuildInformation();

  itkGetGlobalDeclarationMacro(BuildInformationGlobals, PimplGlobals);
  static BuildInformationGlobals * m_PimplGlobals;

}; // end of class
} // end namespace itk
#endif
