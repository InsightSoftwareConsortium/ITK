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
#ifndef itkMetaConverterBase_h
#define itkMetaConverterBase_h

#include "itkSpatialObject.h"
#include "metaObject.h"

namespace itk
{

/** \class MetaConverterBase
 *  \brief Base class for MetaObject<->SpatialObject converters
 *
 *  SpatialObject scenes are written and read using the MetaIO
 *  Library.  This is managed by the MetaSceneConverter class,
 *  which converts MetaObject scenes to SpatialObject scenes and
 *  vice versa.
 *
 *  MetaScene walks the scene and uses the converter on each
 *  object in the scene.
 *
 *  \ingroup ITKSpatialObjects
 */
template <unsigned VDimension = 3>
class ITK_TEMPLATE_EXPORT MetaConverterBase : public Object
{
public:
  /** standard class typedefs */
  typedef MetaConverterBase          Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaConverterBase, Object);

  typedef SpatialObject<VDimension>           SpatialObjectType;
  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;
  typedef MetaObject                          MetaObjectType;

  /** Read a MetaIO file, return a SpatialObject */
  virtual SpatialObjectPointer  ReadMeta(const char *name);

  /** Write a MetaIO file based on this SpatialObject */
  virtual bool WriteMeta(const SpatialObjectType *spatialObject, const char *name);

  /** Convert the MetaObject to Spatial Object */
  virtual SpatialObjectPointer MetaObjectToSpatialObject(const MetaObjectType *mo) = 0;

  /** Convert the SpatialObject to MetaObject */
  virtual MetaObjectType *SpatialObjectToMetaObject(const SpatialObjectType *spatialObject) = 0;

  /** Set/Get flag for writing images to separate files in metaImage
   * instances
   */
  void SetWriteImagesInSeparateFile(bool writeImagesInSeparateFile);
  bool GetWriteImagesInSeparateFile();

protected:
  /** Creator for specific metaObject, defined in subclass */
  virtual MetaObjectType *CreateMetaObject() = 0;
  MetaConverterBase() : m_WriteImagesInSeparateFile(false)
    {}

private:
  bool m_WriteImagesInSeparateFile;
};

}

#ifndef ITK_MANUAL_INSTATIATION
  #include "itkMetaConverterBase.hxx"
#endif

#endif // itkMetaConverterBase_h
