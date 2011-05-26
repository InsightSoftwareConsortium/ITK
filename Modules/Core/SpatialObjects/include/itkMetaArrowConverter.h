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
#ifndef __itkMetaArrowConverter_h
#define __itkMetaArrowConverter_h

#include "itkArrowSpatialObject.h"
#include "metaArrow.h"

namespace itk
{
template< unsigned int NDimensions = 3 >
class ITK_EXPORT MetaArrowConverter : public Object
{
public:
 /** Standard class typedefs */
  typedef MetaArrowConverter           Self;
  typedef Object                       Superclass;
  typedef SmartPointer< Self >         Pointer;
  typedef SmartPointer< const Self >   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaArrowConverter, Object);

  typedef ArrowSpatialObject< NDimensions >     SpatialObjectType;
  typedef typename SpatialObjectType::Pointer   SpatialObjectPointer;

  static SpatialObjectPointer ReadMeta(const char *name);

  static bool WriteMeta(const SpatialObjectType *spatialObject, const char *name);

  static SpatialObjectPointer MetaArrowToArrowSpatialObject( const MetaArrow *arrow );

  static MetaArrow * ArrowSpatialObjectToMetaArrow(const SpatialObjectType *spatialObject);

protected:
  MetaArrowConverter();
  ~MetaArrowConverter() {}

private:
  MetaArrowConverter(const Self &);   //purposely not implemented
  void operator=(const Self &);       //purposely not implemented

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaArrowConverter.txx"
#endif

#endif
