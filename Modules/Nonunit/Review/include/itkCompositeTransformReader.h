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
#ifndef __itkCompositeTransformReader_h
#define __itkCompositeTransformReader_h

#include "itkLightProcessObject.h"
#include "itkCompositeTransform.h"
#include "itkTransformBase.h"
#include "itkTransformFileReader.h"
#include <vector>
#include <string>

namespace itk
{

/** \class CompositeTransformReader
 * \brief Reads a CompositeTransform object from file.
 *
 * This class reads a CompositeTransform from a file. To write files,
 * use CompositeTransformWriter. See CompositeTransformWriter for the file
 * format. The unique needs of writing and reading a CompositeTransform
 * motivated a pair of classes separate from TransformFileReader and
 * TransformFileWriter.
 *
 * The class is templated over the same parameters as the
 * CompositeTransform that is to be read.
 *
 * CompositeTransform files have the extension ".txt".
 *
 * The user sets the filename to be read using \c SetFileName. This is the
 * filename for a master transform file. Files for individual component
 * transforms, as created either by CompositeTransformWriter or otherwise,
 * are expected to be in the same dir as the master file. The component
 * transforms are each read by this class using TransformFileReader.
 *
 * The user calls Update to read the transform into memory.
 * The result is retrieved by the user via GetCompositeTransform.
 *
 * This class derives from TransformFileReader, which reads the transform
 * based on its ".txt" extension.
 *
 * \sa CompositeTranform, CompositeTranformWriter
 *
 * \ingroup ITKReview
 */
template
<class TScalar = double, unsigned int NDimensions = 3>
class ITK_EXPORT CompositeTransformReader:public TransformFileReader
{
public:
  /** Standard class typedefs. */
  typedef CompositeTransformReader   Self;
  typedef TransformFileReader        Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef CompositeTransform<TScalar, NDimensions> CompositeTransformType;
  typedef typename CompositeTransformType::Pointer CompositeTransformPointer;
  typedef typename CompositeTransformType::TransformType ComponentTransformType;
  typedef typename ComponentTransformType::Pointer ComponentTransformPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CompositeTransformReader, Superclass);

  /** Get the transform after reading */
  itkGetObjectMacro( CompositeTransform, CompositeTransformType );

  /** Read the transform. Overload this from TransformFileReader to
   * return the results as a CompositeTransform. */
  void Update();

protected:
  CompositeTransformReader();
  ~CompositeTransformReader();
  void PrintSelf(std::ostream & os, Indent indent) const;
  void OpenStream(std::ofstream & outputStream, bool binary);
  void CreateTransform(
    CompositeTransformPointer & ptr, const std::string & ClassName);

private:
  CompositeTransformReader(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented

  CompositeTransformPointer       m_CompositeTransform;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompositeTransformReader.hxx"
#endif

#endif // __itkCompositeTransformReader_h
