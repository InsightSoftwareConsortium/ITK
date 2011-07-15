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
#ifndef __itkCompositeTransformWriter_h
#define __itkCompositeTransformWriter_h

#include "itkLightProcessObject.h"
#include "itkCompositeTransform.h"
#include <vector>
#include <string>

namespace itk
{

/** \class CompositeTransformWriter
 * \brief Writes a CompositeTransform object out as a master file and
 *  series of individual transform files.
 *
 * This class writes a CompositeTransform to disk. To read the output files,
 * use CompositeTransformReader. The unique needs of writing and reading a
 * CompositeTransform motivated a pair of classes separate from
 * TransformFileReader and TransformFileWriter.
 *
 * The class is templated over same parameters as the CompositeTransform it is
 * to write.
 *
 * The output consists of a master file and zero or more additional files that
 * contain the actual component transforms. The master file stores the type
 * information of the CompositeTransform, the number of component transforms
 * and a list of filenames for each component transform. Each component
 * transform is written to a separate file using TransformFileWriter.
 *
 * The user must first set the input CompositeTransform using SetInput, then
 * set the master filename with full path using SetMasterFullPath. Next the
 * component filenames must be set, using one of these methods: 1) as full
 * paths using SetFirstComponentFullPath and AddComponentFullPath, or using
 * just SetComponentFullPaths; 2) as filenames only, using
 * SetComponentFileNames. In this case the passed filenames are made into full
 * paths by adding the path defined for the master filename; or, 3) by
 * passing only file extensions using SetComponentFileNamesByExtensions or
 * SetComponentFileNamesByCommonExtension. In these cases the filenames are
 * created using the master filename, without its extension, as a prefix to
 * which is added the component's positional index within the
 * CompositeTransform, and then the designated extension. The path from the
 * master filename is used to create the full path for the components.
 *
 * Note that in all cases, full paths are required or created for the component
 * filenames, and the component files are always written to the same directory
 * as the master file. Also, the user must set appropriate filename extensions
 * for the components, currently either '.txt' for ascii output, or '.mat' for
 * binary output.
 *
 * TODO
 *
 * Write to subdir?
 * Do we add an option to save component filenames in a subdir? Possibly
 * just a flag, and name the subdir automatically from master filename?
 *
 * Nested composite transforms
 * CompositeTransforms are currently not allowed here as components of
 * the input composite transform.
 * They'll generate an error if the extension given for them is not recognized,
 * but should we add an explicit check?
 *
 * File extension
 * Do we settle on an extension for composite transforms, and enforce it?
 * Maybe ".ctf"? It's used mainly by AVG anti-virus software, from what
 * I see.
 *
 * \sa CompositeTranform, CompositeTransformReader
 *
 * \ingroup ITKReview
 */
template
<class TScalar = double, unsigned int NDimensions = 3>
class ITK_EXPORT CompositeTransformWriter:public LightProcessObject
{
public:
  /** Standard class typedefs. */
  typedef CompositeTransformWriter   Self;
  typedef LightProcessObject         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CompositeTransformWriter, Superclass);

  /** Composite Transform type */
  typedef CompositeTransform<TScalar, NDimensions> CompositeTransformType;
  typedef typename CompositeTransformType::ConstPointer
    ConstCompositeTransformPointer;

  /** Some convenient typedefs. */
  typedef std::vector< std::string >           FileNamesContainer;

  /** Set/Get master filename
   *  This should include a full absoulte path, or relative path, unless the
   *  file should be written to the runtime working dir. */
  itkSetStringMacro(MasterFullPath);
  itkGetStringMacro(MasterFullPath);

  /** Set/Get the input transform to write */
  void SetInput(const CompositeTransformType *transform)
    {
    this->m_CompositeTransform = transform;
    }

  ConstCompositeTransformPointer GetInput()
    {
    return *( m_CompositeTransform );
    }

  /** Set the first component file name, as full path, to be processed.
   * This deletes existing filenames. */
  void SetFirstComponentFullPath(std::string const & name)
    {
    m_ComponentFullPaths.clear();
    m_ComponentFullPaths.push_back(name);
    this->Modified();
    }

  /** Add a single component filename as full path to the list of files.
   * To add a vector of filenames, use the SetComponentFullPaths method.
   * Push to back of list to follow same order as CompositeTransform.
   */
  void AddComponentFullPath(std::string const & name)
    {
    m_ComponentFullPaths.push_back(name);
    this->Modified();
    }

  /** Set/Get vector of strings with the component file names as full paths.
   *  Filenames are matched with transforms in sequential order.
   *  Include full paths. */
  void SetComponentFullPaths(const FileNamesContainer & name)
    {
    if ( m_ComponentFullPaths != name )
      {
      m_ComponentFullPaths = name;
      this->Modified();
      }
    }

  const FileNamesContainer & GetComponentFullPaths() const
    {
    return m_ComponentFullPaths;
    }

  /** Set component file names as filnames only, without paths.
   *  Filenames are matched with transforms in sequential order.
   *  Do NOT include full paths here. These filenames will be concatentated
   *  with the path set in SetMasterFileName()
   *  MasterFileName must already be set. */
  void SetComponentFileNames(const FileNamesContainer & name);

  /** Set the component filenames by passing a list of extensions, one
   * for each transform. Full filenames are created from the master filename
   * and the transform number.
   * MasterFileName must already be set. */
  void SetComponentFileNamesByExtensions(const FileNamesContainer & extensions);

  /** Set the component filenames by passing a single common extension. Each
   * transform will be written out using this extension.
   * Full filenames are created from the master filename
   * and the transform number.
   * MasterFileName must already be set. */
  void SetComponentFileNamesByCommonExtension(const std::string & extension);

  /* Maybe have a version for just passing each extension as separate string
   * parameter. Better to use default null params than vararg probably, e.g.
   * Set..ByExtensions( string name1, string name2=NULL, string name3=NULL ...)
   */

  /** Write out the transform */
  void Update();

protected:
  CompositeTransformWriter();
  ~CompositeTransformWriter();
  void PrintSelf(std::ostream & os, Indent indent) const;
  void OpenStream(std::ofstream & outputStream, bool binary);

private:
  CompositeTransformWriter(const Self &); //purposely not implemented
  void operator=(const Self &);    //purposely not implemented

  std::string                       m_MasterFullPath;
  ConstCompositeTransformPointer    m_CompositeTransform;
  /** A list of component transform filenames to be written. */
  FileNamesContainer                m_ComponentFullPaths;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompositeTransformWriter.hxx"
#endif

#endif // __itkCompositeTransformWriter_h
