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

#ifndef itkImageToRectilinearFEMObjectFilter_h
#define itkImageToRectilinearFEMObjectFilter_h

#include "vnl/vnl_vector.h"
#include "itkFEMObject.h"
#include "itkProcessObject.h"

namespace itk
{
namespace fem
{
/**
 * \class ImageToRectilinearFEMObjectFilter
 * \brief Generate a rectilinar mesh from an image. The result is stored
 *        in a FEMObject
 *
 * This class generates a Mesh consiting of quadrilateral elements in 2D
 * and hexahedral elements in 3D. The resulting meshes can be used with
 * specific elements for solving membrane or linear elasticity problems.
 *
 * \ingroup ITKFEM
 */

template <typename TInputImage>
class ITK_TEMPLATE_EXPORT ImageToRectilinearFEMObjectFilter : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef ImageToRectilinearFEMObjectFilter Self;
  typedef ProcessObject                     Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToRectilinearFEMObjectFilter, ProcessObject);

  itkStaticConstMacro(NDimensions, unsigned int, TInputImage::ImageDimension);

  /** Typedefs for Input Image */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      ImagePointer;
  typedef typename InputImageType::ConstPointer ImageConstPointer;
  typedef typename InputImageType::RegionType   ImageRegionType;
  typedef typename InputImageType::SizeType     ImageSizeType;
  typedef typename InputImageType::PointType    ImagePointType;
  typedef typename InputImageType::IndexType    ImageIndexType;

  /** Typedefs for Output FEMObject */
  typedef typename itk::fem::FEMObject<NDimensions> FEMObjectType;
  typedef typename FEMObjectType::Pointer           FEMObjectPointer;
  typedef typename FEMObjectType::ConstPointer      FEMObjectConstPointer;
  typedef typename DataObject::Pointer              DataObjectPointer;

  /** Some convenient typedefs. */
  typedef itk::fem::MaterialLinearElasticity MaterialType;
  typedef MaterialType::Pointer              MaterialPointerType;
  // typedef itk::fem::Element2DC0LinearQuadrilateral  QuadElementBaseType;
  // typedef itk::fem::Element3DC0LinearHexahedron     HexElementBaseType;
  typedef itk::fem::Element               ElementBaseType;
  typedef itk::fem::Element::ConstPointer ElementBasePointerType;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
//  itkConceptMacro(SameDimensionOrMinusOne,
//    (Concept::SameDimensionOrMinusOne<NDimensions, 3>));
// End concept checking
#endif

  /**Get/Set the number of voxels/pixels in each dimension used
    *during the mesh generation
    */
  itkGetMacro(PixelsPerElement, vnl_vector<unsigned int> );
  itkSetMacro(PixelsPerElement, vnl_vector<unsigned int> );
  void SetPixelsPerElement( unsigned int numPixels )
  {
    this->m_PixelsPerElement.fill( numPixels );
  }

  /**Get the number of element in each dimension of the generated mesh*/
  itkGetMacro(NumberOfElements, vnl_vector<unsigned int> );

  /**Get/Set the material used for the mesh */
  itkGetMacro(Material, MaterialPointerType);
  itkSetMacro(Material, MaterialPointerType);

  /**Get/Set the element type used to generate the mesh */
  itkGetMacro(Element, ElementBasePointerType);
  itkSetMacro(Element, ElementBasePointerType);

  /** Set/Get the image input of this process object.  */
  using Superclass::SetInput;
  void SetInput( InputImageType *image);

  void SetInput( unsigned int, InputImageType *image);

  InputImageType * GetInput();

  InputImageType * GetInput(unsigned int idx);

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  /** Get the output data of this process object.  The output of this
   * function is not valid until an appropriate Update() method has
   * been called, either explicitly or implicitly.  Both the filter
   * itself and the data object have Update() methods, and both
   * methods update the data.
   *
   * For Filters which have multiple outputs of different types, the
   * GetOutput() method assumes the output is of OutputImageType. For
   * the GetOutput(unsigned int) method, a dynamic_cast is performed
   * incase the filter has outputs of different types or image
   * types. Derived classes should have names get methods for these
   * outputs.
   */
  FEMObjectType * GetOutput();

  FEMObjectType * GetOutput(unsigned int idx);

protected:
  ImageToRectilinearFEMObjectFilter();
  virtual ~ImageToRectilinearFEMObjectFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /** Method invoked by the pipeline in order to trigger mesh generation */
  virtual void  GenerateData() ITK_OVERRIDE;

  void Generate2DRectilinearMesh();

  void Generate3DRectilinearMesh();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToRectilinearFEMObjectFilter);

  vnl_vector<unsigned int> m_NumberOfElements;
  vnl_vector<unsigned int> m_PixelsPerElement;
  MaterialPointerType      m_Material;
  ElementBasePointerType   m_Element;
};

}
}  // end namespace itk::fem

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToRectilinearFEMObjectFilter.hxx"
#endif

#endif // #ifndef itkImageToRectilinearFEMObjectFilter_h
