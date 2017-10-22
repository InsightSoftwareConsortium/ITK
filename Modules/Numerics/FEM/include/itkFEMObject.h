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
#ifndef itkFEMObject_h
#define itkFEMObject_h

#include "itkDataObject.h"

#include "itkFEMElementBase.h"
#include "itkFEMLinearSystemWrapper.h"
#include "itkFEMLinearSystemWrapperVNL.h"
#include "itkFEMLoadBase.h"
#include "itkFEMLoadNode.h"
#include "itkFEMLoadBC.h"
#include "itkFEMLoadBCMFC.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMLoadLandmark.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkVectorContainer.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/** \class FEMObject
 * \brief Implements N-dimensional Finite element (FE) models including
 *   elements, materials, and loads.
 *
 * \par Overview
 * FEMObject was created to provide an object in ITK that specifies
 * the entire FE model. This model can then be passed to the itk::fem::Solver
 * to generate a solution for the model. The design for this class was modelled
 * after the itk::Mesh structure. Presently, no direct I/O support for
 * the FEMObject exists. This must be done using the FEMSpatialObject.
 * The FEMObject simply serves as a storage container for the FE model.
 *
 * The FEMObject stores the FE problem using Vector Containers for
 *   1) Load
 *   2) Material
 *   3) Element
 *   4) Node
 *
 * \par Usage
 * The user can set the Vector Containers that define the Load,
 * Material, Element, and Nodes using the AddNext<Object> and
 * Insert<Object> methods. The user can also get the entire
 * VectorContainer using the Get<Object>Container(). For convience
 * methods are also provided to get any item in the vector containers
 * based on their index (Get<Object>) or their global number
 * (Get<Object>WithGlobalNumber). This class does not know anything
 * about the types of elements, materials, elements, or nodes. The
 * problem presently can only be 2D or 3D.
 *
 * \ingroup ITKFEM
 */

template <unsigned int VDimension = 3>
class ITK_TEMPLATE_EXPORT  FEMObject : public DataObject
{
public:
  /** Standard class typedefs. */
  typedef FEMObject                Self;
  typedef DataObject               Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(FEMObject, DataObject);

  itkStaticConstMacro(FEMDimension, unsigned int, VDimension);
  itkStaticConstMacro(MaxDimensions, unsigned int, 3);

  typedef unsigned long ElementIdentifier;
  typedef unsigned long NodeIdentifier;
  typedef unsigned long LoadIdentifier;
  typedef unsigned long MaterialIdentifier;

  /** Vector containers for 1) Load, 2) Material, 3) Element and 4) Node. */
  typedef VectorContainer<LoadIdentifier, Load::Pointer>          LoadContainerType;
  typedef VectorContainer<MaterialIdentifier, Material::Pointer>  MaterialContainerType;
  typedef VectorContainer<ElementIdentifier, Element::Pointer>    ElementContainerType;
  typedef VectorContainer<NodeIdentifier, Element::Node::Pointer> NodeContainerType;

  /** Create types that are pointers to each of the container types. */
  typedef typename ElementContainerType::Pointer       ElementContainerPointer;
  typedef typename ElementContainerType::ConstPointer  ElementContainerConstPointer;
  typedef typename NodeContainerType::Pointer          NodeContainerPointer;
  typedef typename NodeContainerType::ConstPointer     NodeContainerConstPointer;
  typedef typename LoadContainerType::Pointer          LoadContainerPointer;
  typedef typename LoadContainerType::ConstPointer     LoadContainerConstPointer;
  typedef typename MaterialContainerType::Pointer      MaterialContainerPointer;
  typedef typename MaterialContainerType::ConstPointer MaterialContainerConstPointer;

  /** Create types that are iterators for each of the container types. */
  typedef typename
  ElementContainerType::ConstIterator         ElementContainerConstIterator;
  typedef typename
  ElementContainerType::Iterator              ElementContainerIterator;
  typedef typename
  NodeContainerType::ConstIterator            NodeContainerConstIterator;
  typedef typename
  NodeContainerType::Iterator                 NodeContainerIterator;
  typedef typename
  LoadContainerType::ConstIterator            LoadContainerConstIterator;
  typedef typename
  LoadContainerType::Iterator                 LoadContainerIterator;
  typedef typename
  MaterialContainerType::ConstIterator        MaterialContainerConstIterator;
  typedef typename
  MaterialContainerType::Iterator             MaterialContainerIterator;

  // Copy the contents
  void DeepCopy(FEMObject *Copy);

  // Get methods to get the entire VectorContainers for Elements, Nodes, Loads, and Materials
  itkGetModifiableObjectMacro(ElementContainer, ElementContainerType);
  itkGetModifiableObjectMacro(NodeContainer, NodeContainerType);
  itkGetModifiableObjectMacro(LoadContainer, LoadContainerType);
  itkGetModifiableObjectMacro(MaterialContainer, MaterialContainerType);

  /** Get the Degrees of Freedom for the FE model */
  unsigned int GetNumberOfDegreesOfFreedom(void) const
  {
    return m_NGFN;
  }

  /** Get the Degrees of Freedom for the FE model */
  unsigned int GetNumberOfMultiFreedomConstraints(void) const
  {
    return m_NMFC;
  }

  /** Get the Number of nodes in the FE mesh */
  unsigned int GetNumberOfNodes(void) const
  {
    return m_NodeContainer->Size();
  }

  /** Get the Number of elements in the FE mesh */
  unsigned int GetNumberOfElements(void) const
  {
    return m_ElementContainer->Size();
  }

  /** Get the Number of Loads in the FE problem */
  unsigned int GetNumberOfLoads(void) const
  {
    return m_LoadContainer->Size();
  }

  /** Get the Number of Materials in the FE problem */
  unsigned int GetNumberOfMaterials(void) const
  {
    return m_MaterialContainer->Size();
  }

  /**
  * Add next element to the element array
  */
  void AddNextElement(Element::Pointer e);

  /**
  * Insert an element at the specified location
  */
  void InsertElement(Element::Pointer e, ElementIdentifier index);

  /**
  * Add next node to the node array
  */
  void AddNextNode(Element::Node::Pointer e);

  /**
  * Insert a node at the specified index location
  */
  void InsertNode(Element::Node::Pointer e, NodeIdentifier index);

  /**
   * Add next material data to the material array
   */
  void AddNextMaterial(Material::Pointer mat)
  {
    this->AddNextMaterialInternal(mat.GetPointer());
  }
  void AddNextMaterial(MaterialLinearElasticity::Pointer mat)
  {
    this->AddNextMaterialInternal(mat.GetPointer());
  }
  /**
   * Insert material data at the specified index location
   */
  void InsertMaterial(Material::Pointer e, MaterialIdentifier index);

  /**
   * Add next load data to the load array
   */
  void AddNextLoad(Load::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  void AddNextLoad(LoadNode::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  void AddNextLoad(LoadBCMFC::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  void AddNextLoad(LoadBC::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  void AddNextLoad(LoadEdge::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  void AddNextLoad(LoadGravConst::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  void AddNextLoad(LoadLandmark::Pointer ld)
  { this->AddNextLoadInternal(ld.GetPointer()); }
  /**
   * Insert material data at the specified index location
   */
  void InsertLoad(Load::Pointer ld, LoadIdentifier index);

  /**
   * Get the element at the specified index location
   */
  Element::ConstPointer GetElement(ElementIdentifier index) const;
  Element::Pointer GetElement(ElementIdentifier index);

  /**
   * Get the element at with the specified global number
   */
  Element::ConstPointer GetElementWithGlobalNumber(int globalNumber) const;
  Element::Pointer GetElementWithGlobalNumber(int globalNumber);

  /**
   * Get the node at the specified index location
   */
  Element::Node::Pointer GetNode(NodeIdentifier index);
  Element::Node::ConstPointer GetNode(NodeIdentifier index) const;

  /**
   * Get the Node at with the specified global number
   */
  Element::Node::Pointer GetNodeWithGlobalNumber(int globalNumber);

  /**
   * Get the material data at the specified index location
   */
  Material::ConstPointer GetMaterial(MaterialIdentifier index) const;
  Material::Pointer GetMaterial(MaterialIdentifier index);

  /**
   * Get the Material at with the specified global number
   */
  Material::ConstPointer GetMaterialWithGlobalNumber(int globalNumber) const;
  Material::Pointer GetMaterialWithGlobalNumber(int globalNumber);

  /**
   * Get the load data at the specified index location
   */
  Load::ConstPointer GetLoad(LoadIdentifier index) const;
  Load::Pointer GetLoad(LoadIdentifier index);

  /**
   * Get the Load at with the specified global number
   */
  Load::Pointer GetLoadWithGlobalNumber(int globalNumber);

  /**
   * Clear the entire model and return to an initial state
   */
  void Clear();

  /**
   * Renumber the nodes global number based on their current order
   * in the Node VectorContainer
   */
  void RenumberNodeContainer();

  /**
   * This should be called when all nodes, elements, and loads
   * have been assigned. This method will then generate the
   * degrees of freedom for the speficied system and the number of
   * multi freedom constraints on the system.
   */
  void FinalizeMesh();

protected:
  /** Constructor for use by New() method. */
  FEMObject();
  ~FEMObject() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

  /**
    * Assign a global freedom numbers to each DOF in a system.
    * This must be done before any other solve function can be called.
    * This is called internally by FinalizeMesh()
    */
  void GenerateGFN();

  /**
   * Assign the number of multi freedom constraints on the system.
   * This must be done before any other solve function can be called.
   * This is called internally by FinalizeMesh()
   */
  void GenerateMFC();

  void AddNextMaterialInternal(Material *mat);
  /**
  * Number of global degrees of freedom in a system
  */
  unsigned int m_NGFN;

  /**
   * Number of multi freedom constraints in a system.
   * This member is set in a AssembleK function.
   */
  unsigned int m_NMFC;

  ElementContainerPointer  m_ElementContainer;
  NodeContainerPointer     m_NodeContainer;
  LoadContainerPointer     m_LoadContainer;
  MaterialContainerPointer m_MaterialContainer;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FEMObject);

  void AddNextLoadInternal(Load *l);
};  // End Class: FEMObject

}
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMObject.hxx"
#endif

#endif // #ifndef itkFEMObject_h
