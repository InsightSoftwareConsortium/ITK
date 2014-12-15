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
#ifndef itkFEMObject_hxx
#define itkFEMObject_hxx

#include "itkFEMObject.h"

#include "itkFEMLoadNode.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMLoadBC.h"
#include "itkFEMLoadBCMFC.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMLoadLandmark.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkFEMFactoryBase.h"
#include "itkObjectFactoryBase.h"

#include <algorithm>

namespace itk
{
namespace fem
{
/*
 * Default constructor for FEMObject class
 */
template <unsigned int VDimension>
FEMObject<VDimension>
::FEMObject()
{
  m_NGFN = 0;
  m_NMFC = 0;
  this->m_ElementContainer = ElementContainerType::New();
  this->m_NodeContainer = NodeContainerType::New();
  this->m_LoadContainer = LoadContainerType::New();
  this->m_MaterialContainer = MaterialContainerType::New();
}

template <unsigned int VDimension>
FEMObject<VDimension>
::~FEMObject()
{
  this->Clear();
  this->m_ElementContainer = ITK_NULLPTR;
  this->m_NodeContainer = ITK_NULLPTR;
  this->m_LoadContainer = ITK_NULLPTR;
  this->m_MaterialContainer = ITK_NULLPTR;
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::Clear()
{
  // Required because of circular references between nodes
  // and elements
  int numElements = this->GetNumberOfElements();
  for( int e = 0; e < numElements; e++ )
    {
    Element::Pointer el = this->GetElement(e);
    unsigned int     Npts = el->GetNumberOfNodes();
    for( unsigned int pt = 0; pt < Npts; pt++ )
      {
      el->GetNode(pt)->m_elements.clear( );
      }
    }
  this->m_ElementContainer->Initialize();

  int numNodes = this->GetNumberOfNodes();
  for(int e = 0; e < numNodes; e++)
    {
    Element::Node::Pointer n = this->GetNode(e);
    n->m_elements.clear();
    }
  this->m_NodeContainer->Initialize();
  int numLoads = this->GetNumberOfLoads();
  for(int e = 0; e < numLoads; e++)
    {
    Element::Pointer dummy;
    Load *l = this->GetLoad(e).GetPointer();
    l->SetElement(dummy);
    }
  this->m_LoadContainer->Initialize();
  this->m_MaterialContainer->Initialize();

  this->m_NGFN = 0;
  this->m_NMFC = 0;
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::DeepCopy( FEMObject *Copy)
{
  this->Clear();

  // copy node information
  int                     numNodes = Copy->GetNumberOfNodes();
  fem::Element::Node::Pointer n1;

  itk::fem::Element::VectorType pt(VDimension);
  for( int i = 0; i < numNodes; i++ )
    {
    n1 = fem::Element::Node::New();
    for( unsigned int j = 0; j < VDimension; j++ )
      {
      pt[j] = Copy->GetNode(i)->GetCoordinates()[j];
      }
    n1->SetCoordinates(pt);
    n1->SetGlobalNumber(Copy->GetNode(i)->GetGlobalNumber() );
    this->AddNextNode(n1);
    }

  // copy material information
  int                                         numMat = Copy->GetNumberOfMaterials();
  for( int i = 0; i < numMat; i++ )
    {
    fem::MaterialLinearElasticity *mCopy =
      dynamic_cast<fem::MaterialLinearElasticity *>( Copy->GetMaterial(i).GetPointer() );
    if(mCopy == ITK_NULLPTR)
      {
      itkExceptionMacro(<< "dynamic_cast failed.");
      }
    fem::MaterialLinearElasticity::Pointer m = fem::MaterialLinearElasticity::New();
    m->SetGlobalNumber(mCopy->GetGlobalNumber() );
    m->SetYoungsModulus(mCopy->GetYoungsModulus() );
    m->SetPoissonsRatio(mCopy->GetPoissonsRatio() );
    m->SetCrossSectionalArea(mCopy->GetCrossSectionalArea() );
    m->SetMomentOfInertia(mCopy->GetMomentOfInertia() );
    this->AddNextMaterial(m);
    }

  // copy element information
  int                       numElements = Copy->GetNumberOfElements();
  itk::LightObject::Pointer a = ITK_NULLPTR;
  for( int i = 0; i < numElements; i++ )
    {
    fem::Element *elCopy = Copy->GetElement(i);
    // create a new object of the correct class
    a = ObjectFactoryBase::CreateInstance( elCopy->GetNameOfClass() );
    a->UnRegister();
    fem::Element *o1 = dynamic_cast<fem::Element *>( a.GetPointer() );
    if(o1 == ITK_NULLPTR)
      {
      itkExceptionMacro(<< "dynamic_cast failed.");
      }
    o1->SetGlobalNumber(elCopy->GetGlobalNumber() );

    numNodes = elCopy->GetNumberOfNodes();
    for( int j = 0; j < numNodes; j++ )
      {
      o1->SetNode( j, (this->GetNodeWithGlobalNumber(elCopy->GetNode(j)->GetGlobalNumber() ) ));
      }

    int matNum = elCopy->GetMaterial()->GetGlobalNumber();
    o1->SetMaterial(const_cast<Material *>(this->GetMaterialWithGlobalNumber(matNum).GetPointer()));
    this->AddNextElement( o1 );
    }

  // Copy load/bc information
  int numLoads =  Copy->GetNumberOfLoads();
  for( int k = 0; k < numLoads; k++ )
    {
    fem::Load *load = Copy->GetLoad(k);
    // create a new object of the correct class

    std::string loadname = std::string(load->GetNameOfClass() );
    if( loadname == "LoadNode" )
      {
      fem::LoadNode *lCopy = dynamic_cast<fem::LoadNode *>( load );
      if(lCopy == ITK_NULLPTR)
        {
        itkExceptionMacro(<< "dynamic_cast failed.");
        }
      fem::LoadNode::Pointer o1 = fem::LoadNode::New();

      o1->SetGlobalNumber(lCopy->GetGlobalNumber() );

      o1->SetElement(this->GetElementWithGlobalNumber(lCopy->GetElement()->GetGlobalNumber() ) );

      o1->SetNode(lCopy->GetNode() );

      int                dim = VDimension;
      vnl_vector<double> F(dim);
      for( int i = 0; i < dim; i++ )
        {
        F[i] = lCopy->GetForce()[i];
        }
      o1->SetForce(F);
      this->AddNextLoad( o1 );
      }
    else if( loadname == "LoadBC" )
      {
      fem::LoadBC *lCopy = dynamic_cast<fem::LoadBC *>( load );
      if(lCopy == ITK_NULLPTR)
        {
        itkExceptionMacro(<< "dynamic_cast failed.");
        }

      fem::LoadBC::Pointer o1 = fem::LoadBC::New();

      o1->SetGlobalNumber(lCopy->GetGlobalNumber() );

      o1->SetDegreeOfFreedom(lCopy->GetDegreeOfFreedom() );

      o1->SetElement(this->GetElementWithGlobalNumber(lCopy->GetElement()->GetGlobalNumber() ) );

      int                numRHS = lCopy->GetValue().size();
      vnl_vector<double> F(numRHS);
      for( int i = 0; i < numRHS; i++ )
        {
        F[i] = lCopy->GetValue()[i];
        }
      o1->SetValue(F);
      this->AddNextLoad( o1 );
      }
    else if( loadname == "LoadBCMFC" )
      {
      fem::LoadBCMFC *lCopy = dynamic_cast<fem::LoadBCMFC *>(load);
      if(lCopy == ITK_NULLPTR)
        {
        itkExceptionMacro(<< "dynamic_cast failed.");
        }

      fem::LoadBCMFC::Pointer o1 = fem::LoadBCMFC::New();
      o1->SetGlobalNumber(lCopy->GetGlobalNumber() );

      int   NumLHS;
      int   elementGN;
      int   DOF;
      float Value;

      NumLHS = lCopy->GetNumberOfLeftHandSideTerms();
      for( int i = 0; i < NumLHS; i++ )
        {
        fem::LoadBCMFC::MFCTerm mfcTerm = lCopy->GetLeftHandSideArray()[i];
        elementGN = mfcTerm.m_element->GetGlobalNumber();

        DOF = mfcTerm.dof;

        Value = mfcTerm.value;

        o1->GetLeftHandSideArray().push_back(
          fem::LoadBCMFC::MFCTerm(this->GetElementWithGlobalNumber(elementGN).GetPointer(), DOF, Value) );
        }

      int NumRHS = lCopy->GetNumberOfRightHandSideTerms();
      for( int i = 0; i < NumRHS; i++ )
        {
        o1->GetRightHandSideArray().set_size(o1->GetRightHandSideArray().size() + 1);
        o1->GetRightHandSideArray().put(o1->GetRightHandSideArray().size() - 1, lCopy->GetRightHandSideArray()[i]);
        }
      this->AddNextLoad( o1 );
      }
    else if( loadname == "LoadEdge" )
      {
      fem::LoadEdge *lCopy = dynamic_cast<fem::LoadEdge *>( load );
      if(lCopy == ITK_NULLPTR)
        {
        itkExceptionMacro(<< "dynamic_cast failed.");
        }

      fem::LoadEdge::Pointer o1 = fem::LoadEdge::New();

      o1->SetGlobalNumber(lCopy->GetGlobalNumber() );

      int numRows, numCols;

      o1->AddNextElement(this->GetElementWithGlobalNumber(lCopy->GetElement(0)->GetGlobalNumber() ) );
      o1->SetGlobalNumber(lCopy->GetGlobalNumber() );
      o1->SetEdge(lCopy->GetEdge() );

      vnl_matrix<fem::Element::Float> force = lCopy->GetForce();

      numRows = force.rows();
      numCols = force.columns();

      if( numRows )
        {
        o1->GetForce().set_size(numRows, numCols);
        for( int i = 0; i < numRows; i++ )
          {
          for( int j = 0; j < numCols; j++ )
            {
            o1->GetForce()[i][j] = force[i][j];
            }
          }
        this->AddNextLoad( o1 );
        }
      }
    else if( loadname == "LoadGravConst" )
      {
      fem::LoadGravConst *lCopy = dynamic_cast<fem::LoadGravConst *>( load );
      if(lCopy == ITK_NULLPTR)
        {
        itkExceptionMacro(<< "dynamic_cast failed.");
        }

      fem::LoadGravConst::Pointer o1 = fem::LoadGravConst::New();

      o1->SetGlobalNumber(lCopy->GetGlobalNumber() );
      for( unsigned int i = 0; i < lCopy->GetElementArray().size(); i++ )
        {
        o1->GetElementArray().push_back
          (this->GetElementWithGlobalNumber((lCopy->GetElementArray()[i])->GetGlobalNumber() ).GetPointer() );
        }

      int dim = lCopy->GetForce().size();
      o1->GetForce().set_size(dim);
      for( int i = 0; i < dim; i++ )
        {
        o1->GetForce()[i] = lCopy->GetForce()[i];
        }
      this->AddNextLoad( o1 );
      }
    }

}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::FinalizeMesh()
{
  this->GenerateGFN();
  this->GenerateMFC();
}

/**
 * Assign a global freedom number to each DOF in a system.
 */
template <unsigned int VDimension>
void
FEMObject<VDimension>
::GenerateMFC()
{
  if( m_NGFN <= 0 )
    {
    return;
    }

  m_NMFC = 0;  // reset number of MFC in a system

  /**
   * Before we can start the assembly procedure, we need to know,
   * how many boundary conditions if form of MFCs are there in a system.
   */

  // search for MFC's in Loads array, because they affect the master stiffness
  // matrix
  int numLoads = this->m_LoadContainer->Size();
  for( int l = 0; l < numLoads; l++ )
    {
    LoadBCMFC *l1 = dynamic_cast<LoadBCMFC *>( this->GetLoad(l).GetPointer() );
    if( l1 != ITK_NULLPTR )
      {
      // store the index of an LoadBCMFC object for later
      l1->SetIndex(m_NMFC);

      // increase the number of MFC
      m_NMFC++;
      }
    }
}

/**
 * Assign a global freedom number to each DOF in a system.
 */
template <unsigned int VDimension>
void
FEMObject<VDimension>
::GenerateGFN()
{
  // Clear the list of elements and global freedom numbers in nodes
  // FIXME: should be removed once Mesh is there
  int numNodes = this->m_NodeContainer->Size();
  for( int n = 0; n < numNodes; n++ )
    {
    Element::Node::Pointer np = this->GetNode(n);
    np->m_elements.clear();
    np->ClearDegreesOfFreedom();
    }

  int numElements = this->m_ElementContainer->Size();
  for( int e = 0; e < numElements; e++ )  // step over
  // all
  // elements
    {
    // Add the elemens in the nodes list of elements
    // FIXME: should be removed once Mesh is there
    Element::Pointer el = this->GetElement(e);
    unsigned int     Npts = el->GetNumberOfNodes();
    for( unsigned int pt = 0; pt < Npts; pt++ )
      {
      el->GetNode(pt)->m_elements.insert(el);
      }
    }

  /**
   * Assign new ID to every DOF in a system
   */

  // Start numbering DOFs from 0
  m_NGFN = 0;
  // Step over all elements
  for( int e = 0; e < numElements; e++ )
    {
    // FIXME: Write a code that checks if two elements are compatible, when they
    // share a node
    Element::Pointer el = GetElement(e);
    for( unsigned int n = 0; n < el->GetNumberOfNodes(); n++ )
      {
      for( unsigned int dof = 0; dof < el->GetNumberOfDegreesOfFreedomPerNode(); dof++ )
        {
        if( el->GetNode(n)->GetDegreeOfFreedom(dof) == Element::InvalidDegreeOfFreedomID )
          {
          el->GetNode(n)->SetDegreeOfFreedom(dof, m_NGFN);
          m_NGFN++;
          }
        }
      }
    } // end for e

  //  m_NGFN=Element::GetGlobalDOFCounter()+1;
  if( m_NGFN > 0 )
    {
    return;            // if we got 0 DOF, somebody forgot to define the
                       // system...
    }
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::RenumberNodeContainer()
{

  int numNodes = this->m_NodeContainer->Size();
  for( int i = 0; i < numNodes; i++ )
    {
    this->GetNode(i)->SetGlobalNumber(i);
    }
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::AddNextElement(Element::Pointer e)
{
  ElementIdentifier size = this->m_ElementContainer->Size();

  this->m_ElementContainer->InsertElement(size, e);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::InsertElement(Element::Pointer e, ElementIdentifier index)
{
  this->m_ElementContainer->InsertElement(index, e);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::AddNextNode(Element::Node::Pointer e)
{
  NodeIdentifier size = this->m_NodeContainer->Size();

  this->m_NodeContainer->InsertElement(size, e);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::InsertNode(Element::Node::Pointer e, NodeIdentifier index)
{
  this->m_NodeContainer->InsertElement(index, e);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::AddNextMaterialInternal(Material *e)
{
  MaterialIdentifier size = this->m_MaterialContainer->Size();
  Material::Pointer m(e);
  this->m_MaterialContainer->InsertElement(size, m);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::InsertMaterial(Material::Pointer e, MaterialIdentifier index)
{
  this->m_MaterialContainer->InsertElement(index, e);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::AddNextLoadInternal(Load *e)
{
  Load::Pointer l(e);

  LoadIdentifier size = this->m_LoadContainer->Size();

  this->m_LoadContainer->InsertElement(size, l);
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::InsertLoad(Load::Pointer e, LoadIdentifier index)
{
  this->m_LoadContainer->InsertElement(index, e);
}

template <unsigned int VDimension>
Element::ConstPointer
FEMObject<VDimension>
::GetElement(ElementIdentifier index) const
{
  return this->m_ElementContainer->GetElement(index).GetPointer();
}

template <unsigned int VDimension>
Element::Pointer
FEMObject<VDimension>
::GetElement(ElementIdentifier index)
{
  return this->m_ElementContainer->GetElement(index);
}

template <unsigned int VDimension>
Element::ConstPointer
FEMObject<VDimension>
::GetElementWithGlobalNumber(int globalNumber) const
{
  int numElements = this->m_ElementContainer->Size();
  for( int i = 0; i < numElements; i++ )
    {
    if( this->m_ElementContainer->GetElement(i)->GetGlobalNumber() == globalNumber )
      {
      return this->m_ElementContainer->GetElement(i).GetPointer();
      }
    }
  return ITK_NULLPTR;
}

template <unsigned int VDimension>
Element::Pointer
FEMObject<VDimension>
::GetElementWithGlobalNumber(int globalNumber)
{
  return const_cast<Element *>
    (const_cast<const Self *>(this)->GetElementWithGlobalNumber(globalNumber).GetPointer());
}

template <unsigned int VDimension>
Element::Node::ConstPointer
FEMObject<VDimension>
::GetNode(NodeIdentifier index) const
{
  return this->m_NodeContainer->GetElement(index).GetPointer();
}
template <unsigned int VDimension>
Element::Node::Pointer
FEMObject<VDimension>
::GetNode(NodeIdentifier index)
{
  return this->m_NodeContainer->GetElement(index);
}

template <unsigned int VDimension>
Element::Node::Pointer
FEMObject<VDimension>
::GetNodeWithGlobalNumber(int globalNumber)
{
  int numNodes = this->m_NodeContainer->Size();
  for( int i = 0; i < numNodes; i++ )
    {
    if( this->m_NodeContainer->GetElement(i)->GetGlobalNumber() == globalNumber )
      {
      return this->m_NodeContainer->GetElement(i);
      }
    }
  return ITK_NULLPTR;
}

template <unsigned int VDimension>
Load::ConstPointer
FEMObject<VDimension>
::GetLoad(LoadIdentifier index) const
{
  return this->m_LoadContainer->GetElement(index).GetPointer();
}
template <unsigned int VDimension>
Load::Pointer
FEMObject<VDimension>
::GetLoad(LoadIdentifier index)
{
  return this->m_LoadContainer->GetElement(index);
}

template <unsigned int VDimension>
Load::Pointer
FEMObject<VDimension>
::GetLoadWithGlobalNumber(int globalNumber)
{
  int numLoads = this->m_LoadContainer->Size();
  for( int i = 0; i < numLoads; i++ )
    {
    if( this->m_LoadContainer->GetElement(i)->GetGlobalNumber() == globalNumber )
      {
      return this->m_LoadContainer->GetElement(i);
      }
    }
  return ITK_NULLPTR;
}

template <unsigned int VDimension>
Material::ConstPointer
FEMObject<VDimension>
::GetMaterial(MaterialIdentifier index) const
{
  return this->m_MaterialContainer->GetElement(index).GetPointer();
}

template <unsigned int VDimension>
Material::Pointer
FEMObject<VDimension>
::GetMaterial(MaterialIdentifier index)
{
  return const_cast<Material *>(const_cast<const Self *>(this)->GetMaterial(index).GetPointer());
}

template <unsigned int VDimension>
Material::ConstPointer
FEMObject<VDimension>
::GetMaterialWithGlobalNumber(int globalNumber) const
{
  int numMaterials = this->m_MaterialContainer->Size();
  for( int i = 0; i < numMaterials; i++ )
    {
    if( this->m_MaterialContainer->GetElement(i)->GetGlobalNumber() == globalNumber )
      {
      return this->m_MaterialContainer->GetElement(i).GetPointer();
      }
    }
  return ITK_NULLPTR;
}

template <unsigned int VDimension>
Material::Pointer
FEMObject<VDimension>
::GetMaterialWithGlobalNumber(int globalNumber)
{
  return const_cast<Material *>
    (const_cast<const Self *>(this)->GetMaterialWithGlobalNumber(globalNumber).GetPointer());
}

template <unsigned int VDimension>
void
FEMObject<VDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NGFN: " << this->m_NGFN << std::endl;
  os << indent << "NMFC: " << this->m_NMFC << std::endl;

  os << indent << "ElementContainer: " << this->m_ElementContainer << std::endl;
  os << indent << "NodeContainer: " << this->m_NodeContainer << std::endl;
  os << indent << "LoadContainer: " << this->m_LoadContainer << std::endl;
  os << indent << "MaterialContainer: " << this->m_MaterialContainer << std::endl;
}

}
} // end namespace itk::fem
#endif // itkFEMObject_hxx
