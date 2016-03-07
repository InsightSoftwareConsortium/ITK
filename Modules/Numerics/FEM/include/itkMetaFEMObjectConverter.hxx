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
#ifndef itkMetaFEMObjectConverter_hxx
#define itkMetaFEMObjectConverter_hxx

#include "itkMetaFEMObjectConverter.h"

#include "itkFEMElementBase.h"
#include "itkFEMLightObject.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMLoadGrav.h"
#include "itkObjectFactoryBase.h"

namespace itk
{

/** Constructor */
template <unsigned int NDimensions>
MetaFEMObjectConverter<NDimensions>
::MetaFEMObjectConverter()
{
}

template< unsigned int NDimensions >
typename MetaFEMObjectConverter< NDimensions >::MetaObjectType *
MetaFEMObjectConverter< NDimensions>
::CreateMetaObject()
{
  return dynamic_cast<MetaObjectType *>(new FEMObjectMetaObjectType);
}

/** Convert a metaFEMObject into an FEMObject SpatialObject  */
template <unsigned int NDimensions>
typename MetaFEMObjectConverter<NDimensions>::SpatialObjectPointer
MetaFEMObjectConverter<NDimensions>
::MetaObjectToSpatialObject(const MetaObjectType * mo)
{
  const MetaFEMObject *FEMmo = dynamic_cast<const MetaFEMObject *>(mo);
  if(FEMmo == ITK_NULLPTR)
    {
    itkExceptionMacro(<< "Can't convert MetaObject to MetaFEMObject");
    }

  FEMObjectSpatialObjectPointer FEMSO = FEMObjectSpatialObjectType::New();

  typedef fem::FEMObject<NDimensions>     FEMObjectType;
  typedef typename FEMObjectType::Pointer FEMObjectPointer;

  FEMObjectPointer myFEMObject = FEMObjectType::New();

  // copy all the node information
  typedef typename MetaFEMObject::NodeListType NodeListType;
  const NodeListType nodelist = FEMmo->GetNodeList();

  typename NodeListType::const_iterator it_nodes = nodelist.begin();

  while(it_nodes != nodelist.end())
  {
    FEMObjectNode *node = (*it_nodes);

    // create a new object of the correct class
    //a = FEMOF::Create(clID);
    fem::Element::Node::Pointer o1 = fem::Element::Node::New();
    o1->SetGlobalNumber(node->m_GN);
    fem::Element::VectorType pt(node->m_Dim);
    for (unsigned int i=0; i<node->m_Dim; i++)
    {
    pt[i] = node->m_X[i];
    }
    o1->SetCoordinates(pt);
    myFEMObject->AddNextNode(o1);
    it_nodes++;
  }

   // copy all the material information
   // as of now linear elastic material property is the only
   // material property type. other types could be added in the
   // future.
  typedef typename MetaFEMObject::MaterialListType MaterialListType;
  const MaterialListType materiallist = FEMmo->GetMaterialList();

  typename MaterialListType::const_iterator it_material = materiallist.begin();

  while(it_material != materiallist.end())
  {
    FEMObjectMaterial *material = (*it_material);

    fem::MaterialLinearElasticity::Pointer o1 =
      fem::MaterialLinearElasticity::New();
    o1->SetGlobalNumber(material->m_GN);
    o1->SetYoungsModulus(material->E); /* Young modulus */
    o1->SetPoissonsRatio(material->nu);
    o1->SetCrossSectionalArea(material->A);   /* Crossection area */
    o1->SetMomentOfInertia(material->I);    /* Moment of inertia */
    o1->SetThickness(material->h);
    o1->SetDensityHeatProduct(material->RhoC);
    myFEMObject->AddNextMaterial(o1);
    it_material++;
  }

    // copy all the Element information
  typedef typename MetaFEMObject::ElementListType ElementListType;
  const ElementListType elementlist = FEMmo->GetElementList();

  typename ElementListType::const_iterator it_elements = elementlist.begin();

  while(it_elements != elementlist.end())
  {
    FEMObjectElement *element = (*it_elements);
    itk::LightObject::Pointer a =
      ObjectFactoryBase::CreateInstance ( element->m_ElementName );
    a->UnRegister();
    fem::Element::Pointer o1 =  dynamic_cast<fem::Element *>(a.GetPointer());

    o1->SetGlobalNumber(element->m_GN);
    int numNodes = element->m_NumNodes;
    for (int i=0; i<numNodes; i++)
      {
      o1->SetNode(i, myFEMObject->GetNodeWithGlobalNumber(element->m_NodesId[i]));
      }
    o1->SetMaterial( myFEMObject->GetMaterialWithGlobalNumber(element->m_MaterialGN).GetPointer() );
    myFEMObject->AddNextElement( o1 );
    it_elements++;
  }

    // copy all the load and boundary condition information
  typedef typename MetaFEMObject::LoadListType LoadListType;
  const LoadListType loadlist = FEMmo->GetLoadList();

  typename LoadListType::const_iterator it_load = loadlist.begin();

   while(it_load != loadlist.end())
     {
     FEMObjectLoad *load = (*it_load);

     std::string loadname = std::string(load->m_LoadName);
     if(loadname == "LoadNode")
       {
       fem::LoadNode::Pointer o1 =
         fem::LoadNode::New();
       o1->SetGlobalNumber(load->m_GN);

       o1->SetElement(myFEMObject->GetElementWithGlobalNumber(load->m_ElementGN));

       o1->SetNode(load->m_NodeNumber);

       int dim = load->m_Dim;
       vnl_vector< double > F(dim);
       for (int i=0; i<dim; i++)
         {
         F[i] = load->m_ForceVector[i];
         }
       o1->SetForce(F);
       myFEMObject->AddNextLoad( o1 );
       }
     else if(loadname == "LoadBC")
       {
       fem::LoadBC::Pointer o1 =
         fem::LoadBC::New();
       o1->SetGlobalNumber(load->m_GN);

       o1->SetDegreeOfFreedom(load->m_DOF);

       o1->SetElement(myFEMObject->GetElementWithGlobalNumber(load->m_ElementGN));

       int numRHS = load->m_NumRHS;
       vnl_vector< double > F(numRHS);
       for (int i=0; i<numRHS; i++)
         {
         F[i] = load->m_RHS[i];
         }
       o1->SetValue(F);
       myFEMObject->AddNextLoad( o1 );
       }
     else if(loadname == "LoadBCMFC")
       {
       fem::LoadBCMFC::Pointer o1 =
         fem::LoadBCMFC::New();
       o1->SetGlobalNumber(load->m_GN);

       int NumLHS;
       int elementGN;
       int DOF;
       float Value;
       NumLHS = load->m_NumLHS;

       for ( int i = 0; i < NumLHS; i++ )
         {
         FEMObjectMFCTerm *mfcTerm =
           dynamic_cast< FEMObjectMFCTerm * > (load->m_LHS[i]);
         elementGN = mfcTerm->m_ElementGN;

         DOF = mfcTerm->m_DOF;

         Value = mfcTerm->m_Value;
         o1->GetLeftHandSideArray().push_back(fem::LoadBCMFC::MFCTerm(myFEMObject->GetElementWithGlobalNumber(elementGN).GetPointer(), DOF, Value) );
         }

       int NumRHS = load->m_NumRHS;

       for (int i=0; i<NumRHS; i++)
         {
         o1->GetRightHandSideArray().set_size(o1->GetRightHandSideArray().size() + 1);
         o1->GetRightHandSideArray().put(o1->GetRightHandSideArray().size() - 1, load->m_RHS[i]);
         }

       myFEMObject->AddNextLoad( o1 );
       }
     else if(loadname == "LoadEdge")
       {
       fem::LoadEdge::Pointer o1 =
         fem::LoadEdge::New();
       o1->SetGlobalNumber(load->m_GN);

       int numRows;

       o1->AddNextElement(myFEMObject->GetElementWithGlobalNumber(load->m_ElementGN));
       o1->SetGlobalNumber(load->m_GN);
       o1->SetEdge(load->m_EdgeNumber);

       METAIO_STL::vector< METAIO_STL::vector<float> > force = load->m_ForceMatrix;

       numRows = static_cast<int>( force.size() );
       if(numRows)
         {
         METAIO_STL::vector<float> forcevector = force[0];
         int numCols = static_cast<int>( forcevector.size() );
         o1->GetForce().set_size(numRows, numCols);
         for ( int i = 0; i < numRows; i++ )
           {
           forcevector = force[i];
           for ( int j = 0; j < numCols; j++ )
             {
             o1->GetForce()[i][j] = forcevector[j];
             }
           }
         myFEMObject->AddNextLoad( o1 );
         }
       }
     else if(loadname == "LoadGravConst")
       {
       fem::LoadGravConst::Pointer o1 =
         fem::LoadGravConst::New();
       o1->SetGlobalNumber(load->m_GN);

       for (int i=0; i<load->m_NumElements; i++)
         {
         o1->GetElementArray().push_back(myFEMObject->GetElementWithGlobalNumber(load->m_Elements[i]).GetPointer());
         }

       o1->GetForce().set_size(load->m_Dim);
       for(int i=0; i<load->m_Dim; i++)
         {
         o1->GetForce()[i] = load->m_ForceVector[i];
         }
       myFEMObject->AddNextLoad( o1 );
       }
     else if(loadname == "LoadLandmark")
       {
       fem::LoadLandmark::Pointer o1 =
         fem::LoadLandmark::New();
       o1->SetGlobalNumber(load->m_GN);
       o1->SetEta(load->m_Variance);
       o1->GetElementArray().resize(1);

       int dim = static_cast<int>( load->m_Undeformed.size() );
       vnl_vector<double> source;
       vnl_vector<double> target;
       vnl_vector<double> point;
       vnl_vector<double> force;

       source.set_size(dim);
       target.set_size(dim);
       point.set_size(dim);
       force.set_size(dim);
       for (int i=0; i<dim; i++)
         {
         source[i] = load->m_Deformed[i];
         target[i] = load->m_Undeformed[i];
         point[i]  = load->m_Deformed[i];
         force[i] = load->m_Undeformed[i] - load->m_Deformed[i];

         }
       //FIXME - Check Source and Target
       o1->SetSource( source );
       o1->SetTarget( target );
       o1->SetPoint( point );
       o1->SetForce( force );

       /*
      o1->GetSource().set_size(dim);
      o1->GetPoint().set_size(dim);
      o1->GetTarget().set_size(dim);
      o1->GetForce().set_size(dim);

      for (int i=0; i<dim; i++)
      {
      o1->GetSource()[i] = load->m_Deformed[i];
      o1->GetPoint()[i] = load->m_Deformed[i];
      o1->GetTarget()[i] = load->m_Undeformed[i];
      o1->GetForce()[i] = load->m_Undeformed[i] - load->m_Deformed[i];
      }
      */
       myFEMObject->AddNextLoad( o1 );
       }
     it_load++;
     }

  FEMSO->SetFEMObject(myFEMObject);

  return FEMSO.GetPointer();
}

/** Convert an FEMObject SpatialObject into a metaFEMObject */
template <unsigned int NDimensions>
typename MetaFEMObjectConverter<NDimensions>::MetaObjectType *
MetaFEMObjectConverter<NDimensions>
::SpatialObjectToMetaObject(const SpatialObjectType * so)
{
  FEMObjectSpatialObjectConstPointer FEMSO =
    dynamic_cast<const FEMObjectSpatialObjectType *>(so);
  if(FEMSO.IsNull())
    {
    itkExceptionMacro(<< "Can't downcast SpatialObject to FEMObjectSpatialObject");
    }

  typedef fem::FEMObject<NDimensions>          FEMObjectType;
  typedef typename FEMObjectType::ConstPointer FEMObjectConstPointer;

  FEMObjectConstPointer curFEMObject = FEMSO->GetFEMObject();

  FEMObjectMetaObjectType * FEMmo = new MetaFEMObject(NDimensions);

  // copy the relevant info from spatial object to femobject

  // copy node info.
  const int numSONodes = curFEMObject->GetNumberOfNodes();
  for (int i=0; i<numSONodes; i++)
  {
  FEMObjectNode *Node = new FEMObjectNode(NDimensions);
  fem::Element::Node::ConstPointer SONode = curFEMObject->GetNode(i);
  fem::Element::VectorType pt = SONode->GetCoordinates();

  Node->m_GN = SONode->GetGlobalNumber();
  for (unsigned int j=0; j<NDimensions; j++)
  {
    Node->m_X[j] = pt[j];
  }
  FEMmo->GetNodeList().push_back(Node);
  }

   // copy material info.
   int numMaterial = curFEMObject->GetNumberOfMaterials();
   for (int i=0; i<numMaterial; i++)
   {
     fem::Material::ConstPointer SOMaterial = curFEMObject->GetMaterial(i);
  FEMObjectMaterial *Material = new FEMObjectMaterial;

  // check for the material type
  std::string mat_name = SOMaterial->GetNameOfClass();
  if(mat_name == "MaterialLinearElasticity")
  {
      strcpy(Material->m_MaterialName, mat_name.c_str());
    fem::MaterialLinearElasticity::ConstPointer SOMaterialCast =
      dynamic_cast<const fem::MaterialLinearElasticity * >( SOMaterial.GetPointer() );

    Material->m_GN = SOMaterialCast->GetGlobalNumber();
    Material->E = SOMaterialCast->GetYoungsModulus();
    Material->A = SOMaterialCast->GetCrossSectionalArea();
    Material->I = SOMaterialCast->GetMomentOfInertia();
    Material->nu = SOMaterialCast->GetPoissonsRatio();
    Material->h = SOMaterialCast->GetThickness();
    Material->RhoC = SOMaterialCast->GetDensityHeatProduct();
    FEMmo->GetMaterialList().push_back(Material);
  }
   }

   // copy element info.
  const int numElements = curFEMObject->GetNumberOfElements();
  for (int i=0; i<numElements; i++)
  {
    fem::Element::ConstPointer SOElement = curFEMObject->GetElement(i);
    const int numNodes = SOElement->GetNumberOfNodes();
  FEMObjectElement *Element = new FEMObjectElement(numNodes);

  Element->m_GN = SOElement->GetGlobalNumber();
  Element->m_Dim = NDimensions;
  Element->m_NumNodes = numNodes;

  std::string element_name = SOElement->GetNameOfClass();
  strcpy(Element->m_ElementName, element_name.c_str());
  Element->m_MaterialGN = SOElement->GetMaterial()->GetGlobalNumber();
  for (int j=0; j<numNodes; j++)
  {
    Element->m_NodesId[j] = SOElement->GetNode(j)->GetGlobalNumber();
  }
  FEMmo->GetElementList().push_back(Element);
  }

  // copy load/bc info.
    int numLoads = curFEMObject->GetNumberOfLoads();
   for (int ll=0; ll<numLoads; ++ll)
     {
     fem::Load::ConstPointer SOLoad = curFEMObject->GetLoad(ll);
     FEMObjectLoad *Load = new FEMObjectLoad;

     // check for the load/bc type
     std::string load_name = SOLoad->GetNameOfClass();
     strcpy(Load->m_LoadName, load_name.c_str());
     if(load_name == "LoadNode")
       {
       fem::LoadNode::ConstPointer SOLoadCast =
         dynamic_cast<const fem::LoadNode * >( SOLoad.GetPointer() );

       Load->m_GN = SOLoadCast->GetGlobalNumber();
       Load->m_ElementGN = SOLoadCast->GetElement()->GetGlobalNumber();
       Load->m_NodeNumber = SOLoadCast->GetNode();

       int dim = SOLoadCast->GetForce().size();
       Load->m_ForceVector.resize(dim);
       Load->m_Dim = dim;
       for (int j=0; j<dim; j++)
         {
         Load->m_ForceVector[j] = SOLoadCast->GetForce()[j];
         }
       FEMmo->GetLoadList().push_back(Load);
       }
     else if(load_name == "LoadBC")
       {
       fem::LoadBC::ConstPointer SOLoadCast =
         dynamic_cast<const fem::LoadBC * >( SOLoad.GetPointer() );

       Load->m_GN = SOLoadCast->GetGlobalNumber();
       Load->m_DOF = SOLoadCast->GetDegreeOfFreedom();
       Load->m_ElementGN = SOLoadCast->GetElement()->GetGlobalNumber();

       int numRHS = SOLoadCast->GetValue().size();
       Load->m_RHS.resize(numRHS);
       Load->m_NumRHS = numRHS;
       for (int j=0; j<numRHS; j++)
         {
         Load->m_RHS[j] = SOLoadCast->GetValue()[j];
         }
       FEMmo->GetLoadList().push_back(Load);
       }
     else if(load_name == "LoadBCMFC")
       {
       int elementGN;
       int DOF;
       float Value;

       fem::LoadBCMFC::ConstPointer SOLoadCast =
         dynamic_cast<const fem::LoadBCMFC * >( SOLoad.GetPointer() );

       Load->m_GN = SOLoadCast->GetGlobalNumber();

       Load->m_NumLHS = SOLoadCast->GetNumberOfLeftHandSideTerms();

       for ( int i = 0; i < Load->m_NumLHS; i++ )
         {
         /** set the global number of element that we're applying the load to */
         elementGN = SOLoadCast->GetLeftHandSideTerm(i).m_element->GetGlobalNumber();

         /** set the dof within that element */
         DOF = SOLoadCast->GetLeftHandSideTerm(i).dof;

         /** set weight */
         Value = SOLoadCast->GetLeftHandSideTerm(i).value;

         /** add a new MFCTerm to the lhs */
         FEMObjectMFCTerm *mfcTerm = new FEMObjectMFCTerm(elementGN, DOF, Value);
         Load->m_LHS.push_back(mfcTerm);
         }

       /** set the rhs */
       Load->m_NumRHS = SOLoadCast->GetNumberOfRightHandSideTerms();
       Load->m_RHS.resize(Load->m_NumRHS);
       for (int i=0; i<Load->m_NumRHS; i++)
         {
         Load->m_RHS[i] = SOLoadCast->GetRightHandSideTerm(i);
         }
       FEMmo->GetLoadList().push_back(Load);
       }
     else if(load_name == "LoadEdge")
       {
       fem::LoadEdge::ConstPointer SOLoadCast =
         dynamic_cast<const fem::LoadEdge * >( SOLoad.GetPointer() );


       Load->m_GN = SOLoadCast->GetGlobalNumber();

       Load->m_ElementGN = SOLoadCast->GetElementArray()[0]->GetGlobalNumber();
       Load->m_EdgeNumber = SOLoadCast->GetEdge();

       vnl_matrix< fem::Element::Float > force = SOLoadCast->GetForce();

       const int numRows = force.rows();
       const int numCols = force.columns();

       for ( int i = 0; i < numRows; i++ )
         {
         METAIO_STL::vector<float> F(numCols);
         for ( int j = 0; j < numCols; j++ )
           {
           F[j] = force[i][j];
           }
         Load->m_ForceMatrix.push_back(F);
         }
       FEMmo->GetLoadList().push_back(Load);
       }
     else if(load_name == "LoadGravConst")
       {
       fem::LoadGravConst::ConstPointer SOLoadCast =
         dynamic_cast<const fem::LoadGravConst * >( SOLoad.GetPointer() );

       Load->m_GN = SOLoadCast->GetGlobalNumber();

       const int numLoadElements  = static_cast<const int>( SOLoadCast->GetElementArray().size() );
       Load->m_NumElements = numLoadElements;
       for (int i=0; i<numLoadElements; i++)
         {
         const int elementGN = SOLoadCast->GetElementArray()[i]->GetGlobalNumber();
         Load->m_Elements.push_back(elementGN);
         }

       Load->m_Dim = SOLoadCast->GetForce().size();
       for (int i=0; i<Load->m_Dim; i++)
         {
         Load->m_ForceVector.push_back(SOLoadCast->GetForce()[i]);
         }

       FEMmo->GetLoadList().push_back(Load);
       }
     else if(load_name == "LoadLandmark")
       {
       fem::LoadLandmark::ConstPointer SOLoadCast =
         dynamic_cast<const fem::LoadLandmark * >( SOLoad.GetPointer() );

       Load->m_GN = SOLoadCast->GetGlobalNumber();

       Load->m_Variance = SOLoadCast->GetEta();

       const int dim = SOLoadCast->GetSource().size();

       Load->m_Undeformed.resize(dim);
       Load->m_Deformed.resize(dim);

       for (int i=0; i<dim; i++)
         {
         Load->m_Deformed[i] = SOLoadCast->GetSource()[i];
         Load->m_Undeformed[i] = SOLoadCast->GetTarget()[i];
         }
       FEMmo->GetLoadList().push_back(Load);
       }
     }
  return FEMmo;
}


} // end namespace itk


#endif
