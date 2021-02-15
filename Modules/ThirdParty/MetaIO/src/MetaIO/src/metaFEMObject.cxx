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

#include "metaFEMObject.h"

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE
{
#endif

FEMObjectNode::FEMObjectNode(int dim)
{
  m_Dim = static_cast<unsigned int>(dim);
  m_GN = -1;
  m_X = new float[m_Dim];
  for (unsigned int i = 0; i < m_Dim; i++)
  {
    m_X[i] = 0;
  }
}

FEMObjectNode::~FEMObjectNode()
{
  delete[] m_X;
}

FEMObjectElement::FEMObjectElement(int dim)
{
  m_Dim = static_cast<unsigned int>(dim);
  m_GN = -1;
  m_NodesId = new int[m_Dim];
  for (unsigned int i = 0; i < m_Dim; i++)
  {
    m_NodesId[i] = -1;
  }
}

FEMObjectElement::~FEMObjectElement()
{
  delete[] m_NodesId;
}

FEMObjectLoad::FEMObjectLoad() = default;

FEMObjectLoad::~FEMObjectLoad()
{
  for (auto & it : this->m_LHS)
  {
    delete it;
  }
  this->m_LHS.clear();
  this->m_RHS.clear();
  this->m_ForceMatrix.clear();
  this->m_ForceVector.clear();
}

//
// MetaFEMObject Constructors
//
MetaFEMObject::MetaFEMObject()
  : MetaObject()
{
  META_DEBUG_PRINT( "MetaFEMObject()" );

  MetaFEMObject::Clear();

  this->m_ClassNameList.push_back("Node");
  this->m_ClassNameList.push_back("MaterialLinearElasticity");
  this->m_ClassNameList.push_back("Element2DC0LinearLineStress");
  this->m_ClassNameList.push_back("Element2DC1Beam");
  this->m_ClassNameList.push_back("Element2DC0LinearTriangularMembrane");
  this->m_ClassNameList.push_back("Element2DC0LinearTriangularStrain");
  this->m_ClassNameList.push_back("Element2DC0LinearTriangularStress");
  this->m_ClassNameList.push_back("Element2DC0LinearQuadrilateralMembrane");
  this->m_ClassNameList.push_back("Element2DC0LinearQuadrilateralStrain");
  this->m_ClassNameList.push_back("Element2DC0LinearQuadrilateralStress");
  this->m_ClassNameList.push_back("Element2DC0QuadraticTriangularStress");
  this->m_ClassNameList.push_back("Element2DC0QuadraticTriangularStrain");
  this->m_ClassNameList.push_back("Element3DC0LinearHexahedronMembrane");
  this->m_ClassNameList.push_back("Element3DC0LinearHexahedronStrain");
  this->m_ClassNameList.push_back("Element3DC0LinearTetrahedronMembrane");
  this->m_ClassNameList.push_back("Element3DC0LinearTetrahedronStrain");
  this->m_ClassNameList.push_back("LoadBC");
  this->m_ClassNameList.push_back("LoadBCMFC");
  this->m_ClassNameList.push_back("LoadNode");
  this->m_ClassNameList.push_back("LoadEdge");
  this->m_ClassNameList.push_back("LoadGravConst");
  this->m_ClassNameList.push_back("LoadLandmark");
  this->m_ClassNameList.push_back("LoadPoint");
  this->m_ElementDataFileName = "LOCAL";
}

//
MetaFEMObject::MetaFEMObject(const char * _headerName)
  : MetaObject()
{
  META_DEBUG_PRINT( "MetaFEMObject()" );
  MetaFEMObject::Clear();
  MetaFEMObject::Read(_headerName);
  this->m_ElementDataFileName = "LOCAL";
}

//
MetaFEMObject::MetaFEMObject(const MetaFEMObject * _mesh)
  : MetaObject()
{
  META_DEBUG_PRINT( "MetaFEMObject()" );
  MetaFEMObject::Clear();
  MetaFEMObject::CopyInfo(_mesh);
}


//
MetaFEMObject::MetaFEMObject(unsigned int dim)
  : MetaObject(dim)
{
  META_DEBUG_PRINT( "MetaFEMObject()" );
  MetaFEMObject::Clear();
  this->m_ElementDataFileName = "LOCAL";
}

/** Destructor */
MetaFEMObject::~MetaFEMObject()
{
  // Delete the list of pointers to Nodes.
  auto it_Node = m_NodeList.begin();
  while (it_Node != m_NodeList.end())
  {
    FEMObjectNode * Node = *it_Node;
    ++it_Node;
    delete Node;
  }
  // Delete the list of pointers to Materials.
  auto it_Material = m_MaterialList.begin();
  while (it_Material != m_MaterialList.end())
  {
    FEMObjectMaterial * Material = *it_Material;
    ++it_Material;
    delete Material;
  }

  // Delete the list of pointers to Elements.
  auto it_Element = m_ElementList.begin();
  while (it_Element != m_ElementList.end())
  {
    FEMObjectElement * Element = *it_Element;
    ++it_Element;
    delete Element;
  }

  // Delete the list of pointers to Loads.
  auto it_Load = m_LoadList.begin();
  while (it_Load != m_LoadList.end())
  {
    FEMObjectLoad * Load = *it_Load;
    ++it_Load;
    delete Load;
  }

MetaObject::M_Destroy();
}

//
void
MetaFEMObject::PrintInfo() const
{
  MetaObject::PrintInfo();
}

void
MetaFEMObject::CopyInfo(const MetaObject * _object)
{
  MetaObject::CopyInfo(_object);
}

/** Clear FEMObject information */
void
MetaFEMObject::Clear()
{
  META_DEBUG_PRINT( "MetaFEMObject: Clear" );

  MetaObject::Clear();

  strcpy(m_ObjectTypeName, "FEMObject");

  META_DEBUG_PRINT( "MetaFEMObject: Clear: m_NPoints" );

  // Delete the list of pointers to Nodes.
  auto it_Node = m_NodeList.begin();
  while (it_Node != m_NodeList.end())
  {
    FEMObjectNode * Node = *it_Node;
    ++it_Node;
    delete Node;
  }

  // Delete the list of pointers to Elements.
  auto it_Element = m_ElementList.begin();
  while (it_Element != m_ElementList.end())
  {
    FEMObjectElement * Element = *it_Element;
    ++it_Element;
    delete Element;
  }

  // Delete the list of pointers to Loads.
  auto it_Load = m_LoadList.begin();
  while (it_Load != m_LoadList.end())
  {
    FEMObjectLoad * Load = *it_Load;
    ++it_Load;
    delete Load;
  }

  // Delete the list of pointers to Materials.
  auto it_Material = m_MaterialList.begin();
  while (it_Material != m_MaterialList.end())
  {
    FEMObjectMaterial * Material = *it_Material;
    ++it_Material;
    delete Material;
  }

  m_NodeList.clear();
  m_ElementList.clear();
  m_LoadList.clear();
  m_MaterialList.clear();
}

/** Set Read fields */
void
MetaFEMObject::M_SetupReadFields()
{
  META_DEBUG_PRINT( "MetaFEMObject: M_SetupReadFields" );

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ElementDataFile", MET_STRING, true);
  mF->required = true;
  mF->terminateRead = true;
  m_Fields.push_back(mF);
}

void
MetaFEMObject::M_SetupWriteFields()
{
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ElementDataFile", MET_STRING, m_ElementDataFileName.length(), m_ElementDataFileName.c_str());
  mF->terminateRead = true;
  m_Fields.push_back(mF);
}


bool
MetaFEMObject::M_Read()
{
  META_DEBUG_PRINT( "MetaFEMObject: M_Read: Loading Header" );

  if (!MetaObject::M_Read())
  {
    std::cout << "MetaFEMObject: M_Read: Error parsing file" << std::endl;
    return false;
  }

  META_DEBUG_PRINT( "MetaFEMObject: M_Read: Parsing Header" );

  // currently reader handles only ASCII data
  if (m_BinaryData)
  {
    std::cout << "MetaFEMObject: M_Read: Data content should be in ASCII format" << std::endl;
    return false;
  }

  // we read 1)node, 2) material, 3) element and 4) load  and the input should be in the afore
  // mentioned order.

  int segment_read = 0; // to keep track of what is being read. corresponds to enumerated types in the header file
  /* then we start reading objects from stream */
  do
  {
    // local variables
    char                   buf[256];
    std::string            s;
    std::string::size_type b;
    std::string::size_type e;
    std::string            errorMessage;

    if (segment_read > 3)
    {
      this->SkipWhiteSpace();
      return true; // end of FEM segment in spatial object reader.
    }

    this->m_ReadStream->tellg(); // remember the stream position
    this->SkipWhiteSpace();          // skip comments and whitespaces
    if (this->m_ReadStream->eof())
    {
      return false; // end of stream. all was good
    }
    char c;
    if ((c = static_cast<char>(this->m_ReadStream->get())) != '<')
    {
      std::string rest;
      std::getline(*this->m_ReadStream, rest);
      errorMessage = "Expected < token not found. Instead found '";
      errorMessage += c;
      errorMessage += "'.\nRest of line is '";
      errorMessage += rest;
      errorMessage += "'.\n";
      std::cout << errorMessage << std::endl;
      return false; // the file is not in proper format
    }
    this->m_ReadStream->getline(buf, 256, '>'); // read up to 256 characters until '>' is reached.
    // we read and discard the '>'
    s = std::string(buf);

    // get rid of the whitespaces in front of and the back of token
    b = s.find_first_not_of(MetaFEMObject::whitespaces); // end of
    // whitespaces
    // in the
    // beginning
    if ((e = s.find_first_of(MetaFEMObject::whitespaces, b)) == std::string::npos) //
    // beginning
    // of
    // whitespaces
    // at the
    // end
    {
      e = s.size();
    }
    s = s.substr(b, e - b);

    if (s == "END")
    {
      /*
       * We can ignore this token. Start again by reading the next object.
       */
      segment_read++;
    }
    else
    {
      bool clID = this->IsClassNamePresent(s); // obtain the class ID from FEMObjectFactory
      if (!clID)
      {
        errorMessage = s;
        errorMessage += "   is not a valid FEM data type";
        errorMessage += "'.";
        std::cout << errorMessage << std::endl;
        return false; // class not found
      }
      /*
       * Now we have to read additional data, which is
       * specific to the class of object we just created
       */

      switch (segment_read)
      {
        case NODE:
          this->M_Read_Node();
          break;
        case MATERIAL:
          this->M_Read_Material(s);
          break;
        case ELEMENT:
          this->M_Read_Element(s);
          break;
        case LOAD:
          this->M_Read_Load(s);
          break;
        default:
          return false;
      }
    }
  } while (segment_read <= 3); // end of FEM segment in spatial object reader.
  return true;
}

bool
MetaFEMObject::M_Write()
{
  if (!MetaObject::M_Write())
  {
    std::cout << "MetaFEMObject: M_Write: Error parsing file" << std::endl;
    return false;
  }

  auto it_Node = m_NodeList.begin();
  while (it_Node != m_NodeList.end())
  {
    FEMObjectNode * Node = *it_Node;
    this->M_Write_Node(Node);
    ++it_Node;
  }
  *this->m_WriteStream << "\n<END>  % End of nodes\n\n";

  auto it_Material = m_MaterialList.begin();
  while (it_Material != m_MaterialList.end())
  {
    FEMObjectMaterial * Material = *it_Material;
    this->M_Write_Material(Material);
    ++it_Material;
  }
  *this->m_WriteStream << "\n<END>  % End of material definition\n\n";


  auto it_Element = m_ElementList.begin();
  while (it_Element != m_ElementList.end())
  {
    FEMObjectElement * Element = *it_Element;
    this->M_Write_Element(Element);
    ++it_Element;
  }
  *this->m_WriteStream << "\n<END>  % End of element definition\n\n";

  auto it_Load = m_LoadList.begin();
  while (it_Load != m_LoadList.end())
  {
    FEMObjectLoad * Load = *it_Load;
    this->M_Write_Load(Load);
    ++it_Load;
  }
  *this->m_WriteStream << "\n<END>  % End of load definition\n\n";

  return true;
}

void
MetaFEMObject::M_Write_Node(FEMObjectNode * Node)
{
  // first write the class name
  *this->m_WriteStream << '<' << "Node"
                       << ">\n";

  // then the global object number
  *this->m_WriteStream << "\t" << Node->m_GN << "\t% Global object number\n";

  /* write co-ordinate values */
  *this->m_WriteStream << "\t" << Node->m_Dim;
  for (unsigned int i = 0; i < Node->m_Dim; i++)
  {
    *this->m_WriteStream << " " << Node->m_X[i];
  }
  *this->m_WriteStream << "\t% Node coordinates"
                       << "\n";
}

void
MetaFEMObject::M_Write_Material(FEMObjectMaterial * Material)
{
  if (std::string(Material->m_MaterialName) == "MaterialLinearElasticity")
  {
    *this->m_WriteStream << '<' << "MaterialLinearElasticity"
                         << ">\n";
    *this->m_WriteStream << "\t" << Material->m_GN << "\t% Global object number\n";
    *this->m_WriteStream << "\tE  : " << Material->E << "\t% Young modulus\n";
    *this->m_WriteStream << "\tA  : " << Material->A << "\t% Beam crossection area\n";
    *this->m_WriteStream << "\tI  : " << Material->I << "\t% Moment of inertia\n";
    *this->m_WriteStream << "\tnu : " << Material->nu << "\t% Poisson's ratio\n";
    *this->m_WriteStream << "\th : " << Material->h << "\t% Plate thickness\n";
    *this->m_WriteStream << "\tRhoC : " << Material->RhoC << "\t% Density times capacity\n";
    *this->m_WriteStream << "\tEND:\t% End of material definition\n";
  }
}

void
MetaFEMObject::M_Write_Element(FEMObjectElement * Element)
{
  *this->m_WriteStream << '<' << Element->m_ElementName << ">\n";
  *this->m_WriteStream << "\t" << Element->m_GN << "\t% Global object number\n";
  unsigned int numNodes = Element->m_NumNodes;
  for (unsigned int p = 0; p < numNodes; p++)
  {
    *this->m_WriteStream << "\t" << Element->m_NodesId[p] << "\t% Node #" << (p + 1) << " ID\n";
  }
  *this->m_WriteStream << "\t" << Element->m_MaterialGN << "\t% Material ID\n";
}

void
MetaFEMObject::M_Write_Load(FEMObjectLoad * Load)
{
  *this->m_WriteStream << '<' << Load->m_LoadName << ">\n";
  *this->m_WriteStream << "\t" << Load->m_GN << "\t% Global object number\n";

  // write according to the load type
  if (std::string(Load->m_LoadName) == "LoadBC")
  {
    *this->m_WriteStream << "\t" << Load->m_ElementGN << "\t% GN of element"
                         << "\n";
    *this->m_WriteStream << "\t" << Load->m_DOF << "\t% DOF# in element"
                         << "\n";
    *this->m_WriteStream << "\t" << Load->m_NumRHS;
    for (int i = 0; i < Load->m_NumRHS; i++)
    {
      *this->m_WriteStream << " " << Load->m_RHS[i];
    }
    *this->m_WriteStream << "\t% value of the fixed DOF"
                         << "\n";
    return;
  }

  if (std::string(Load->m_LoadName) == "LoadNode")
  {
    *this->m_WriteStream << "\t" << Load->m_ElementGN << "\t% GN of element"
                         << "\n";
    *this->m_WriteStream << "\t" << Load->m_NodeNumber << " "
                         << "\t% Point number within the element\n";
    *this->m_WriteStream << "\t" << Load->m_Dim;
    for (int i = 0; i < Load->m_Dim; i++)
    {
      *this->m_WriteStream << " " << Load->m_ForceVector[i];
    }
    *this->m_WriteStream << "\t% Force vector (first number is the size of a vector)\n";
    return;
  }

  if (std::string(Load->m_LoadName) == "LoadBCMFC")
  {
    /** write the number of DOFs affected by this MFC */
    *this->m_WriteStream << "\t" << Load->m_NumLHS << "\t% Number of DOFs in this MFC" << std::endl;

    /** write each term */
    *this->m_WriteStream << "\t  %==>\n";
    for (int i = 0; i < Load->m_NumLHS; i++)
    {
      auto * mfcTerm = dynamic_cast<FEMObjectMFCTerm *>(&*Load->m_LHS[i]);
      *this->m_WriteStream << "\t  " << mfcTerm->m_ElementGN << "\t% GN of element" << std::endl;
      *this->m_WriteStream << "\t  " << mfcTerm->m_DOF << "\t% DOF# in element" << std::endl;
      *this->m_WriteStream << "\t  " << mfcTerm->m_Value << "\t% weight" << std::endl;
      *this->m_WriteStream << "\t  %==>\n";
    }

    /** write the rhs */
    *this->m_WriteStream << "\t" << Load->m_NumRHS;
    for (int i = 0; i < Load->m_NumRHS; i++)
    {
      *this->m_WriteStream << " " << Load->m_RHS[i];
    }
    *this->m_WriteStream << "\t% rhs of MFC" << std::endl;
    return;
  }

  if (std::string(Load->m_LoadName) == "LoadEdge")
  {
    *this->m_WriteStream << "\t" << Load->m_ElementGN << "\t% GN of the element on which the load acts"
                         << "\n";
    /** ... edge number */
    *this->m_WriteStream << "\t" << Load->m_EdgeNumber << "\t% Edge number"
                         << "\n";

    /** ... force matrix */
    size_t numRows = Load->m_ForceMatrix.size();
    size_t numCols = Load->m_ForceMatrix[0].size();

    *this->m_WriteStream << "\t" << numRows << "\t% # rows in force matrix"
                         << "\n";
    *this->m_WriteStream << "\t" << numCols << "\t% # cols in force matrix"
                         << "\n";
    *this->m_WriteStream << "\t% force matrix\n";
    for (size_t i = 0; i < numRows; i++)
    {
      *this->m_WriteStream << "\t";
      std::vector<float> F = Load->m_ForceMatrix[i];
      for (size_t j = 0; j < numCols; j++)
      {
        *this->m_WriteStream << F[j] << " ";
      }
      *this->m_WriteStream << "\n";
    }
    return;
  }

  if (std::string(Load->m_LoadName) == "LoadGravConst")
  {
    /** Write the list of element global numbers */
    if (Load->m_NumElements > 0)
    {
      *this->m_WriteStream << "\t" << Load->m_NumElements;
      *this->m_WriteStream << "\t% # of elements on which the load acts" << std::endl;
      *this->m_WriteStream << "\t";
      for (int i = 0; i < Load->m_NumElements; i++)
      {
        *this->m_WriteStream << Load->m_Elements[i] << " ";
      }
      *this->m_WriteStream << "\t% GNs of elements" << std::endl;
    }
    else
    {
      *this->m_WriteStream << "\t-1\t% Load acts on all elements" << std::endl;
    }
    /** then write the actual data force vector */
    *this->m_WriteStream << "\t" << Load->m_Dim << "\t% Size of the gravity force vector\n";
    for (int i = 0; i < Load->m_Dim; i++)
    {
      *this->m_WriteStream << "\t" << Load->m_ForceVector[i];
    }
    *this->m_WriteStream << "\t% Gravity force vector\n";
  }

  if (std::string(Load->m_LoadName) == "LoadLandmark")
  {
    // print undeformed coordinates
    size_t dim = Load->m_Undeformed.size();

    *this->m_WriteStream << "\t" << dim;
    for (size_t i = 0; i < dim; i++)
    {
      *this->m_WriteStream << Load->m_Undeformed[i] << " ";
    }
    *this->m_WriteStream << "\t % Dimension , undeformed state local coordinates";
    *this->m_WriteStream << "\n";

    // print deformed coordinates
    *this->m_WriteStream << "\t" << dim;
    for (size_t i = 0; i < dim; i++)
    {
      *this->m_WriteStream << Load->m_Deformed[i] << " ";
    }
    *this->m_WriteStream << "\t % Dimension , deformed state local coordinates";
    *this->m_WriteStream << "\n";

    // print square root of Variance
    *this->m_WriteStream << Load->m_Variance;
    *this->m_WriteStream << "\t % Square root of the landmark variance ";
    *this->m_WriteStream << "\n";
    return;
  }
}

void
MetaFEMObject::SkipWhiteSpace()
{
  std::string skip;

  while (this->m_ReadStream && !this->m_ReadStream->eof() && (std::ws(*this->m_ReadStream).peek()) == '%')
  {
    std::getline(*this->m_ReadStream, skip);
  }
}

bool
MetaFEMObject::IsClassNamePresent(const std::string& c_string)
{
  ClassNameListType::const_iterator it = this->m_ClassNameList.begin();
  while (it != this->m_ClassNameList.end())
  {
    if ((*it) == c_string)
    {
      return true;
    }
    ++it;
  }
  return false;
}

bool
MetaFEMObject::M_Read_Node()
{
  unsigned int n = 0;
  float        coor[3];
  /**
   * First call the parent's read function
   */
  int GN = this->ReadGlobalNumber();

  if (GN == -1)
  {
    std::cout << "Error reading Global Number" << std::endl;
    return false;
  }
  /*
   * Read and set node coordinates
   */
  // read dimensions
  this->SkipWhiteSpace();
  *this->m_ReadStream >> n;
  if (!this->m_ReadStream)
  {
    std::cout << "Error reading Node dimensions" << std::endl;
    return false;
  }
  auto * node = new FEMObjectNode(n);
  node->m_GN = GN;

  this->SkipWhiteSpace();
  for (unsigned int i = 0; i < n; i++)
  {
    *this->m_ReadStream >> coor[i];
    if (!this->m_ReadStream)
    {
      std::cout << "Error reading Node coordinates" << std::endl;
      return false;
    }
    node->m_X[i] = coor[i];
  }
  this->m_NodeList.push_back(node);

  return true;
}

bool
MetaFEMObject::M_Read_Material(const std::string& material_name)
{
  /**
   * First call the parent's read function
   */
  int GN = this->ReadGlobalNumber();

  if (GN == -1)
  {
    std::cout << "Error reading Global Number" << std::endl;
    return false;
  }
  /*
   * Read material properties
   */
  double d = 0;

  std::streampos         l(0);
  char                   buf[256];
  std::string            s;
  std::string::size_type b;
  std::string::size_type e;

  // clear the data already inside the object
  double E = 0.0;
  double A = 0.0;
  double I = 0.0;
  double nu = 0.0;
  double h = 1.0;
  double RhoC = 1.0;

  /*
   * Next we read any known constant from stream. This allows a user to
   * specify only constants which are actually required by elements in
   * a system. This makes creating input files a bit easier.
   */
  while (this->m_ReadStream)
  {
    l = this->m_ReadStream->tellg(); // remember the stream position
    this->SkipWhiteSpace();          // skip comments and whitespaces

    /**
     * All Constants are in the following format:
     *    constant_name : value
     */
    this->m_ReadStream->getline(buf, 256, ':'); // read up to 256 characters until ':' is
                                                // reached. we read and discard the ':'
    if (!this->m_ReadStream)
    {
      std::cout << "Error reading Material properties" << std::endl;
      return false;
    } // no : was found
    s = std::string(buf);

    // Get rid of the whitespaces in front of and the back of token
    b = s.find_first_not_of(whitespaces);                           // end
                                                                    // of
                                                                    // whitespaces
                                                                    // in
                                                                    // the
                                                                    // beginning
    if ((e = s.find_first_of(whitespaces, b)) == std::string::npos) //
                                                                    // beginning
                                                                    // of
                                                                    // whitespaces
                                                                    // at
                                                                    // the
                                                                    // end
    {
      e = s.size();
    }

    /*
     * s now contains just the name of the constant.
     * The value is ready to be read next from the stream
     */
    s = s.substr(b, e - b);

    if (s == "E")
    {
      *this->m_ReadStream >> d;
      if (!this->m_ReadStream)
      {
        std::cout << "Error reading Material E property" << std::endl;
        return false;
      }
      E = d;
      continue;
    }

    if (s == "A")
    {
      *this->m_ReadStream >> d;
      if (!this->m_ReadStream)
      {
        std::cout << "Error reading Material A property" << std::endl;
        return false;
      }
      A = d;
      continue;
    }

    if (s == "I")
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> d;
      if (!this->m_ReadStream)
      {
        std::cout << "Error reading Material I property" << std::endl;
        return false;
      }
      I = d;
      continue;
    }

    if (s == "nu")
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> d;
      if (!this->m_ReadStream)
      {
        std::cout << "Error reading Material nu property" << std::endl;
        return false;
      }
      nu = d;
      continue;
    }

    if (s == "h")
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> d;
      if (!this->m_ReadStream)
      {
        std::cout << "Error reading Material h property" << std::endl;
        return false;
      }
      h = d;
      continue;
    }

    if (s == "RhoC")
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> d;
      if (!this->m_ReadStream)
      {
        std::cout << "Error reading Material RhoC property" << std::endl;
        return false;
      }
      RhoC = d;
      continue;
    }
    if (s == "END")
    {
      // End of constants in material definition
      // store all the material definitions
      auto * material = new FEMObjectMaterial();
      strcpy(material->m_MaterialName, material_name.c_str());
      material->m_GN = GN;
      material->E = E;
      material->A = A;
      material->I = I;
      material->nu = nu;
      material->h = h;
      material->RhoC = RhoC;
      this->m_MaterialList.push_back(material);
      break;
    }

    /**
     * If we got here an unknown constant was reached.
     * We reset the stream position and set the stream error.
     */
    this->m_ReadStream->seekg(l);
    this->m_ReadStream->clear(std::ios::failbit);
  }

  if (!this->m_ReadStream)
  {
    std::cout << "Error reading Material properties" << std::endl;
    return false;
  }
  return true;
}

bool
MetaFEMObject::M_Read_Element(const std::string& element_name)
{
  unsigned int n = 0;
  unsigned int materialGN = 0;
  int          info[2];
  if (MetaFEMObject::GetElementDimensionAndNumberOfNodes(element_name, info) == nullptr)
  {
    std::cout << "Invalid element_name" << std::endl;
    return false;
  }

  int GN = this->ReadGlobalNumber();

  if (GN == -1)
  {
    std::cout << "Error reading Global Number" << std::endl;
    return false;
  }
  /*
   * Read and set element connectivity
   */
  int * NodesId = new int[info[0]];
  for (int p = 0; p < info[0]; p++)
  {
    this->SkipWhiteSpace();
    *this->m_ReadStream >> n;
    if (!this->m_ReadStream)
    {
      delete[] NodesId;
      std::cout << "Error reading Element node numbers" << std::endl;
      return false;
    }
    NodesId[p] = n;
  }

  // read material associated with the element
  this->SkipWhiteSpace();
  *this->m_ReadStream >> materialGN;
  if (!this->m_ReadStream)
  {
    delete[] NodesId;
    std::cout << "Error reading Element global number" << std::endl;
    return false;
  }
  // store the read information
  auto * element = new FEMObjectElement(info[0]);
  element->m_GN = GN;
  for (int p = 0; p < info[0]; p++)
  {
    element->m_NodesId[p] = NodesId[p];
  }
  element->m_MaterialGN = materialGN;
  element->m_NumNodes = static_cast<unsigned int>(info[0]);
  element->m_Dim = static_cast<unsigned int>(info[1]);
  strcpy(element->m_ElementName, element_name.c_str());

  delete[] NodesId;
  this->m_ElementList.push_back(element);
  return true;
}

bool
MetaFEMObject::M_Read_Load(const std::string& load_name)
{
  int GN;
  int elementGN = 0;
  int DOF = 0;
  int NumRHS = 0;
  int NodeNumber = 0;
  int Dim = 0;
  int NumLHS = 0;
  int Value = 0;

  auto * load = new FEMObjectLoad;
  strcpy(load->m_LoadName, load_name.c_str());

  GN = this->ReadGlobalNumber();
  if (GN == -1)
  {
    delete load;
    std::cout << "Error reading Load definition - global number" << std::endl;
    return false;
  }

  load->m_GN = GN;

  if (load_name == "LoadBC")
  {
    /* read and set pointer to element that we're applying the load to */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> elementGN;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading Load definition - Element Global Number" << std::endl;
      return false;
    }
    load->m_ElementGN = elementGN;

    /* read the local DOF number within that element */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> DOF;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading Load definition - Degrees of Freedom" << std::endl;
      return false;
    }
    load->m_DOF = DOF;

    /* read the value to which the DOF is fixed */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> NumRHS;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading Load definition - Number of fixed degrees of freedom" << std::endl;
      return false;
    }
    load->m_NumRHS = NumRHS;
    load->m_RHS.resize(static_cast<unsigned long>(NumRHS));

    for (int i = 0; i < NumRHS; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> load->m_RHS[i];
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading Load definition - Fixed degree of freedom" << std::endl;
        return false;
      }
    }
  }
  else if (load_name == "LoadNode")
  {
    /* read and set pointer to element that we're applying the load to */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> elementGN;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadNode definition - Element Global Number" << std::endl;
      return false;
    }
    load->m_ElementGN = elementGN;

    /* read the value to which the DOF is fixed */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> NodeNumber;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadNode definition - Node Number" << std::endl;
      return false;
    }
    load->m_NodeNumber = NodeNumber;

    this->SkipWhiteSpace();
    *this->m_ReadStream >> Dim;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadNode definition - Dimension" << std::endl;
      return false;
    }
    load->m_Dim = Dim;

    load->m_ForceVector.resize(static_cast<unsigned long>(Dim));
    for (int i = 0; i < Dim; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> load->m_ForceVector[i];
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadNode definition - Force Vector" << std::endl;
        return false;
      }
    }
  }
  else if (load_name == "LoadBCMFC")
  {
    /** read number of terms in lhs of MFC equation */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> NumLHS;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadBCMFC definition - Number of LHS terms" << std::endl;
      return false;
    }

    load->m_NumLHS = NumLHS;
    for (int i = 0; i < NumLHS; i++)
    {
      /** read and set pointer to element that we're applying the load to */
      this->SkipWhiteSpace();
      *this->m_ReadStream >> elementGN;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadBCMFC definition - Element Global Number" << std::endl;
        return false;
      }

      /** read the number of dof within that element */
      this->SkipWhiteSpace();
      *this->m_ReadStream >> DOF;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadBCMFC definition - Element Degree of Freedom" << std::endl;
        return false;
      }

      /** read weight */
      this->SkipWhiteSpace();
      *this->m_ReadStream >> Value;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadBCMFC definition - Weight" << std::endl;
        return false;
      }

      /** add a new MFCTerm to the lhs */
      auto * mfcTerm = new FEMObjectMFCTerm(
        static_cast<unsigned int>(elementGN), static_cast<unsigned int>(DOF), static_cast<float>(Value));
      load->m_LHS.push_back(mfcTerm);
    }

    /** read the rhs */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> NumRHS;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadBCMFC definition - Number of RHS terms" << std::endl;
      return false;
    }

    load->m_NumRHS = NumRHS;
    load->m_RHS.resize(static_cast<unsigned long>(NumRHS));
    for (int i = 0; i < NumRHS; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> load->m_RHS[i];
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadBCMFC definition - RHS Term" << std::endl;
        return false;
      }
    }
  }
  else if (load_name == "LoadEdge")
  {
    int edgeNum = 0;
    int numRows = 0;
    int numCols = 0;

    /* read the global number of the element on which the load acts */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> elementGN;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadEdge definition - Element Global Number" << std::endl;
      return false;
    }
    load->m_ElementGN = elementGN;

    /** ... edge number */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> edgeNum;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadEdge definition - Edge Number" << std::endl;
      return false;
    }
    load->m_EdgeNumber = edgeNum;

    /** ... # of rows */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> numRows;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadEdge definition - Number of Rows" << std::endl;
      return false;
    }

    /** ... # of cols */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> numCols;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadEdge definition - Number of Columns" << std::endl;
      return false;
    }

    for (int i = 0; i < numRows; i++)
    {
      this->SkipWhiteSpace();
      std::vector<float> F(static_cast<unsigned long>(numCols));
      for (int j = 0; j < numCols; j++)
      {
        *this->m_ReadStream >> F[j];
        if (!this->m_ReadStream)
        {
          delete load;
          std::cout << "Error reading LoadEdge definition - Force Matrix" << std::endl;
          return false;
        }
      }
      this->SkipWhiteSpace();
      load->m_ForceMatrix.push_back(F);
    }
  }
  else if (load_name == "LoadGravConst")
  {
    // read in the list of elements on which the load acts
    this->SkipWhiteSpace();
    *this->m_ReadStream >> load->m_NumElements;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadGravConst definition - Number of Elements" << std::endl;
      return false;
    }

    for (int i = 0; i < load->m_NumElements; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> elementGN;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadGravConst definition - Element Global Number" << std::endl;
        return false;
      }
      load->m_Elements.push_back(elementGN);
    }

    /** first read and set the size of the vector */
    this->SkipWhiteSpace();
    *this->m_ReadStream >> load->m_Dim;
    if (!this->m_ReadStream)
    {
      delete load;
      std::cout << "Error reading LoadGravConst definition - Dimension" << std::endl;
      return false;
    }

    float loadcomp = 0;
    /** then the actual values */
    for (int i = 0; i < load->m_Dim; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> loadcomp;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading LoadGravConst definition - Force Vector" << std::endl;
        return false;
      }
      load->m_ForceVector.push_back(loadcomp);
    }
  }
  else if (load_name == "LoadLandmark")
  {
    this->SkipWhiteSpace();
    int n1 = 0;
    int n2 = 0;

    // read the dimensions of the undeformed point and set the size of the point
    // accordingly
    this->SkipWhiteSpace();
    *this->m_ReadStream >> n1;
    if (!this->m_ReadStream)
    {
      return false;
    }
    load->m_Undeformed.resize(static_cast<unsigned long>(n1));
    for (int i = 0; i < n1; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> load->m_Undeformed[i];
      std::cout << "  " << load->m_Undeformed[i] << std::endl;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading Loadlandmark definition - Undeformed point" << std::endl;
        return false;
      }
    }

    // Read the dimensions of the deformed point and set the size of the point
    // accordingly
    this->SkipWhiteSpace();
    *this->m_ReadStream >> n2;
    if (!this->m_ReadStream)
    {
      return false;
    }
    load->m_Deformed.resize(static_cast<unsigned long>(n2));
    for (int i = 0; i < n2; i++)
    {
      this->SkipWhiteSpace();
      *this->m_ReadStream >> load->m_Deformed[i];
      std::cout << "  " << load->m_Deformed[i] << std::endl;
      if (!this->m_ReadStream)
      {
        delete load;
        std::cout << "Error reading Loadlandmark definition - Undeformed point" << std::endl;
        return false;
      }
    }

    // Verify that the undeformed and deformed points are of the same size.
    if (n1 != n2)
    {
      delete load;
      std::cout
        << "Error reading Loadlandmark definition - Undeformed point and deformed point should have same dimension"
        << std::endl;
      return false;
    }
    // read the square root of the m_Variance associated with this landmark
    this->SkipWhiteSpace();
    *this->m_ReadStream >> load->m_Variance;
  }
  if (!this->m_ReadStream)
  {
    delete load;
    std::cout << "Error reading Load definition" << std::endl;
    return false;
  }
  this->m_LoadList.push_back(load);
  return true;
}

int *
MetaFEMObject::GetElementDimensionAndNumberOfNodes(const std::string& c_string, int info[2])
{
  if ((c_string == "Element2DC0LinearLineStress") || (c_string == "Element2DC1Beam"))
  {
    info[0] = 2;
    info[1] = 2;
  }

  else if ((c_string == "Element2DC0LinearTriangularMembrane") || (c_string == "Element2DC0LinearTriangularStrain") ||
           (c_string == "Element2DC0LinearTriangularStress"))
  {
    info[0] = 3;
    info[1] = 2;
  }

  else if ((c_string == "Element2DC0LinearQuadrilateralMembrane") ||
           (c_string == "Element2DC0LinearQuadrilateralStrain") || (c_string == "Element2DC0LinearQuadrilateralStress"))
  {
    info[0] = 4;
    info[1] = 2;
  }


  else if ((c_string == "Element2DC0QuadraticTriangularStrain") || (c_string == "Element2DC0QuadraticTriangularStress"))
  {
    info[0] = 6;
    info[1] = 2;
  }

  else if ((c_string == "Element3DC0LinearHexahedronMembrane") || (c_string == "Element3DC0LinearHexahedronStrain"))
  {
    info[0] = 8;
    info[1] = 3;
  }

  else if ((c_string == "Element3DC0LinearTetrahedronMembrane") || (c_string == "Element3DC0LinearTetrahedronStrain"))
  {
    info[0] = 4;
    info[1] = 3;
  }

  else
  {
    return nullptr;
  }

  return info;
}
int
MetaFEMObject::ReadGlobalNumber()
{
  int n = 0;

  /** Read and set the global object number */
  this->SkipWhiteSpace();
  *this->m_ReadStream >> n;
  if (this->m_ReadStream)
  {
    return n;
  }
  else
  {
    return -1;
  }
}

// string containing all whitespace characters
const std::string MetaFEMObject ::whitespaces = " \t\n\r";

#if (METAIO_USE_NAMESPACE)
}
#endif
