#ifndef METADTITube_H
#define METADTITube_H

#include <metaTypes.h>
#include <metaUtils.h>
#include <metaObject.h>

#include <list>


/*!    MetaDTITube (.h and .cpp)
 *
 * Description:
 *    Reads and Writes MetaDTITubeFiles.
 *
 * \author Julien Jomier
 * 
 * \date May 22, 2002
 */

class DTITubePnt
{
public:

  DTITubePnt(int dim)
  { 
    m_Dim = dim;
    m_X = new float[m_Dim];
    m_T = new float[m_Dim];
    m_V1= new float[m_Dim];
    m_V2= new float[m_Dim];
    m_MinEV = new float[3];
    m_MedEV = new float[3];
    m_MaxEV = new float[3];
    m_MRI = new float[5];
    m_TensorMatrix = new float[6];
  
    unsigned int i=0;
    for(i=0;i<m_Dim;i++)
      {
      m_X[i] = 0;
      m_V1[i]= 0;
      m_V2[i]= 0;
      m_T[i]= 0;
      }
 
    for(i=0;i<3;i++)
      {
      m_MinEV[i] = 0;
      m_MedEV[i] = 0;
      m_MaxEV[i] = 0;
      }

    for(i=0;i<5;i++)
      {
      m_MRI[i] = 0;
      }
  
    // Initialize the tensor matrix to identity
    for(i=0;i<6;i++)
      {
      m_TensorMatrix[i] = 0;
      }
    m_TensorMatrix[0] = 1;
    m_TensorMatrix[3] = 1;
    m_TensorMatrix[5] = 1;

    m_R=0;
    m_FA=0;
    m_ADC=0;
    m_GA = 0;
    m_Interpolation=0;
    m_Lambda1=0;
    m_Lambda2=0;
    m_Lambda3=0;

    //Color is red by default
    m_Color[0]=1.0;
    m_Color[1]=0.0;
    m_Color[2]=0.0;
    m_Color[3]=1.0;
    m_ID = -1;
  }

  ~DTITubePnt()
  {
    delete m_X;
    delete m_V1;
    delete m_V2;
    delete m_T;
    delete m_MinEV;
    delete m_MedEV;
    delete m_MaxEV;
    delete m_MRI;
    delete m_TensorMatrix;
  };
  
  unsigned int m_Dim;
  float* m_V1;
  float* m_V2;
  float* m_X;
  float* m_T;
  float m_R;
  float m_Color[4];
  int   m_ID;
  float m_FA;
  float m_ADC;
  float m_GA;
  float m_Lambda1;
  float m_Lambda2;
  float m_Lambda3;
  float* m_MinEV;
  float* m_MedEV;
  float* m_MaxEV;
  float* m_MRI;
  float* m_TensorMatrix;
  int   m_Interpolation;
};




class MetaDTITube : public MetaObject
  {

  /////
  //
  // PUBLIC
  //
  ////
  public:

   typedef std::list<DTITubePnt*> PointListType;
    ////
    //
    // Constructors & Destructor
    //
    ////
    MetaDTITube(void);

    MetaDTITube(const char *_headerName);   

    MetaDTITube(const MetaDTITube *_DTITube); 
    
    MetaDTITube(unsigned int dim);

    ~MetaDTITube(void);

    void PrintInfo(void) const;

    void CopyInfo(const MetaDTITube * _DTITube);

    //    NPoints(...)
    //       Required Field
    //       Number of points wich compose the DTITube
    void  NPoints(int npnt);
    int   NPoints(void) const;

    //    PointDim(...)
    //       Required Field
    //       Definition of points
    void        PointDim(const char* pointDim);
    const char* PointDim(void) const;

    //    Root(...)
    //       Optional Field
    //       Set if this DTITube is a root
    void  Root(int root);
    int   Root(void) const;


    //    ParentPoint(...)
    //       Optional Field
    //       Set the point number of the parent DTITube where the branch occurs
    void  ParentPoint(int parentpoint);
    int   ParentPoint(void) const;

    void  Clear(void);

    PointListType &  GetPoints(void) {return m_PointList;}
    const PointListType &  GetPoints(void) const {return m_PointList;}
    
    MET_ValueEnumType ElementType(void) const;
    void  ElementType(MET_ValueEnumType _elementType);

  ////
  //
  // PROTECTED
  //
  ////
  protected:

    bool  m_ElementByteOrderMSB;

    void  M_Destroy(void);

    void  M_SetupReadFields(void);

    void  M_SetupWriteFields(void);

    bool  M_Read(void);

    bool  M_Write(void);

    int m_ParentPoint;  // "ParentPoint = "     -1

    int m_Root;         // "Root = "            0

    int m_NPoints;      // "NPoints = "         0

    char m_PointDim[255]; // "PointDim = "       "x y z r"

    PointListType m_PointList;
    MET_ValueEnumType m_ElementType;
  };


#endif
