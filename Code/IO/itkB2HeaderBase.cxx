#include "itkExceptionObject.h"
#include "itkB2HeaderBase.h"
#include "itkB2HeaderFactory.h"

//#include "itkMacro.h"

namespace itk {
B2HeaderBase::B2HeaderBase()
{
  //Nothing to be done here.
}
B2HeaderBase::~B2HeaderBase()
{
  this->ClearHeader();
}

void B2HeaderBase::ReadB2Header(std::string filename)
{
  std::ifstream   local_InputStream;
  local_InputStream.open( filename.c_str(), std::ios::in | std::ios::binary );
  if( !local_InputStream )
    {
    //               itk::itkExceptionMacro("Error opening image data file for reading.");
    }
  ReadB2Header(local_InputStream);
  local_InputStream.close();
}

void B2HeaderBase::ClearHeader(void)
{
  //Clear out all children, While children exist do
  while(m_child.size() != 0)
    {
    //Get value of front of list
    B2HeaderBase * pi=m_child.front();
    //remove value from list
    m_child.pop_front();
    //delte value
    delete pi;
    }
  this->clear();
}


void B2HeaderBase::WriteB2Header(std::string filename) const
{
  std::ofstream   local_OutputStream;
  local_OutputStream.open( filename.c_str(), std::ios::out | std::ios::binary );
  if( !local_OutputStream )
    {
    //              itk::itkExceptionMacro("Error opening image data file for reading.");
    }
  WriteB2Header(local_OutputStream);
  local_OutputStream.close();
}

std::ifstream & B2HeaderBase::ReadB2Header(std::ifstream  & inputstream)
{
  std::string Key;
  //NOTE: tellg returns the position in number of bytes from the begining of the stream.
  long int FileStartPos=inputstream.tellg();
  //NOTE: the >> operator on a stream reads "words" at a time.
  inputstream >> Key;  //The first word to read must be "IPL_HEADER_BEGIN", with no value
  //std::cout << "Looking for " << this->GetHeaderBeginTag() <<" but found " << Key << std::endl;
  if(Key.find(this->GetHeaderBeginTag()) == std::string::npos )
    {
    //Return the file to it's position before attempting read.
    inputstream.seekg(FileStartPos);
    return inputstream;
    }
  else
    {
    this->push_back(std::list< std::pair<std::string,std::string> >::value_type(Key,""));
    }
  itk::B2HeaderFactory MyB2HdrFac;
  long int PreKeyReadPosition=inputstream.tellg();
  inputstream >> Key; //Read key that follows "IPL_HEADER_BEGIN"
  while(Key != this->GetHeaderEndTag() )  //If key = "IPL_HEADER_END", then there is no value
    {
    if(inputstream.eof() == true)
      {
      ExceptionObject exception(__FILE__, __LINE__);
      exception.SetDescription("Unexpected end of file");
      throw exception;
      }
    //Check for the case where the specific header type begins and ends
    //i.e. MASK_HEADER_BEGIN and MASK_HEADER_END, or IMAGE_HEADER_BEGIN and IMAGE_HEADER_END
    if ( Key.find("HEADER_BEGIN") != std::string::npos)
      {
      this->push_back(std::list< std::pair<std::string,std::string> >::value_type("--BEGIN_CHILD--",""));
      //Rewind to befor the key.
      inputstream.seekg(PreKeyReadPosition);
      //Need Factory Here to produce proper factory based on Key.
      this->m_child.push_back(MyB2HdrFac.CreateB2HeaderReader(Key));
      if(this->m_child.back() == NULL)
        {
        //DEBUG: Throw error
        return inputstream;
        }
      this->m_child.back()->ReadB2Header(inputstream);
      PreKeyReadPosition=inputstream.tellg();
      inputstream >> Key;
      continue;
      }
    std::string Value;  //If key does not == "IPL_HEADER_END", then there must be a value
    inputstream >> Value;
    this->push_back(std::list< std::pair<std::string,std::string> >::value_type(Key,Value));
    //Read Next Key
    PreKeyReadPosition=inputstream.tellg();
    inputstream >> Key;
    }
  //pusch back the end key
  this->push_back(std::list< std::pair<std::string,std::string> >::value_type(Key,""));
  return inputstream;
}
std::ofstream & B2HeaderBase::WriteB2Header(std::ofstream & outputstream) const
{
  return outputstream;
}
void B2HeaderBase::PrintSelf(std::ostream &os) const
{
  std::list<B2HeaderBase *>::const_iterator childiterator=this->m_child.begin();
  //For each element in internal list
  for(std::list< std::pair<std::string,std::string> >::const_iterator pi=this->begin();
      pi != this->end(); pi++)
    {
    if(pi->first == "--BEGIN_CHILD--")
      {
      //std::cout <<"Size of m_child " << m_child.size() << std::endl;
      (*childiterator)->PrintSelf(os);
      childiterator++;
      continue;
      }
    //os << "Key  " << pi->first << " Value " << pi->second << std::endl;
    os << pi->first << "  " << pi->second << std::endl;
    }
}

bool B2HeaderBase::DoesKeyExist(const std::string &KeyID) const
{
  std::list<B2HeaderBase *>::const_iterator childiterator=this->m_child.begin();
  //For each element in internal list
  for(std::list< std::pair<std::string,std::string> >::const_iterator pi=this->begin();
      pi != this->end(); pi++)
    {
    if(pi->first == KeyID)
      {
      return true;
      }
    else if(pi->first == "--BEGIN_CHILD--")
      {
      //std::cout <<"Size of m_child " << m_child.size() << std::endl;
      if((*childiterator)->DoesKeyExist(KeyID)==true)
        {
        return true;
        }
      childiterator++;
      continue;
      }
    }
  //Return an empty string
  return false;
}


std::string B2HeaderBase::getString(const std::string &KeyID) const
{
  //this->PrintSelf(std::cout);
  std::list<B2HeaderBase *>::const_iterator childiterator=this->m_child.begin();
  //For each element in internal list
  for(std::list< std::pair<std::string,std::string> >::const_iterator pi=this->begin();
      pi != this->end(); pi++)
    {
    if(pi->first == KeyID)
      {
      return pi->second;
      }
    else if(pi->first == "--BEGIN_CHILD--")
      {
      //std::cout <<"Size of m_child " << m_child.size() << std::endl;
      std::string TempStringValue=(*childiterator)->getString(KeyID);
      if(TempStringValue.length() != 0)
        {
        return TempStringValue;
        }
      childiterator++;
      continue;
      }
    }
  //Return an empty string
  return std::string("");
}

float B2HeaderBase::getFloat(const std::string &KeyID) const
{
  std::string TempStringValue=this->getString(KeyID);
  if(TempStringValue.length() != 0)
    {
    return atof(TempStringValue.c_str());
    }
  return 0.0F;
}

int B2HeaderBase::getInt(const std::string &KeyID) const
{
  std::string TempStringValue=this->getString(KeyID);
  if(TempStringValue.length() != 0)
    {
    return atoi(TempStringValue.c_str());
    }
  return 0;
}
} //End namespace itk
