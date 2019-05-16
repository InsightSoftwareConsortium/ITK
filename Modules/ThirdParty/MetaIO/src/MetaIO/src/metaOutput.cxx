/*============================================================================
  MetaIO
  Copyright 2000-2010 Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#ifdef _MSC_VER
#pragma warning(disable:4702)
#pragma warning(disable:4996)
#endif

#include "metaOutput.h"

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <winsock2.h>
#else
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include <cstring>
#include <ctime>

#include <typeinfo>

#if (METAIO_USE_NAMESPACE)
namespace METAIO_NAMESPACE {
#endif


/** Stolen from kwsys */
static std::string GetCurrentDateTime(const char* format)
{
  char buf[1024];
  time_t t;
  time(&t);
  strftime(buf, sizeof(buf), format, localtime(&t));
  return std::string(buf);
}


/****/
MetaOutputStream::
MetaOutputStream()
{
  m_Enable = true;
  m_IsStdStream = false;
  m_Name = "";
  m_MetaOutput = nullptr;
}

void MetaOutputStream::
SetName(const char* name)
{
  m_Name = name;
}

void MetaOutputStream::
Enable()
{
  m_Enable = true;
}

void MetaOutputStream::
Disable()
{
  m_Enable = false;
}

void MetaOutputStream::
SetStdStream(std::ostream * stream)
{
  m_StdStream = stream;
  m_IsStdStream = true;
}

bool MetaOutputStream::
IsStdStream()
{
  return m_IsStdStream;
}

std::ostream *
MetaOutputStream::
GetStdStream()
{
  return m_StdStream;
}

std::string
MetaOutputStream::
GetName() const
{
  return m_Name;
}

bool
MetaOutputStream::
IsEnable() const
{
  return m_Enable;
}

bool
MetaOutputStream::
Write(const char* buffer)
{
  if(m_IsStdStream)
    {
    *m_StdStream << buffer;
    }
  return true;
}

bool
MetaOutputStream::
Open()
{
  m_IsOpen = true;
  return true;
}

bool
MetaOutputStream::
Close()
{
  m_IsOpen = false;
  return true;
}

void
MetaOutputStream::
SetMetaOutput(void* metaOutput)
{
  m_MetaOutput = metaOutput;
}

/****/
MetaFileOutputStream::
MetaFileOutputStream(const char* name)
: MetaOutputStream()
{
  m_FileName = name;
  this->SetStdStream(&m_FileStream);
}

bool
MetaFileOutputStream::
Open()
{
  MetaOutputStream::Open();
#ifdef __sgi
  m_FileStream.open(m_FileName.c_str(), std::ios::out);
#else
  m_FileStream.open(m_FileName.c_str(), std::ios::binary
                                        | std::ios::out);
#endif
  if( m_FileStream.rdbuf()->is_open() )
    {
    return true;
    }
  else
    {
    return false;
    }
}

bool
MetaFileOutputStream::
Close()
{
  MetaOutputStream::Close();
  m_FileStream.close();
  return true;
}

std::string
MetaFileOutputStream::
GetFileName()
{
  return m_FileName;
}


/** Constructor */
MetaOutput::
MetaOutput()
{
  m_MetaCommand = nullptr;
  m_CurrentVersion = "0.1";
}

/** Destructor */
MetaOutput::
~MetaOutput()
{
  StreamVector::iterator itStream = m_StreamVector.begin();
  while(itStream != m_StreamVector.end())
    {
    itStream = m_StreamVector.erase(itStream);
    }
}

 /** Add a field */
bool MetaOutput::
AddField(std::string name,
         std::string description,
         TypeEnumType type,
         std::string value,
         std::string rangeMin,
         std::string rangeMax
         )
{
  Field field;
  field.name = name;
  field.description = description;
  field.value.push_back(value);
  field.type = type;
  field.rangeMin = rangeMin;
  field.rangeMax = rangeMax;
  m_FieldVector.push_back(field);
  return true;
}

 /** Add a float field */
bool MetaOutput::
AddFloatField(std::string name,
              std::string description,
              float value,
              std::string rangeMin,
              std::string rangeMax
              )
{
  char val[20];
  snprintf(val,sizeof(val),"%f",value);
  this->AddField(name,description,FLOAT,val,rangeMin,rangeMax);
  return true;
}

/** Add a int field */
bool MetaOutput::
AddIntField(std::string name,
            std::string description,
            int value,
            std::string rangeMin,
            std::string rangeMax
            )
{
  char val[10];
  snprintf(val,sizeof(val),"%d",value);
  this->AddField(name,description,INT,val,rangeMin,rangeMax);
  return true;
}

/** Add list field */
bool MetaOutput::AddListField(std::string name,
                              std::string description,
                              ListType list)
{
  Field field;
  field.name = name;
  field.description = description;
  ListType::const_iterator it = list.begin();
  while(it != list.end())
    {
    field.value.push_back(*it);
    ++it;
    }
  field.type = LIST;
  m_FieldVector.push_back(field);
  return true;
}

/** Add meta command */
void MetaOutput::
SetMetaCommand(MetaCommand* metaCommand)
{
  m_MetaCommand = metaCommand;
  m_MetaCommand->SetOption("GenerateMetaOutput","",false,"Generate MetaOutput");
  m_MetaCommand->SetOptionLongTag("GenerateMetaOutput","generateMetaOutput");
  m_MetaCommand->SetOption("GenerateXMLMetaOutput","",
                           false,"Generate XML MetaOutput to the console");
  m_MetaCommand->SetOptionLongTag("GenerateXMLMetaOutput","oxml");
  m_MetaCommand->SetOption("GenerateXMLFile","",
            false,"Generate XML MetaOutput to a file",MetaCommand::STRING,"",
            MetaCommand::DATA_OUT);
  m_MetaCommand->SetOptionLongTag("GenerateXMLFile","ofxml");
}

/** Get the username */
std::string MetaOutput::
GetUsername()
{
#if defined (_WIN32) && !defined(__CYGWIN__)
    char buf[1024];
    DWORD size = sizeof(buf);
    buf[0] = '\0';
    GetUserName( buf, &size);
    return  std::string(buf);
#else  // not _WIN32
    struct passwd *pw = getpwuid(getuid());
    if ( pw == nullptr )
      {
        std::cout << "getpwuid() failed " << std::endl;
        return "";
      }
    return pw->pw_name;
#endif // not _WIN32
}

std::string MetaOutput::
GetHostname()
{
#if defined (_WIN32) && !defined(__CYGWIN__)
  WSADATA    WsaData;
  int err = WSAStartup (0x0101, &WsaData);              // Init Winsock
  if(err!=0)
    {
    return "";
    }
#endif
  char nameBuffer[1024];
  gethostname(nameBuffer, 1024);
  std::string hostName(nameBuffer);
  return hostName;
}

std::string MetaOutput::GetHostip()
{
  #if defined (_WIN32) && !defined(__CYGWIN__)
    WSADATA    WsaData;
    int err = WSAStartup (0x0101, &WsaData);              // Init Winsock
    if(err!=0)
        return "";
  #endif

  struct hostent *phe = gethostbyname(GetHostname().c_str());
  if (phe == nullptr)
      return "";

  struct in_addr addr;
  char** address = phe->h_addr_list;
  int m_numaddrs = 0;
  while (*address)
{
    m_numaddrs++;
    address++;
}

  std::string m_ip = "";
  if (m_numaddrs != 0)
{
    memcpy(&addr, phe->h_addr_list[m_numaddrs-1], sizeof(struct in_addr));
    m_ip = inet_ntoa(addr);
}

 return m_ip;
}

/** Return the string representation of a type */
std::string MetaOutput::TypeToString(TypeEnumType type)
{
  switch(type)
    {
    case INT:
      return "int";
    case FLOAT:
      return "float";
    case STRING:
      return "string";
    case LIST:
      return "list";
    case FLAG:
      return "flag";
    case BOOL:
      return "boolean";
    case CHAR:
    default:
      return "not defined";
    }
}
/** Private function to fill in the buffer */
std::string MetaOutput::GenerateXML(const char* filename)
{
  std::string buffer;
  buffer = "<?xml version=\"1.0\"?>\n";
  buffer += "<MetaOutputFile ";
  if(filename)
    {
    std::string filenamestr = filename;
    buffer += "name=\"" +filenamestr+"\"";
    }
  buffer += " version=\""+m_CurrentVersion+"\">\n";

  buffer += "<Creation date=\""
             + GetCurrentDateTime("%Y%m%d") + "\"";
  buffer += " time=\""
             + GetCurrentDateTime("%H%M%S") + "\"";
  buffer += " hostname=\""+this->GetHostname() +"\"";
  buffer += " hostIP=\""+this->GetHostip() +"\"";
  buffer += " user=\"" + this->GetUsername() + "\"/>\n";

  buffer += "<Executable name=\"" + m_MetaCommand->GetApplicationName() + "\"";
  buffer += " version=\"" + m_MetaCommand->GetVersion() + "\"";
  buffer += " author=\"" + m_MetaCommand->GetAuthor() + "\"";
  buffer += " description=\"" + m_MetaCommand->GetDescription() + "\"/>\n";

  buffer += "<Inputs>\n";
  const MetaCommand::OptionVector options = m_MetaCommand->GetParsedOptions();
  MetaCommand::OptionVector::const_iterator itInput = options.begin();
  MetaCommand::OptionVector::const_iterator itInputEnd = options.end();
  while(itInput != itInputEnd)
    {
    if((*itInput).name == "GenerateMetaOutput")
      {
      ++itInput;
      continue;
      }

    typedef std::vector<MetaCommand::Field> CmdFieldVector;
    CmdFieldVector::const_iterator itField = (*itInput).fields.begin();
    CmdFieldVector::const_iterator itFieldEnd = (*itInput).fields.end();
    while(itField != itFieldEnd)
      {
      if((*itInput).fields.size() == 1)
        {
        buffer += "  <Input name=\"" + (*itInput).name +"\"";
        }
      else
        {
        buffer += "  <Input name=\"" + (*itInput).name + "."
                                     + (*itField).name +"\"";
        }

      buffer += " description=\"" + (*itInput).description + "\"";
      if((*itField).required)
        {
        buffer += " required=\"true\"";
        }
      buffer += " value=\"" + (*itField).value + "\"";
      buffer += " type=\"" + m_MetaCommand->TypeToString((*itField).type) + "\"";
      if((*itField).rangeMin != "")
        {
        buffer += " rangeMin=\"" + (*itField).rangeMin + "\"";
        }
      if((*itField).rangeMax != "")
        {
        buffer += " rangeMax=\"" + (*itField).rangeMax + "\"";
        }
      if((*itField).externaldata == MetaCommand::DATA_IN)
        {
        buffer += " externalData=\"in\"";
        }
      else if((*itField).externaldata == MetaCommand::DATA_OUT)
        {
        buffer += " externalData=\"out\"";
        }
      buffer += "/>\n";
      ++itField;
      }
    ++itInput;
    }

  buffer += "</Inputs>\n";

  // Output
  buffer += "<Outputs>\n";

  FieldVector::const_iterator itOutput =  m_FieldVector.begin();
  FieldVector::const_iterator itOutputEnd =  m_FieldVector.end();
  while(itOutput != itOutputEnd)
    {
    buffer += "  <Output name=\""+ (*itOutput).name + "\"";
    buffer += " description=\""+ (*itOutput).description + "\"";
    buffer += " type=\""+ this->TypeToString((*itOutput).type) + "\"";

    unsigned int index = 0;
    typedef std::vector<std::string> VectorType;
    VectorType::const_iterator itValue = (*itOutput).value.begin();
    while(itValue != (*itOutput).value.end())
      {
      buffer += " value";
      if((*itOutput).value.size()>1)
        {
        char val[10];
        snprintf(val,sizeof(val),"%u",index);
        buffer += val;
        }
      buffer += "=\"" + *itValue + "\"";
      ++itValue;
      index++;
      }
    buffer += "/>\n";
    ++itOutput;
    }
  buffer += "</Outputs>\n";

  // CRC32
  unsigned long crc = crc32(0L,(const Bytef*)buffer.c_str(),
    static_cast<int>(buffer.size()));
  char crcstring[10];
  snprintf(crcstring,sizeof(crcstring),"%lu",crc);
  // Compute the crc
  buffer += "<CRC32>";
  buffer += crcstring;
  buffer += "</CRC32>\n";
  buffer += "</MetaOutputFile>\n";
  return buffer;
}

/** Write the output to the connected streams */
void MetaOutput::Write()
{
  if(m_MetaCommand && m_MetaCommand->GetOptionWasSet("GenerateXMLMetaOutput"))
    {
    std::cout << this->GenerateXML().c_str() << std::endl;
    }
  if(m_MetaCommand && m_MetaCommand->GetOptionWasSet("GenerateXMLFile"))
    {
    //this->GenerateXML();
    std::string filename = m_MetaCommand
                                  ->GetValueAsString("GenerateXMLFile");
    std::ofstream fileStream;

#ifdef __sgi
    fileStream.open(filename.c_str(), std::ios::out);
#else
    fileStream.open(filename.c_str(), std::ios::binary
                                      | std::ios::out);
#endif

    if(fileStream.rdbuf()->is_open())
      {
      fileStream << this->GenerateXML(filename.c_str()).c_str();
      fileStream.close();
      }

    }

  if(m_MetaCommand && !m_MetaCommand->GetOptionWasSet("GenerateMetaOutput"))
    {
    return;
    }
  StreamVector::iterator itStream = m_StreamVector.begin();
  while(itStream != m_StreamVector.end())
    {
    if(!(*itStream)->IsEnable())
      {
      ++itStream;
      continue;
      }

    (*itStream)->SetMetaOutput(this);

    if(!(*itStream)->Open())
      {
      std::cout << "MetaOutput ERROR: cannot open stream"
                       << std::endl;
      return;
      }

    FieldVector::const_iterator it = m_FieldVector.begin();
    FieldVector::const_iterator itEnd = m_FieldVector.end();
    while(it != itEnd)
      {
      if(dynamic_cast<MetaFileOutputStream*>(*itStream))
        {
        std::string filename = ((MetaFileOutputStream*)(*itStream))
                                      ->GetFileName().c_str();
        (*itStream)->Write(this->GenerateXML(filename.c_str()).c_str());
        }
      else
        {
        (*itStream)->Write(this->GenerateXML().c_str());
        }
      ++it;
      }

    if(!(*itStream)->Close())
      {
      std::cout << "MetaOutput ERROR: cannot close stream"
                          << std::endl;
      return;
      }
    ++itStream;
    }
}

/** Add a stream */
void MetaOutput::AddStream(const char* name,std::ostream & stdstream)
{
  MetaOutputStream* stream = new MetaOutputStream;
  stream->SetName(name);
  stream->SetStdStream(&stdstream);
  m_StreamVector.push_back(stream);
}

void MetaOutput::AddStream(const char* name,MetaOutputStream * stream)
{
  stream->SetName(name);
  m_StreamVector.push_back(stream);
}

/** Add a stream file. Helper function */
void MetaOutput::AddStreamFile(const char* name,const char* filename)
{
  MetaFileOutputStream* stream = new MetaFileOutputStream(filename);
  this->AddStream(name,stream);
}

/** Enable a stream */
void MetaOutput::EnableStream(const char* name)
{
  StreamVector::iterator itStream = m_StreamVector.begin();
  while(itStream != m_StreamVector.end())
    {
    if(!strcmp((*itStream)->GetName().c_str(),name))
      {
      (*itStream)->Enable();
      }
    ++itStream;
    }
}

/** Disable a stream */
void MetaOutput::DisableStream(const char* name)
{
  StreamVector::iterator itStream = m_StreamVector.begin();
  while(itStream != m_StreamVector.end())
    {
    if(!strcmp((*itStream)->GetName().c_str(),name))
      {
      (*itStream)->Disable();
      }
    ++itStream;
    }
}

#if (METAIO_USE_NAMESPACE)
};
#endif
