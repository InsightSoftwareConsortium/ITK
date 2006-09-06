class itkCType :
   __cTypes__ = {}

   def __init__(self,name,shortName):
      self.__name=name
      self.__shortName=shortName

      itkCType.__cTypes__[self.__name]=self

   def __del__(self):
      try:
         del itkCType.__cTypes__[self.__name]
      except:
         pass

   def __repr__(self):
      return "<itkCType %s>" % self.__name

   def GetCType(name):
      try:
         return(itkCType.__cTypes__[name])
      except KeyError:
         return(None)
   GetCType=staticmethod(GetCType)


F  = itkCType("float",          "F")
D  = itkCType("double",         "D")
LD = itkCType("long double",   "LD")
UC = itkCType("unsigned char", "UC")
US = itkCType("unsigned short","US")
UI = itkCType("unsigned int",  "UI")
UL = itkCType("unsigned long", "UL")
SC = itkCType("signed char",   "SC")
SS = itkCType("signed short",  "SS")
SI = itkCType("signed int",    "SI")
SL = itkCType("signed long",   "SL")
B  = itkCType("bool",           "B")