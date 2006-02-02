#! /usr/bin/env python
"""
Let's write our own python parser to clean up the pdf (after 
pdftotext of course). 
Instructions: run pdftotext like this:

$ pdftotext -f 9 -l 81 -raw -nopgbrk 04_06PU.PDF 04_06PU-3.txt

then run the python parser like this:

$ python ParseDict.py 04_06PU.txt dicomV3.dic
"""
import re,os

"""
PdfTextParser takes as input a text file (produced by pdftotext)
and create as output a clean file (ready to be processed) by
DicomV3Expander
Warning: PdfTextParser does not expand:
- (xxxx,xxxx to xxxx) xxxxxxxxxxxx
or
- (12xx, 3456) comment...

"""
class PdfTextParser:
  # Cstor
  def __init__(self):
    self._InputFilename = ''
    self._OutputFilename = ''
    self._Infile = 0
    self._OutLines = []
    self._PreviousBuffers = []

  def SetInputFileName(self,s):
    self._InputFilename = s

  def SetOutputFileName(self,s):
    self._OutputFilename = s
  
  # Function returning if s is a comment for sure
  def IsAComment(self,s):
    #print s,  len(s)
    if s == "Tag Name VR VM":
      return True
    elif s == "PS 3.6-2003":
      return True
    elif s == "PS 3.6-2004":
      return True
    patt = re.compile('^Page [0-9]+$') 
    if( patt.match(s) ):
      return True
    return False

  def IsAStartingLine(self,s):
    patt = re.compile('^\\([0-9a-fA-Fx]+,[0-9a-fA-F]+\\) (.*)$') 
    if( patt.match(s) ):
      return True
    return False

  def IsAFullLine(self,s):
    patt = re.compile('^\\([0-9a-fA-Fx]+,[0-9a-fA-F]+\\) (.*) [A-Z][A-Z] [0-9]$')
    if( patt.match(s) ):
      return True
    return False

  # FIXME this function could be avoided...
  def IsSuspicious(self,s):
    l = len(s)
    if l > 80:
      return True
    return False

  def AddOutputLine(self,s):
    assert not self.IsAComment(s)
    self._OutLines.append(s + '\n')

  def Open(self):
    self._Infile = file(self._InputFilename, 'r')
    for line in self._Infile.readlines():
      line = line[:-1] # remove '\n'
      if not self.IsAComment( line ):
        if self.IsAStartingLine(line):
          #print "Previous buffer:",self._PreviousBuffers
          previousbuffer = ' '.join(self._PreviousBuffers)
          if self.IsAStartingLine(previousbuffer):
            if not self.IsSuspicious(previousbuffer):
              self.AddOutputLine(previousbuffer)
            else:
              # this case should not happen if I were to rewrite the
              # thing I should be able to clean that
              #print "Suspicious:", previousbuffer
              #print "List is:", self._PreviousBuffers
              s = self._PreviousBuffers[0]
              if self.IsAFullLine(s):
                # That means we have a weird line that does not start
                # as usual (xxxx,xxxx) therefore we tried constructing
                # a buffer using a the complete previous line...
                #print "Full line:", s
                self.AddOutputLine(s)
                s2 = ' '.join(self._PreviousBuffers[1:])
                #print "Other Full line:", s2
                self.AddOutputLine(s2)
              else:
                # we have a suspicioulsy long line, so what that could
                # happen, let's check:
                if self.IsAFullLine(previousbuffer):
                  self.AddOutputLine(previousbuffer)
                else:
                  # This is the only case where we do not add
                  # previousbuffer to the _OutLines
                  print "Suspicious and Not a full line:", s
          else:
            if previousbuffer:
              print "Not a buffer:", previousbuffer
          # We can clean buffer, since only the case 'suspicious' +
          # 'Not a full line' has not added buffer to the list
          self._PreviousBuffers = []
          # In all cases save the line for potentially growing this line
          assert not self.IsAComment(line)
          self._PreviousBuffers.append(line)
        else:
          #print "Not a line",line
          assert not self.IsAComment(line)
          self._PreviousBuffers.append(line)
      else:
        #print "Comment:",line
        previousbuffer = ' '.join(self._PreviousBuffers)
        if previousbuffer and self.IsAStartingLine(previousbuffer):
          #print "This line is added:", previousbuffer
          self.AddOutputLine( previousbuffer )
        else:
          #print "Line is comment:", line
          print "Buffer is:", previousbuffer
        # Ok this is a comment we can safely clean the buffer:
        self._PreviousBuffers = []
    self.Write()

  def Write(self):
    outfile = file(self._OutputFilename, 'w')
    outfile.writelines( self._OutLines )
    outfile.close()
    self._Infile.close()
    
  # Main function to call for parsing
  def Parse(self):
    self.Open()

"""
subclass
"""
class UIDParser(PdfTextParser):
  def IsAStartingLine(self,s):
    patt = re.compile('^1.2.840.10008.[0-9.]+ (.*)$') 
    if( patt.match(s) ):
      return True
    #print "Is Not:", s
    return False

  def IsAFullLine(self,s):
    patt = re.compile('^1.2.840.10008.[0-9.]+ (.*) PS ?[0-9].1?[0-9]$') 
    if( patt.match(s) ):
      return True
    patt = re.compile('^1.2.840.10008.[0-9.]+ (.*) Well-known frame of reference$') 
    if( patt.match(s) ):
      return True
    patt = re.compile('^1.2.840.10008.[0-9.]+ (.*) \\(Retired\\)$') 
    if( patt.match(s) ):
      return True
    return False

  def IsAComment(self,s):
    if PdfTextParser.IsAComment(self,s):
      return True
    # else let's enhance the super class
    patt = re.compile('^SPM2 (.*) http(.*)$') 
    if( patt.match(s) ):
      return True
    return False

  def AddOutputLine(self,s):
    if self.IsAFullLine(s):
      return PdfTextParser.AddOutputLine(self,s)
    print "Discarding:", s


"""
TransferSyntaxParser
"""
class TransferSyntaxParser(UIDParser):
  def IsAFullLine(self,s):
    patt = re.compile('^(.*) Transfer Syntax PS ?[0-9].1?[0-9]$') 
    if patt.match(s):
      return UIDParser.IsAStartingLine(self,s)
    print "Not a TS:", s
    return False
    
"""
Papyrus parser
pdftotext -f 19 -l 41 -raw -nopgbrk /tmp/Papyrus31Specif.pdf /tmp/Papyrus31Specif.txt 

I need to do a second pass for pages:
#29 since I need to find [0-9.]+
#40,41 since it start with number in two columns !!
""" 
class PapyrusParser(PdfTextParser):
  def __init__(self):
    self._PreviousPage = 0
    self._PreviousNumber = 0
    PdfTextParser.__init__(self)

  def IsAStartingLine(self,s):
    patt = re.compile('^[A-Za-z \'\(\)]+ +\\([0-9A-F]+,[0-9A-F]+\\) +(.*)$') 
    if( patt.match(s) ):
      return True
    # After page 39, lines are like:
    patt = re.compile('^[0-9x]+ [0-9xA-F]+ .*$') 
    if( patt.match(s) ):
      #print "PAge 39", s
      return True
    return False

  def IsAFullLine(self,s):
    patt = re.compile('^[A-Za-z \'\(\)]+ +\\([0-9A-F]+,[0-9A-F]+\\) +(.*)$') 
    if( patt.match(s) ):
      return True
    # After page 39, lines are like:
    patt = re.compile('^[0-9x]+ [0-9xA-F]+ .* [A-Z][A-Z] [0-9].*$') 
    if( patt.match(s) ):
      #print "PAge 39", s
      return True
    return False

  def IsAComment(self,s):
    # dummy case:
    if s == 'Attribute Name Tag Type Attribute Description':
      #print "Dummy", s
      return True
    patt = re.compile('^.*ANNEXE.*$')
    if patt.match(s):
      return True
    # Indicate page #, spaces ending with only one number
    # Sometime there is a line with only one number, we need to
    # make sure that page # is strictly increasing
    patt = re.compile('^[1-9][0-9]+$') 
    if( patt.match(s) ):
      p = eval(s)
      if( p > self._PreviousPage):
        #print "Page #", p
        self._PreviousNumber = 0
        self._PreviousPage = p
        return True
#      else:
#        print "PAGE ERROR:", s
    # Now within each page there is a comment that start with a #
    # let's do the page approach wich reset at each page
    patt = re.compile('^[0-9]+$') 
    if( patt.match(s) ):
      if( eval(s) > self._PreviousNumber):
        #print "Number #", eval(s)
        self._PreviousNumber = eval(s)
        return True
      #else:
      #  print "ERROR:", s
    return False

  def AddOutputLine(self,s):
    assert not self.IsAComment(s)
    s = s.replace('\n','')
    #print "REMOVE return:", s
    patt = re.compile('^([A-Za-z \'\(\)]+) (\\([0-9A-F]+,[0-9A-F]+\\)) ([0-9C]+) (.*)$') 
    m = patt.match(s)
    ss = 'dummy (0000,0000) 0'
    if m:
      ss = m.group(2) + ' ' + m.group(3) + ' ' + m.group(1)
    else:
      patt = re.compile('^([A-Za-z \'\(\)]+) (\\([0-9A-F]+,[0-9A-F]+\\)) (.*)$') 
      m = patt.match(s)
      if m:
        ss = m.group(2) + ' 0 ' + m.group(1)
      else:
        ss = s
        # There is two case one that end with all capital letter
        # explaining the 'DEFINED TERMS'
        patt = re.compile('^[0-9x]+ [0-9xA-F]+ .* [A-Z][A-Z] [0-9] [A-Z, ]$') 
        #patt = re.compile('^[0-9x]+ [0-9xA-F]+ .* [A-Z][A-Z] [0-9]|1\\-n [A-Z, |3.0]+$') 
        #patt = re.compile('^[0-9x]+ [0-9xA-F]+ .* [A-Z][A-Z] [01n-] [A-Z, |3.0]+$') 
        if patt.match(s):
          print "Match", s
          ss = ''
    self._OutLines.append(ss + '\n')

  def Open(self):
    self._Infile = file(self._InputFilename, 'r')
    for line in self._Infile.readlines():
      line = line[:-1] # remove '\n'
      if not self.IsAComment( line ):
        if self.IsAStartingLine(line):
          #print "Previous buffer:",self._PreviousBuffers
          previousbuffer = ' '.join(self._PreviousBuffers)
          if self.IsAFullLine(previousbuffer):
            self.AddOutputLine(previousbuffer)
          else:
            if previousbuffer:
              print "Not a buffer:", previousbuffer
          # We can clean buffer, since only the case 'suspicious' +
          # 'Not a full line' has not added buffer to the list
          self._PreviousBuffers = []
          # In all cases save the line for potentially growing this line
          # just to be safe remove any white space at begining of string
          assert not self.IsAComment(line)
          self._PreviousBuffers.append(line.strip())
        else:
          #print "Not a line",line
          assert not self.IsAComment(line)
          # just to be safe remove any white space at begining of string
          self._PreviousBuffers.append(line.strip())
      else:
        #print "Previous buffer:",self._PreviousBuffers
        previousbuffer = ' '.join(self._PreviousBuffers)
        if previousbuffer and self.IsAStartingLine(previousbuffer):
          #print "This line is added:", previousbuffer
          self.AddOutputLine( previousbuffer )
#        else:
#          #print "Line is comment:", line
#          print "Buffer is:", previousbuffer
        # Ok this is a comment we can safely clean the buffer:
        self._PreviousBuffers = []
    self.Write()

"""
Parser for:
GE Medical Systems HISPEED ADVANTAGE CT/i CONFORMANCE STATEMENT
pdftotext -f 81 -l 90 -raw -nopgbrk 2162114_100r5.pdf 2162114_100r5.txt
"""
class GEMSParser(PdfTextParser):
#  def __init__(self):
#    PdfTextParser.__init__(self)

  def IsAStartingLine(self,s):
    #patt = re.compile('^[A-Za-z \'\(\)]+ +\\([0-9A-F]+,[0-9A-F]+\\) +(.*)$') 
    patt = re.compile('^[A-Za-z0-9 .#(),_/-]+ +\\([0-9A-F]+, ?[0-9A-F]+\\) +(.*)$')
    if( patt.match(s) ):
      return True
    return False

  def IsAFullLine(self,s):
    #patt = re.compile('^[A-Za-z \'\(\)]+ +\\([0-9A-F]+,[0-9A-F]+\\) +(.*)$') 
    patt = re.compile('^[A-Za-z0-9 .#(),_/-]+ +\\([0-9A-F]+, ?[0-9A-F]+\\) [A-Z][A-Z] [0-9]+$') 
    if( patt.match(s) ):
      return True
    print "Not full:", s
    return False

  def IsAComment(self,s):
    if PdfTextParser.IsAComment(self,s):
      return True
    #patt = re.compile('^.*GE Medical Systems LightSpeed QX/i CONFORMANCE STATEMENT REV 2.2 sm 2288567-100.*$')
    #if patt.match(s):
    #  return True
    patt = re.compile('^.*GE Medical Systems HISPEED ADVANTAGE CT/i CONFORMANCE STATEMENT.*$') 
    if patt.match(s):
      return True
    patt = re.compile('^GE Medical Systems LightSpeed QX/i CONFORMANCE STATEMENT.*$')
    if patt.match(s):
      return True
    patt = re.compile('^Attribute Name Tag VR VM$')
    if patt.match(s):
      return True
    patt = re.compile('^B.[1-9].*Private .*$')
    if patt.match(s):
      return True
    patt = re.compile('^Table B.1.? .* Private .*$')
    if patt.match(s):
      return True
    patt = re.compile('^Note :.*$')
    if patt.match(s):
      return True
    patt = re.compile('^7.11.1$')
    if patt.match(s):
      return True
    return False

  def AddOutputLine(self,s):
    #print s
    assert not self.IsAComment(s)
    patt = re.compile('^([A-Za-z0-9 .#(),_/-]+) +\\(([0-9A-F]+), ?([0-9A-F]+)\\) ([A-Z][A-Z]) ([0-9]+)$') 
    m = patt.match(s)
    if m:
      ss = m.group(2).lower() + ' ' + m.group(3).lower() + ' ' + m.group(4) + ' ' + m.group(5) + ' ' + m.group(1)
      self._OutLines.append(ss + '\n')
    else:
      print 'OOOPs', s


"""
This class is meant to expand line like:
- (xxxx,xxxx to xxxx) xxxxxxxxxxxx
or
- (12xx, 3456) comment...

"""
class DicomV3Expander:
  def __init__(self):
    self._InputFilename = ''
    self._OutputFilename = ''
    self._OutLines = []

  def SetInputFileName(self,s):
    self._InputFilename = s

  def SetOutputFileName(self,s):
    self._OutputFilename = s
 
  # Function to turn into lower case a tag:
  # ex: (ABCD, EF01) -> (abcd, ef01)
  def LowerCaseTag(self,s):
    #print "Before:", s[:-1]
    patt = re.compile('^(\\([0-9a-fA-F]+,[0-9a-fA-F]+\\))(.*)$')
    m = patt.match(s)
    if m:
      s1 = m.group(1)
      s2 = m.group(2)
      return s1.lower() + s2
    else:
      patt = re.compile('^[0-9a-fA-F]+ [0-9a-fA-F]+ [A-Z][A-Z] [0-9n-] .*$')
      if patt.match(s):
        return s
      else:
        print "Impossible case:", s
        os.sys.exit(1)

  def AddOutputLine(self,s):
    if s.__class__ == list:
      for i in s:
        self._OutLines.append(i + '\n')
    else:
      self._OutLines.append(s + '\n')

  # Expand the line approriaetkly and also add it to the
  # _OutLines list
  def ExpandLine(self, s):
    assert s[-1] == '\n'
    s = s[:-1]  # remove \n
    list = []
    if self.NeedToExpansion(s, list):
      self.AddOutputLine(list) # list != []
    elif self.NeedGroupXXExpansion(s, list):
      self.AddOutputLine(list) # list != []
    elif self.NeedElemXXExpansion(s, list):
      self.AddOutputLine(list) # list != []
    else:
      self.AddOutputLine(self.LowerCaseTag(s))

  # If line is like:
  # (0020,3100 to 31FF) Source Image Ids RET
  def NeedToExpansion(self,s, list):
    patt = re.compile('^\\(([0-9a-fA-F]+),([0-9a-fA-F]+) to ([0-9a-fA-F]+)\\)(.*)$')
    m = patt.match(s)
    if m:
      #print m.groups()
      gr = m.group(1)
      el_start = '0x'+m.group(2)
      el_end = '0x'+m.group(3)
      for i in range(eval(el_start), eval(el_end)):
        el = hex(i)[2:]
        l = '('+gr+','+el+')'+m.group(4)
        list.append(l)
      return True
    return False

  # If line is like:
  # (50xx,1200) Number of Patient Related Studies IS 1
  def NeedGroupXXExpansion(self,s,list):
    patt = re.compile('^\\(([0-9a-fA-F]+)xx,([0-9a-fA-F]+)\\)(.*)$')
    m = patt.match(s)
    if m:
      #print m.groups()
      gr_start = m.group(1)
      el = m.group(2)
      #el_start = '0x'+m.group(2)
      #el_end = '0x'+m.group(3)
      start = '0x'+gr_start+'00'
      end   = '0x'+gr_start+'FF'
      for i in range(eval(start), eval(end)):
        gr = hex(i)[2:]
        l = '('+gr+','+el+')'+m.group(3)
        #print l
        list.append(l)
      return True
    return False

  # If line is like:
  # (2001,xx00) Number of Patient Related Studies IS 1
  def NeedElemXXExpansion(self,s,list):
    patt = re.compile('^([0-9a-fA-F]+) ([0-9a-fA-F]+)xx(.*)$')
    m = patt.match(s)
    if m:
      #print m.groups()
      gr = m.group(1)
      el_start = m.group(2)
      start = '0x00'
      end   = '0xFF'
      for i in range(eval(start), eval(end)):
        el = '%02x'% i
        l = '('+gr+','+el_start+el+')'+m.group(3)
        print l
        list.append(l)
      return True
    else:
      patt = re.compile('^([0-9a-fA-F]+) xx([0-9a-fA-F]+)(.*)$')
      m = patt.match(s)
      if m:
        #print m.groups()
        gr = m.group(1)
        el_start = m.group(2)
        start = '0x00'
        end   = '0xFF'
        for i in range(eval(start), eval(end)):
          el = '%02x'% i
          l = '('+gr+','+el+el_start+')'+m.group(3)
          print l
          list.append(l)
        return True
    return False

  def Write(self):
    outfile = file(self._OutputFilename, 'w')
    outfile.writelines( self._OutLines )
    outfile.close()

  def Expand(self):
    infile = file(self._InputFilename,'r')
    for line in infile.readlines():
      # ExpandLine also LowerCase the line
      self.ExpandLine(line) # l is [1,n] lines
    self.Write()
    infile.close()

"""
Parse line from a philips document, line are like this:

Syncra Scan Type 2005,10A1 VR = CS, VM = 1
"""
class InteraParser:
  def __init__(self):
    self._InputFilename = ''
    self._OutputFilename = ''

  def Reformat(self,s):
    assert self.IsGood(s)
    patt = re.compile("^([A-Za-z0-9 -]+) ([0-9A-Z]+),([0-9A-Z]+) VR = ([A-Z][A-Z]), VM = (.*)$")
    m = patt.match(s)
    if m:
      dicom = m.group(2) + ' ' + m.group(3) + ' ' + m.group(4) + ' ' + m.group(5) + ' ' + m.group(1)
      return dicom
    else:
      print "oops"

  def IsGood(self,s):
    patt = re.compile("^[A-Za-z0-9 -]+ [0-9A-Z]+,[0-9A-Z]+ VR = [A-Z][A-Z], VM = .*$")
    if patt.match(s):
      return True
    print "Not good:", s
    return False

  def SetInputFileName(self,s):
    self._InputFilename = s

  def SetOutputFileName(self,s):
    self._OutputFilename = s
  
  def Parse(self):
    infile = file(self._InputFilename, 'r')
    outLines = []
    for line in infile.readlines():
      print self.Reformat(line)
      outLines.append( self.Reformat(line) + '\n' )
    outfile = file(self._OutputFilename, 'w')
    outfile.writelines( outLines )
    outfile.close()
 
"""
Parse line from a dicom3tools document, line are like this:

(0003,0008) VERS="SSPI" VR="US"   VM="1"        Owner="SIEMENS ISI"             Keyword="ISICommandField"                       Name="ISI Command Field"
"""
class Dicom3ToolsParser:
  def __init__(self):
    self._InputFilename = ''
    self._OutputFilename = ''

  def Reformat(self,s):
    assert self.IsGood(s)
    patt = re.compile("^\(([0-9a-f]+),([0-9a-f]+)\)\s+VERS=\".*\"\s+VR=\"([A-Z][A-Z])\"\s+VM=\"(.*)\"\s+Owner=\".*\"\s+Keyword=\".*\"\s+Name=\"(.*)\"$")
    m = patt.match(s)
    dicom = ''
    if m:
      # Apparently some have Name == '?', skip those
      name = m.group(5)
      if name != '?' and name != '? ':
        dicom = m.group(1) + ' ' + m.group(2) + ' ' + m.group(3) + ' ' + m.group(4) + ' ' + m.group(5)
      else:
        print "oops"
    else:
      print "oops"
    return dicom

  def IsGood(self,s):
    #patt = re.compile("^\([0-9a-f]+,[0-9a-f]+\) VERS=\".*\" VR=\"[A-Z][A-Z]\" VM=\".*\" Owner=\".*\" Keyword=\".*\" Name=\".*\"$")
    patt = re.compile("^\([0-9a-f]+,[0-9a-f]+\)\s+VERS=\".*\"\s+VR=\"[A-Z][A-Z]\"\s+VM=\".*\"\s+Owner=\".*\"\s+Keyword=\".*\"\s+Name=\".*\".*$")
    if patt.match(s):
      return True
    print "Not good:", s
    return False

  def SetInputFileName(self,s):
    self._InputFilename = s

  def SetOutputFileName(self,s):
    self._OutputFilename = s
  
  def Parse(self):
    infile = file(self._InputFilename, 'r')
    outLines = []
    for line in infile.readlines():
      newline = self.Reformat(line)
      print newline
      if newline:
        outLines.append( newline + '\n' )
    outfile = file(self._OutputFilename, 'w')
    outfile.writelines( outLines )
    outfile.close()
 
"""
Parse line from a PhilipsAdvance document, line are like this:

GE Advance Implementation Version Name (0009,1001) 3 LO 2 n/a
"""
class GEAdvanceParser:
  def __init__(self):
    self._InputFilename = ''
    self._OutputFilename = ''

  def Reformat(self,s):
    assert self.IsGood(s)
    #patt = re.compile("^\(([0-9a-f]+),([0-9a-f]+)\)\s+VERS=\".*\"\s+VR=\"([A-Z][A-Z])\"\s+VM=\"(.*)\"\s+Owner=\".*\"\s+Keyword=\".*\"\s+Name=\"(.*)\"$")
    patt = re.compile("^([A-Za-z0-9 ._>]+) \\(([0-9A-F]+),([0-9A-F]+)\\) [0-9] ([A-Z][A-Z]) ([0-9]) .*$")
    m = patt.match(s)
    dicom = ''
    if m:
      dicom = m.group(2) + ' ' + m.group(3).lower() + ' ' + m.group(4) + ' ' + m.group(5) + ' ' + m.group(1)
    else:
      print "oops"
    return dicom

  def IsGood(self,s):
    #patt = re.compile("^\([0-9a-f]+,[0-9a-f]+\)\s+VERS=\".*\"\s+VR=\"[A-Z][A-Z]\"\s+VM=\".*\"\s+Owner=\".*\"\s+Keyword=\".*\"\s+Name=\".*\".*$")
    patt = re.compile("^[A-Za-z0-9 ._>]+ \\([0-9A-F]+,[0-9A-F]+\\) [0-9] [A-Z][A-Z] [0-9] .*$")
    if patt.match(s):
      return True
    print "Not good:", s
    return False

  def SetInputFileName(self,s):
    self._InputFilename = s

  def SetOutputFileName(self,s):
    self._OutputFilename = s
  
  def Parse(self):
    infile = file(self._InputFilename, 'r')
    outLines = []
    for line in infile.readlines():
      newline = self.Reformat(line)
      #print newline
      if newline:
        outLines.append( newline + '\n' )
    outfile = file(self._OutputFilename, 'w')
    outfile.writelines( outLines )
    outfile.close()
 
if __name__ == "__main__":
  argc = len(os.sys.argv )
  if ( argc < 3 ):
    print "Sorry, wrong list of args"
    os.sys.exit(1) #error

  inputfilename = os.sys.argv[1]
  outputfilename = os.sys.argv[2]
  tempfile = "/tmp/mytemp"
  """
  dp = PdfTextParser()
  dp.SetInputFileName( inputfilename )
  #dp.SetOutputFileName( outputfilename )
  dp.SetOutputFileName( tempfile )
  dp.Parse()
  exp = DicomV3Expander()
  #exp.SetInputFileName( tempfile )
  exp.SetInputFileName( inputfilename )
  exp.SetOutputFileName( outputfilename )
  exp.Expand()

  dp = TransferSyntaxParser()
  dp.SetInputFileName( inputfilename )
  dp.SetOutputFileName( outputfilename )
  dp.Parse()

  dp = PapyrusParser()
  dp.SetInputFileName( inputfilename )
  dp.SetOutputFileName( outputfilename )
  dp.Parse()

  dp = InteraParser()
  dp.SetInputFileName( inputfilename )
  dp.SetOutputFileName( outputfilename )
  dp.Parse()
  dp = GEMSParser()
  dp.SetInputFileName( inputfilename )
  dp.SetOutputFileName( outputfilename )
  dp.Parse()

  """
  dp = Dicom3ToolsParser()
  dp.SetInputFileName( inputfilename )
  dp.SetOutputFileName( outputfilename )
  dp.Parse()

  """
  dp = GEAdvanceParser()
  dp.SetInputFileName( inputfilename )
  dp.SetOutputFileName( outputfilename )
  dp.Parse()
  """

  #print dp.IsAStartingLine( "(0004,1212) File-set Consistency Flag US 1\n" )
