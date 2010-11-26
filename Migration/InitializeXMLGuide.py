# InitializeXMLGuide.py
#
# This script will parse the Git commits on the current branch and initializes
# an XML migration guide document with the apropriate content.

import os.path
import sys
import subprocess

####################
# Helper Functions #
####################

#
# strip the prefix from the string in order
def stripPrefix(string, prefix):
  if string[0:len(prefix)] == prefix:
    return string[len(prefix):len(string)]
  else:
    return string

#
# strip the suffix from the string in order
def stripSuffix(string, suffix):
  if string[len(string)-len(suffix):len(string)] == suffix:
    return string[0:len(string)-len(suffix)]
  else:
    return string

#
# test the start of a string
def startsWith(string, prefix):
  return (string.find(prefix) == 0)

#
# test the end of a string
def endsWith(string, suffix):
  return (string.rfind(suffix) == len(string) - len(suffix))

#
# get the output of an external command
def runCommand(command):
  args = command.split()
  process = subprocess.Popen(args, stdout = subprocess.PIPE)
  while process.returncode == None:
    process.poll()
  return process.communicate()[0]

#
# add proper number of indents
def getIndent(indent):
  if indent:
    return "  "
  else:
    return ""

#
# sub out protected characters
def prepXMLString(string):
  out = string.replace("<", "&lt;")
  out = out.replace("&", "&amp;")
  out = out.replace(">", "&gt;")
  out = out.replace('"', "&quot;")
  return out.replace("'", "&apos;")

#
# add an element to the XML file string
def addXMLElement(xmlString, elementName, elementText, indent=False, comment = ""):
  # comment
  if comment != "":
    xmlString = xmlString + getIndent(indent) + "<!--**\n"
  for line in comment.splitlines():
    xmlString = xmlString + getIndent(indent) + "** " + line + "\n"
  if comment != "":
    xmlString = xmlString + getIndent(indent) + "**-->\n"
  # opening tag
  xmlString = xmlString + getIndent(indent) + "<" + elementName + ">\n"
  # body
  for line in elementText.splitlines():
    xmlString = xmlString + getIndent(indent) + "  " + line + "\n"
  # closing tag
  return xmlString + getIndent(indent) + "</" + elementName + ">\n\n"

#
# add spaces for camel case string
def addCamelCaseSpaces(string):
  firstLetter = True
  out = ""
  for letter in string:
    if not firstLetter and letter.isupper():
      out = out + " " + letter
    else:
      out = out + letter
    firstLetter = False
  return out


########################
# Main execution point #
########################
if __name__ == '__main__':

  #
  # Get a unique XMLFileName from the user
  #

  # get directory info
  migrationDir = sys.path[0]
  baseDir = stripSuffix(migrationDir, "/Migration")

  # get XMLFileName (loop until name is unique)
  uniqueName = False
  XMLFileName = ""
  XMLFilePath = ""
  print "Please enter a unique name for the XML document"
  while uniqueName == False:

    # get the user's input
    nameCandidate = raw_input(">> ")
    if not endsWith(nameCandidate, ".xml"):
      nameCandidate = nameCandidate + ".xml"
    pathCandidate = migrationDir + "/" + nameCandidate

    # check uniqueness
    if not os.path.exists(pathCandidate):
      uniqueName = True
      XMLFileName = nameCandidate
      XMLFilePath = pathCandidate
    else:
      print '"' + nameCandidate + '" already exists.  Please specify a differnt name'

  # split the name for the title tag
  titleText = addCamelCaseSpaces(stripSuffix(XMLFileName, ".xml"))

  #
  # Get list of commits on current branch
  #

  # Get the current branch name
  HEADfilename = baseDir + "/.git/HEAD"
  try:
    HEADfile = open(HEADfilename, "r")
  except IOError:
    print "I/O error: Could not open .git/HEAD"
    sys.exit()
  branchName = HEADfile.readline()
  branchName = stripPrefix(branchName, "ref: refs/heads/")
  branchName = stripSuffix(branchName, "\n")

  # parse file in .git/logs/refs/heads/BRANCH_NAME
  branchLogFilename = baseDir + "/.git/logs/refs/heads/" + branchName
  try:
    branchLogFile = open(branchLogFilename, "r")
  except IOError:
    print "I/O error: Could not open branch log file"
    sys.exit()

  # grab the commit lines, but ignore ammended ones
  commitList = []
  for line in branchLogFile.readlines():
    if line.find("commit (amend):") == -1:
      commitList.append(line.split()[1])
    else:
      commitList.pop()
      commitList.append(line.split()[1])

  #
  # Parse each commit's log
  #
  descriptionText = ""
  changeIdText = ""
  sampleCodeOldText = ""
  sampleCodeNewText = ""
  changedFileList = []
  exampleAndTestChangedFileList = []

  firstCommit = commitList[0];
  commitList.remove(firstCommit)

  for commit in commitList:

    # get the log for the commit
    logCommand = "git log " + commit + " -n1 --stat"
    log = runCommand(logCommand)

    descriptionText = descriptionText + "---- " + commit + " ----\n"

    for line in log.splitlines():

      # commit message lines and change id lines
      if startsWith(line, "  "):
        if startsWith(line.strip(), "Change-Id: "):
          changeId = stripPrefix(line.strip(), "Change-Id: ")
          changeIdText = changeIdText + changeId + "\n"
        else:
          descriptionText = descriptionText + line.strip() + '\n'

      # changed file lines
      elif startsWith(line, " "):
        splits = line.split("|")
        if len(splits) == 2:
          filename = splits[0].strip()
          if not filename in changedFileList:
            changedFileList.append(filename)
          if startsWith(filename, "Examples") or startsWith(filename, "Testing"):
            exampleAndTestChangedFileList.append(filename)


  #
  # parse the diff of each Example or Testing file
  #
  for filename in exampleAndTestChangedFileList:

    # Add filename
    sampleCodeOldText = sampleCodeOldText + "---- " + filename + " ----\n"
    sampleCodeNewText = sampleCodeNewText + "---- " + filename + " ----\n"

    # get the log for the commit
    fullPath = baseDir + "/" + filename
    diffCommand = "git diff " + firstCommit + " " + commitList[len(commitList)-1] \
      + " -- " + fullPath
    diff = runCommand(diffCommand)

    # parse lines into old and new
    for line in diff.splitlines():

      # old line
      if (not startsWith(line, "--")) and startsWith(line, "-"):
        sampleCodeOldText = sampleCodeOldText + line.lstrip("-").strip() + "\n"

      # new line
      elif (not startsWith(line, "++")) and startsWith(line, "+"):
        sampleCodeNewText = sampleCodeNewText + line.lstrip("+").strip() + "\n"


  #
  # Create XMl file text
  #

  xmlString = '<?xml version="1.0" encoding="UTF-8"?>\n\n'
  changeElementBody = ""

  # <Title> element
  titleComment = "Title for the online migration page"
  changeElementBody = addXMLElement(changeElementBody, "Title", titleText, True, titleComment)

  # <Description> element
  descriptionComment = "Plain text description of the change\n-->Extracted from git commit messages"
  changeElementBody = \
    addXMLElement(changeElementBody, "Description",\
    prepXMLString(descriptionText), True, descriptionComment)

  # <SampleCode> element
  sampleCodeComment = "Sample code snippets\n-->Extracted from git diff of changed files in Examples and Testing"
  sampleCodeElementBody = ""
  sampleCodeElementBody = \
    addXMLElement(sampleCodeElementBody, "Old", prepXMLString(sampleCodeOldText))
  sampleCodeElementBody = \
    addXMLElement(sampleCodeElementBody, "New", prepXMLString(sampleCodeNewText))
  changeElementBody = addXMLElement(changeElementBody, "SampleCode",\
                                    sampleCodeElementBody, True, sampleCodeComment)

  # <Gerrit-ChangeId> element
  changeIdComment = "The change-ids for all commits in the topic branch"
  changeElementBody = \
    addXMLElement(changeElementBody, "Gerrit-ChangeId",\
    changeIdText, True, changeIdComment)

  # <FileList> element
  fileListComment = "List of all changed files from the topic branch"
  fileListText = ""
  for f in changedFileList:
    fileListText = fileListText + f + "\n"
  changeElementBody = \
    addXMLElement(changeElementBody, "FileList",\
    fileListText, True, fileListComment)

  # <Change> element
  changeComment = "\n" + XMLFileName + "\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>\nTHIS FILE HAS BEEN AUTOMATICALLY GENERATED. EDIT IT BEFORE COMMITING\n<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n"
  xmlString = addXMLElement(xmlString, "Change", changeElementBody, False, changeComment)
  xmlString = xmlString.strip()

  #
  # Save the file
  #
  try:
    xmlFile = open(XMLFilePath, "w")
    xmlFile.write(xmlString)
  except IOError:
    print "I/O Error: Could not write to " + XMLFilePath
    sys.exit()
