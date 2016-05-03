#!/usr/bin/env python
#
# InitializeXMLGuide.py
#
# This script will parse the Git commits on the current branch and initializes
# an XML migration guide document with the apropriate content.

from __future__ import print_function

import os.path
import sys
import subprocess
import datetime

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
# add a comment to the XML file string
def addXMLComment(xmlString, comment, indent=False):
  # comment
  xmlString = xmlString + getIndent(indent) + "<!--**\n"
  for line in comment.splitlines():
    xmlString = xmlString + getIndent(indent) + "** " + line + "\n"
  xmlString = xmlString + getIndent(indent) + "**-->\n"
  return xmlString

#
# add an element to the XML file string
def addXMLElement(xmlString, elementName, elementText, indent=False, comment = ""):
  # comment
  if comment != "":
    xmlString = addXMLComment(xmlString, comment, indent)
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
  baseDir = stripSuffix(migrationDir, "/Documentation/Migration")
  # get XMLFileName (loop until name is unique)
  uniqueName = False
  XMLFileName = ""
  XMLFilePath = ""
  print("Please enter a unique name for the XML document")
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
      print('"' + nameCandidate + '" already exists.'
            'Please specify a different name')

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
    print("I/O error: Could not open .git/HEAD")
    sys.exit()
  branchName = HEADfile.readline()
  branchName = stripPrefix(branchName, "ref: refs/heads/")
  branchName = stripSuffix(branchName, "\n")

  # parse file in .git/logs/refs/heads/BRANCH_NAME
  branchLogFilename = baseDir + "/.git/logs/refs/heads/" + branchName
  try:
    branchLogFile = open(branchLogFilename, "r")
  except IOError:
    print("I/O error: Could not open branch log file")
    sys.exit()

  # grab the commit lines, but ignore ammended ones
  logCommand = "git log --format=format:%H origin/master.."
  log = runCommand(logCommand)
  commitList = [l.strip() for l in log.splitlines()]

  # grab the base commit, the one used to create the branch
  logCommand = "git log -n1 --format=format:%P " + commitList[-1]
  log = runCommand(logCommand)
  baseCommit = log.strip()

  #
  # Parse each commit's log
  #
  descriptionText = "---- REMOVE THIS LINE -- This element should contain an English description of what changes were made along with rational for making them. ----\n"
  descriptionText += "---- REMOVE THIS LINE -- The commit log should help you to fill this field. ----\n"
  changeIdText = ""
  sampleCodeOldList = []
  sampleCodeNewList = []
  changedFileList = []
  exampleAndTestChangedFileList = []

  for commit in commitList:

    # get the log for the commit
    logCommand = "git log -n1 --format=format:%s%n%b " + commit
    log = runCommand(logCommand)

    descriptionText = descriptionText + "---- REMOVE THIS LINE -- Log from commit " + commit + " ----\n"

    for line in log.splitlines():
      # commit message lines and change id lines
      if startsWith(line.strip(), "Change-Id: "):
        changeId = stripPrefix(line.strip(), "Change-Id: ")
        changeIdText = changeIdText + changeId + "\n"
      else:
        descriptionText = descriptionText + line.strip() + '\n'

    # get the modified file list for the commit
    logCommand = "git log -n1 --name-only --format=format: " + commit
    log = runCommand(logCommand)
    for line in log.splitlines():
      # changed file lines
      filename = line.strip()
      if filename:
        if not filename in changedFileList:
          changedFileList.append(filename)
        if startsWith(filename, "Examples") or startsWith(filename, "Testing"):
          exampleAndTestChangedFileList.append(filename)


  #
  # parse the diff of each Example or Testing file
  #
  print(exampleAndTestChangedFileList)
  for filename in exampleAndTestChangedFileList:

    # Add filename
    sampleCodeOldText = "---- REMOVE THIS LINE -- Code from " + filename + " ----\n"
    sampleCodeNewText = "---- REMOVE THIS LINE -- Code from " + filename + " ----\n"

    # get the log for the commit
    fullPath = baseDir + "/" + filename
    diffCommand = "git diff " + baseCommit + " -- " + fullPath
    print(diffCommand)
    diff = runCommand(diffCommand)

    # parse lines into old and new
    for line in diff.splitlines():

      # old line
      if (not startsWith(line, "--")) and startsWith(line, "-"):
        sampleCodeOldText = sampleCodeOldText + line.lstrip("-").strip() + "\n"

      # new line
      elif (not startsWith(line, "++")) and startsWith(line, "+"):
        sampleCodeNewText = sampleCodeNewText + line.lstrip("+").strip() + "\n"

    sampleCodeOldList.append(sampleCodeOldText)
    sampleCodeNewList.append(sampleCodeNewText)
  #
  # Create XMl file text
  #

  xmlString = '<?xml version="1.0" encoding="UTF-8"?>\n<!DOCTYPE Change SYSTEM "https://itk.org/migrationv4/ITKMigration.dtd">\n\n'
  changeElementBody = ""

  # <Title> element
  titleComment = "Title for the online migration page"
  changeElementBody = addXMLElement(changeElementBody, "Title", titleText, True, titleComment)

  # <Author> element
  authorName = runCommand("git log -n1 --format=format:%an")
  authorComment = "The author of the change"
  changeElementBody = addXMLElement(changeElementBody, "Author", authorName, True, authorComment)

  # <Date> element
  date = datetime.datetime.now().strftime("%Y-%m-%d")
  dateComment = "Date of creation for the XML document"
  changeElementBody = addXMLElement(changeElementBody, "Date", date, True, dateComment)

  # <Description> element
  descriptionComment = "Plain text description of the change\nExtracted from git commit messages"
  changeElementBody = \
    addXMLElement(changeElementBody, "Description",\
    "<![CDATA[\n" + prepXMLString(descriptionText) + "]]>\n", True, descriptionComment)

  # <SampleCode> element
  if len(sampleCodeOldList) == 0:
    sampleCodeOldText = ""
    sampleCodeNewText = ""
  else:
    sampleCodeOldText = sampleCodeOldList[0]
    sampleCodeNewText = sampleCodeNewList[0]

  sampleCodeComment = "Sample code snippets\nExtracted from git diff of changed files in Examples and Testing"
  sampleCodeElementBody = "---- REMOVE THIS LINE -- This element should contain some code snippet that illustrates how to update the API from the old version to the new version. ----\n"
  sampleCodeElementBody += "---- REMOVE THIS LINE -- The changes extracted from the Examples and Testing directory should help you to fill this field. ----\n"
  sampleCodeElementBody += "---- REMOVE THIS LINE -- You can set multiple SampleCode tags. ----\n"
  sampleCodeElementBody = \
    addXMLElement(sampleCodeElementBody, "Old", "<![CDATA[\n" + prepXMLString(sampleCodeOldText) + "]]>\n")
  sampleCodeElementBody = \
    addXMLElement(sampleCodeElementBody, "New", "<![CDATA[\n" + prepXMLString(sampleCodeNewText) + "]]>\n")
  changeElementBody = addXMLElement(changeElementBody, "SampleCode",\
                                  sampleCodeElementBody, True, sampleCodeComment)

  i = 1
  while i<len(sampleCodeOldList):
    sampleCodeOldText = sampleCodeOldList[i]
    sampleCodeNewText = sampleCodeNewList[i]
    sampleCodeComment = ""
    sampleCodeElementBody = ""
    sampleCodeElementBody = \
      addXMLElement(sampleCodeElementBody, "Old", "<![CDATA[\n" + prepXMLString(sampleCodeOldText) + "]]>\n")
    sampleCodeElementBody = \
      addXMLElement(sampleCodeElementBody, "New", "<![CDATA[\n" + prepXMLString(sampleCodeNewText) + "]]>\n")
    changeElementBody = addXMLElement(changeElementBody, "SampleCode",\
                                    sampleCodeElementBody, True, sampleCodeComment)
    i += 1

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

  # <MigrationFix-Automatic> comment
  autoFixComment = "If the migration can be accomplished by a simple string\nsubstitution, then use the following construct to define\nthe substitution rule.\n\n<MigrationFix-Automatic>\n  <Old>\n    <![CDATA[MipsleledName]]>\n  </Old>\n  <New>\n    <![CDATA[MisspelledName]]>\n  </New>\n</MigrationFix-Automatic>"
  changeElementBody = addXMLComment(changeElementBody, autoFixComment, True) +"\n"

  # <MigrationFix-Manual> comment
  manFixComment = "If the migration can NOT be accomplished by a simple string\nsubstitution, but potential problem spots can be identified,\nuse the following construct to define a migration flag rule.\n\n<MigrationFix-Manual>\n  OldFunctionName\n</MigrationFix-Manual>"
  changeElementBody = addXMLComment(changeElementBody, manFixComment, True) + "\n"

  # <Change> element
  changeComment = "\n" + XMLFileName + "\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>\nTHIS FILE HAS BEEN AUTOMATICALLY GENERATED. EDIT IT BEFORE COMMITING\n<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\nPlease, make sure this file validates the following w3c test before committing it: http://validator.w3.org"
  xmlString = addXMLElement(xmlString, "Change", changeElementBody, False, changeComment)

  # drop the blanks at the end of the lines to please git's hooks
  xmlString = "\n".join([l.rstrip() for l in xmlString.splitlines()])

  #
  # Save the file
  #
  try:
    xmlFile = open(XMLFilePath, "w")
    xmlFile.write(xmlString)
  except IOError:
    print("I/O Error: Could not write to " + XMLFilePath)
    sys.exit()
