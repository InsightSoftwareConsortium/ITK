#!/usr/bin/bash
#
#  This script is intended to be run from the top
#  of the ITK source tree.
#
#  With a call similar to:
#
#  cd  /home/ibanez/src/ITK
#
#  ./Utilities/DevelopmentSetupScripts/AddTestingData.sh  BUG_19923  image1.png image2.png image3.jpg
#
#

#
#  The first argument (se.g. BUG_19923) is used to
#  create a local branch. It should be a string that
#  describes the change.
#

change_identifier_string=$1

git checkout -b $change_identifier_string origin/master
shift 1

#
# We update the ITK repository, and the Testing/Data one.
#
git submodule update
cd Testing/Data
git config remote.origin.pushurl git@itk.org:ITKData.git
git checkout master
git pull --rebase origin master

#
#  Add and commit all the files that have been modified
#
git add -- $*
git commit -m "$change_identifier_string"
git push origin master

#
#  Now update the reference in the ITK repository
#
cd ../..
git add -- Testing/Data
git commit -m "$change_identifier_string"

#
#  Finally push it to the stage
#
git stage-push
