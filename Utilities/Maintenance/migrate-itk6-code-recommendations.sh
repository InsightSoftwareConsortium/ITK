#!/bin/bash
# Update code to new macro names
#
# script for updating all the remote modules to update
# to the latest ITK.
# This script is also useful for updating other packages
# 	BRAINSTools
# 	ANTs
# 	Slicer
# 	....
cat > /dev/null << EOF
cd ~/Dashboard/src/ITK/Modules/Remote
for gg in ~/Dashboard/src/ITK/Moudules/Remote/*/.git; do
  cd $(dirname $gg)
  bash  ~/Dashboard/src/ITK/Utilities/Maintenance/migrate-itk6-code-recommendations.sh $(pwd)
  cd ~/Dashboard/src/ITK/Modules/Remote
done
EOF

directory_to_convert=$(realpath $1)

if [ ! -d "${directory_to_convert}" ]; then
   echo "File to convert is missing: ${directory_to_convert}"
   exit -1
fi

if [ "$(uname -s)" == "Darwin" ]; then
	SEDBIN=gsed
else
	SEDBIN=sed
fi

cd "${directory_to_convert}"

git remote -v |grep upstream
has_upstream=$?
if [ ${has_upstream} -eq 0 ]; then
	primaryrepo="upstream"
else
	primaryrepo="upstream"
fi

git fetch origin
git remote -v |grep upstream && git fetch upstream
default_branch_name=$(git branch -l main master --format '%(refname:short)')
git remote -v |grep upstream && git push origin upstream/${default_branch_name}:${default_branch_name} -f
git fetch origin
git rebase origin/${default_branch_name}

files_modified=0
for ff in $(git grep -l itkTypeMacro) $(git grep -l itkTypeMacroNoParent); do
   echo "Modifying $ff"
   ${SEDBIN} -i 's/itkTypeMacro *(\(.*\),.*) *;/itkOverrideGetNameOfClassMacro(\1);/g' "${ff}"
   ${SEDBIN} -i 's/itkTypeMacroNoParent *(\(.*\),.*) *;/itkVirtualGetNameOfClassMacro(\1);/g' "${ff}"
   git add ${ff}
   files_modified=1
done

if [ ${files_modified} -ne 0 ] ; then
git checkout -b update-to-new-macros

git commit -m"STYLE: Add itkVirtualGetNameOfClassMacro + itkOverrideGetNameOfClassMacro

Added two new macro's, intended to replace the old 'itkTypeMacro' and
'itkTypeMacroNoParent'.

The main aim is to be clearer about what those macro's do: add a virtual
'GetNameOfClass()' member function and override it. Unlike 'itkTypeMacro',
'itkOverrideGetNameOfClassMacro' does not have a 'superclass' parameter, as it
was not used anyway.

Note that originally 'itkTypeMacro' did not use its 'superclass' parameter
either, looking at commit 699b66cb04d410e555656828e8892107add38ccb, Will
Schroeder, June 27, 2001:
https://github.com/InsightSoftwareConsortium/ITK/blob/699b66cb04d410e555656828e8892107add38ccb/Code/Common/itkMacro.h#L331-L337
"

gh pr create
fi

## Now change ITK_DISALLOW_COPY_AND_MOVE

#Replace deprecated ITK_DISALLOW_COPY_AND_ASSIGN with modern ITK_DISALLOW_COPY_AND_MOVE
files_modified=0
for ff in $(git grep -l ITK_DISALLOW_COPY_AND_ASSIGN); do
   echo "Modifying $ff to update to ITK_DISALLOW_COPY_AND_MOVE"
   ${SEDBIN} -i 's/ITK_DISALLOW_COPY_AND_ASSIGN/ITK_DISALLOW_COPY_AND_MOVE/g' "${ff}"
   git add ${ff}
   files_modified=1
done

if [ ${files_modified} -ne 0 ] ; then
git checkout -b use-new-dissallow-copy-and-move

git commit -m"STYLE: Rename ITK_DISALLOW_COPY_AND_ASSIGN to ITK_DISALLOW_COPY_AND_MOVE

Clarifies that the macro does not just disallow copy and assign, but
also move operations. Note that in this context, the term 'move' refers
to both move-construct and move-assign.

With this commit, the old macro name will remain available, as long as
'ITK_FUTURE_LEGACY_REMOVE = OFF' (which is the default).
"

gh pr create
fi

## Now replace Replace itkStaticConstMacro with static constexpr

files_modified=0
for ff in $(git grep -l itkStaticConstMacro) $(git grep -l itkGetStaticConstMacro); do
   echo "Modifying $ff to update to itkStaticConstMacro"
   ${SEDBIN} -i 's/itkStaticConstMacro *( *\([^,]*\),[ \_s]*\([^,]*\),[ \_s]*\([^)]*\)) */static constexpr \2 \1 = \3/g' "${ff}"
   ${SEDBIN} -i 's/itkGetStaticConstMacro *(\(.*\))/Self::\1/g' "${ff}"

   git add ${ff}
   files_modified=1
done

if [ ${files_modified} -ne 0 ] ; then
git checkout -b replace-itkstaticconstmacro-with-constexpr

git commit -m"STYLE: Replace itkStaticConstMacro with static constexpr

Use static constexpr directly now that C++11 conformance
is required by all compilers.

:%s/itkStaticConstMacro *( *\([^,]*\),[ \_s]*\([^,]*\),\_s*\([^)]*\)) */static constexpr \2 \1 = \3/ge

'itkStaticConstMacro(name, type, value)' became unconditionally
identical to 'static constexpr type name = value' with ITK commit
aec95193ab00e1322039911e1032da00f3a103b6 \"ENH: Update compiler macros (#810)\",
maekclena, 7 May 2019.

'itkGetStaticConstMacro(name)' became unconditionally identical to
'(Self::name)' with ITK commit 84e490b81e3f3c2b0edb89ae7b9de53bfc52f2b2
\"Removing some outdated compiler conditionals\", Hans Johnson, 31 July
2010.

Most 'itkStaticConstMacro' calls were removed by ITK commit 5c14741e1e063a132ea7e7ee69c5bd0a4e49af74
"

gh pr create
fi

## Now replace Replace ITKv5_CONST must be replaced with 'const'

files_modified=0
for ff in $(git grep -l ITKv5_CONST) $(git grep -l ITKv5_CONST); do
   echo "Modifying $ff to update to ITKv5_CONST"
   ${SEDBIN} -i 's/ITKv5_CONST/const/g' "${ff}"

   git add ${ff}
   files_modified=1
done

if [ ${files_modified} -ne 0 ] ; then
git checkout -b replace_ITKv5_CONST-with-const

git commit -m"ENH: ITKv5_CONST macro for VerifyPreconditions() and VerifyInputInformation()

ITKv5_CONST enables backwards compatible behavior when ITKV4_COMPATIBILITY
is turned ON for methods which have acquired 'const' qualifier in ITKv5.
Breaking changes were originally introduced by ITK  commits
3e6b6f5bd6772316aa0fa6b89f31b42bef562112 (on 2018-10-18) and
16eae15c1bb6cc1bae9fba3e09a3102bdc02e955 (on 2018-10-23).
"

gh pr create
fi

## Now replace Replace Co

files_modified=0
for ff in $(git grep -l CoordRepType) $(git grep -l InputCoordRepType); do
   echo "Modifying $ff to update to CoordinateType"
   ${SEDBIN} -i 's/ImagePointCoordRepType/ImagePointCoordinateType/g' "${ff}"
   ${SEDBIN} -i 's/InputCoordRepType/InputCoordinateType/g' "${ff}"
   ${SEDBIN} -i 's/OutputCoordRepType/OutputCoordinateType/g' "${ff}"
   ${SEDBIN} -i 's/CoordRepType/CoordinateType/g' "${ff}"

   git add ${ff}
   files_modified=1
done

if [ ${files_modified} -ne 0 ] ; then
git checkout -b use-CoordinateType

git commit -m"STYLE: CoordRepType -> CoordinateType code readability

For the sake of code readability, a new 'CoordinateType' alias is added for
each nested 'CoordRepType' alias. The old 'CoordRepType' aliases will still be
available with ITK 6.0, but it is recommended to use 'CoordinateType' instead.
The 'CoordRepType' aliases will be removed when 'ITK_FUTURE_LEGACY_REMOVE' is
enabled. Similarly, 'InputCoordinateType', 'OutputCoordinateType', and
'ImagePointCoordinateType' replace 'InputCoordRepType', 'OutputCoordRepType',
and 'ImagePointCoordRepType', respectively.
"

gh pr create
fi

git branch -v |fgrep use-new-dissallow-copy-and-move:use-new-dissallow-copy-and-move && git push -f --set-upstream origin use-new-dissallow-copy-and-move:use-new-dissallow-copy-and-move
git branch -v |fgrep update-to-new-macros:update-to-new-macros                       && git push -f --set-upstream origin update-to-new-macros:update-to-new-macros
git branch -v |fgrep replace-itkstaticconstmacro-with-constexpr                      && git push -f --set-upstream origin replace-itkstaticconstmacro-with-constexpr
git branch -v |fgrep replace_ITKv5_CONST-with-const                                  && git push -f --set-upstream origin replace_ITKv5_CONST-with-const
git branch -v |fgrep use-CoordinateType                                              && git push -f --set-upstream origin use-CoordinateType
