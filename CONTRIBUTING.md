Contributing to ITK
===================

Welcome to the Insight Toolkit (ITK) ! We're excited you're here and want to contribute.

This article documents how to contribute improvements to ITK.

For a *quick start guide*, see the [ITK Git Cheatsheet].

Setup
-----

Before you begin, perform initial setup:

  1. [Register for a GitHub](https://github.com/join) account.
  2. Optionally download our [one page PDF desk reference](https://raw.githubusercontent.com/InsightSoftwareConsortium/ITK/master/Documentation/GitCheatSheet.pdf).
  3. Follow the [download instructions] to create a local ITK clone:

```sh
   $ git clone https://github.com/InsightSoftwareConsortium/ITK
```

  4. Run the developer setup script [`SetupForDevelopment.sh`] to prepare your
     ITK work tree and create Git command aliases used below:

```sh
   $ ./Utilities/SetupForDevelopment.sh
```

This script helps configure your GitHub fork remote, Git client-side hooks,
and useful Git aliases. The default Git remote names for your fork and
`InsightSoftwareConsortium/ITK` are `origin` and `upstream`, respectively.
However, other remote names can be used. Note that ITK defines some useful
Git aliases, such as `review-push`, `pr`, `pr-clean`, and `prepush`, through
the [`setup-git-aliases`] script for general Git tasks in ITK.

Visit the *Pro Git: Setup* resource in [Git Help] for further
information on setting up your local Git environment.

Workflow
--------

ITK development uses a branchy workflow based on topic branches.
This corresponds to the *Fork & Pull Model* mentioned in the
[GitHub flow guide]. Our collaboration workflow consists of
three main steps:

  1. Local Development
     * [Update](#update)
     * [Create a Topic](#create-a-topic)
  2. Code Review
     * [Share a Topic](#share-a-topic)
     * [Test a Topic](#test-a-topic)
     * [Revise a Topic](#revise-a-topic)
  3. Integrate Changes
     * [Merge a Topic](#merge-a-topic)
     * [Delete a Topic](#delete-a-topic)

Update
------

Update your local `master` branch:

```sh
   $ git checkout master
   $ git pullall
```

Create a Topic
--------------

All new work must be committed on topic branches. Name topics like you might
name functions: concise but precise. A reader should have a general idea of the
feature or fix to be developed given just the branch name.

To start a new topic branch:

```sh
   $ git fetch upstream
```

For new development, start the topic from `upstream/master`:

```sh
   $ git checkout -b my-topic upstream/master
```

For release branch fixes, start the topic from `upstream/release`:

```sh
   $ git checkout -b my-topic upstream/release
```

(*You may visit the* Pro Git: Basic Branching *resource in [Git Help] for
further information on working with branches.*)

Edit files and create commits (repeat as needed). Add a prefix to your commit
message (see below).

```sh
   $ edit file1 file2 file3
```
(*To add data follow [these instructions](Documentation/Data.md#add-data).*)

```sh
   $ git add file1 file2 file3
   $ git commit
```

(*You may visit the* Pro Git: Recording Changes *resource in [Git Help] for
further information on making changes and committing snapshots.*)

**Note**: *If your change modifies any of the modules in the
`Modules/ThirdParty` directory, please read our
[Updating Third Party] guide.*

Breaking Changes
----------------

Breaking changes are defined in ITK as those changes that introduce changes to
the API of the [major version](https://semver.org/) of the toolkit, and as
such, make a component of the toolkit no longer backwards compatible. Breaking
changes are only allowed in new major releases. Thus, the change may be held
up by the toolkit's maintainers to ensure consistency in the toolkit. Before
making such changes to the code, and considering other options to keep the
code backward-compatible, please either open an
[issue](https://github.com/InsightSoftwareConsortium/ITK/issues/new/choose)
from the appropriate category or discuss the subject in [ITK's Discourse]. If
the change finally is made into a *pull request*, cross-reference the issue
and/or the discussion with the appropriate link.

Design Changes
--------------

Design changes should be discussed in [ITK's Discourse]. A
[Design Impact
Report](https://github.com/InsightSoftwareConsortium/ITK/issues/new?labels=type%3ADesign&template=design_impact_report.md)
can also be opened to keep track of the requested change. Design changes need
explicit approval from the toolkit's maintainers.

Commit Messages
---------------

Write your commit messages using the standard prefixes for ITK commit
messages:

  * `BUG:` Fix for runtime crash or incorrect result
  * `COMP:` Compiler error or warning fix
  * `DOC:` Documentation change
  * `ENH:` New functionality
  * `PERF:` Performance improvement
  * `STYLE:` No logic impact (indentation, comments)
  * `WIP:` Work In Progress not ready for merge

The body of the message should clearly describe the motivation of the commit
(**what**, **why**, and **how**). In order to ease the task of reviewing
commits, the message body should follow the following guidelines:

  1. Leave a blank line between the subject and the body.
  This helps `git log` and `git rebase` work nicely, and allows to smooth
  generation of release notes.
  2. Try to keep the subject line below 72 characters, ideally 50.
  3. Capitalize the subject line.
  4. Do not end the subject line with a period.
  5. Use the imperative mood in the subject line (e.g. `BUG: Fix spacing
  not being considered.`).
  6. Wrap the body at 80 characters.
  7. Use semantic line feeds to separate different ideas, which improves the
  readability.
  8. Be concise, but honor the change: if significant alternative solutions
  were available, explain why they were discarded.
  9. If the commit refers to a topic discussed in [ITK's Discourse], or fixes
  a regression test, provide the link. If it fixes a compiler error, provide a
  minimal verbatim message of the compiler error. If the commit closes an
  issue, use the [GitHub issue closing
  keywords](https://help.github.com/en/articles/closing-issues-using-keywords).

Keep in mind that the significant time is invested in reviewing commits and
*pull requests*, so following these guidelines will greatly help the people
doing reviews.

These guidelines are largely inspired by Chris Beam's
[How to Write a Commit Message](https://chris.beams.io/posts/git-commit/)
post.

Share a Topic
-------------

When a topic is ready for review and possible inclusion, share it by pushing
to GitHub and opening a *pull request* on the *InsightSoftwareConsortium/ITK*
upstream repository.

Checkout the topic if it is not your current branch:

```sh
   $ git checkout my-topic
```

Check what commits will be pushed to GitHub for review:

```sh
   $ git prepush
```

Push commits in your topic branch for review by the community:

```sh
   $ git review-push --force
```

A URL will be provided in the terminal -- visit this url to review the topic
and open a pull request.

Optionally, discuss the change by opening a topic on [ITK's Discourse].

Test a Topic
------------

When a topic is submitted, it is tested across the three major platforms
before being merged thanks to the [Azure DevOps Pipelines CI
system](https://azure.microsoft.com/en-ca/services/devops/pipelines/),
as well as the [CDash GitHub
Checks](https://github.com/InsightSoftwareConsortium/ITKGitHubCDashStatus),
and [ITK Coding Style
check](https://github.com/InsightSoftwareConsortium/ITKClangFormatLinterAction).

If a platform configuration test failure appears to be a false positive, the
test can be re-executed by adding a comment to the pull request with the
content `/azp run <ConfigurationName>`. For example:

```
    /azp run ITK.Linux
```

After the topic has been merged, it is tested on many
platforms and configurations on the [nightly
dashboard](https://open.cdash.org/index.php?project=Insight).

If tests fail on a submitted topic, see the [Revise a Topic](#revise-a-topic)
step on how to submit a revised version. After a topic is merged, please check
the next day's nightly dashboard to ensure there are not any regressions. If
there are any new warnings or errors, submit a follow-up patch as soon as
possible.

Revise a Topic
--------------

Usually, a topic goes through several revisions in the review process.
Once a topic is approved during GitHub review, proceed to the
[next step](#merge-a-topic).

Checkout the topic if it is not your current branch:

```sh
   $ git checkout my-topic
```

To revise the most recent commit on the topic edit files and add changes
normally and then amend the commit:

```sh
   $ git commit --amend
```

(*You may visit the* Pro Git: Changing the Last Commit *resource in [Git Help]
for further information on revising and rewriting your commit history.*)

To revise commits further back on the topic, say the `3`rd commit back:

```sh
   $ git rebase -i HEAD~3
```

(*Substitute the correct number of commits back, as low as `1`.*)

Follow Git's interactive instructions.

Return to the [Share a Topic](#share-a-topic) step to share the revised topic.

(*You may visit the* Pro Git: Changing Multiple Commits *resource in [Git Help]
for further information on changing multiple commits -i.e. not only the last
one, but further back in your history-, and the* Pro Git: Rebasing *resource on
taking all the changes that were committed on one branch and replaying them on
another one.*)

Merge a Topic
-------------

**Only authorized developers with GitHub merge permissions execute this step.**

After a feature topic has been reviewed and approved in GitHub, ITK
maintainers will merge it into the upstream repository via the GitHub user
interface.

(*If the merge conflicts follow the printed instructions to resolve them.*)

For bug fixes that are ready to be included in the next patch release, make a
comment on the pull request which states the topic should be merged to the
`release` branch.

Here are the recommended steps to merge a topic to both `release` and `master`
branches, assuming the topic branch is forked off the `release` branch:

```sh
   $ git checkout release
   $ git merge --no-ff my-topic
   $ git push upstream release
```

and do:

```sh
   $ git checkout master
   $ git merge --no-ff release
   $ git push upstream master
```

to merge the `release` branch back to `master`.

Delete a Topic
--------------

After a topic has been merged upstream, delete your local branch for the topic.

Checkout and update the `master` branch:

```sh
   $ git checkout master
   $ git pullall
```

Delete the local topic branch:

```sh
   $ git branch -d my-topic
```

The `branch -d` command works only when the topic branch has been correctly
merged. Use `-D` instead of `-d` to force the deletion of an unmerged topic
branch (*warning*: you could lose commits).

Citation Addition
-----------------

To connect your [ORCID](https://orcid.org/) profile to the [ITK Zenodo
citation](https://zenodo.org/record/3592082), add your name and ORCID iD to
the *ITK/.zenodo* file after contributing 10 or more commits.

More Information
----------------

- [ITK Software Guide, Book 1, Part III: Development Guidelines](https://itk.org/ItkSoftwareGuide.pdf)
- General [Git Help]
- [GitHub flow guide]
- [ITK Git Cheatsheet]
- [Updating Third Party] libraries distributed with ITK
- [Handling testing data](Documentation/Data.md)
- [Uploading binary data](Documentation/UploadBinaryData.md)

[ITK Git Cheatsheet]: Documentation/GitCheatSheet.pdf
[download instructions]: Documentation/Download.md
[Git Help]: Documentation/GitHelp.md
[Updating Third Party]: Documentation/Maintenance/UpdatingThirdParty.md

[`SetupForDevelopment.sh`]: https://github.com/InsightSoftwareConsortium/ITK/blob/master/Utilities/SetupForDevelopment.sh
[`setup-git-aliases`]: https://github.com/InsightSoftwareConsortium/ITK/blob/master/Utilities/GitSetup/setup-git-aliases

[ITK's Discourse]: https://discourse.itk.org/

[Git]: http://git-scm.com
[GitHub flow guide]: https://guides.github.com/introduction/flow/index.html
