#!/usr/bin/env python3

"""List all new authors since the given revision. Only tested on Linux."""

import os
import sys
import subprocess
from pathlib import Path

if len(sys.argv) < 2 or sys.argv[1] == '-h' or sys.argv[1] == '--help':
    usage = "Usage: authors-since.py revision"
    print(usage)
    sys.exit(1)
revision = sys.argv[1]


previous_authors = set()
recent_authors = set()
authors_with_email = set()


def remove_maintainers(authors):
    individual_authors = []
    for author in authors:
        if author.lower().find('maintainer') == -1 and \
           author.lower().find('robot') == -1 and \
           author.lower().find('group') == -1 and \
           author.lower().find('upstream') == -1 and \
           len(author) > 3:
            individual_authors.append(author)
    return set(individual_authors)

def decode_authors(git_output):
    return git_output.decode('utf-8').strip().split('\n')

def update_previous_authors(revision):
    previous_authors_output = subprocess.check_output('git log --pretty=format:"%aN" {0} | sort -u'.format(revision),
                                               shell=True)
    previous_authors.update(decode_authors(previous_authors_output))

def update_recent_authors(revision):
    recent_authors_out = subprocess.check_output('git log --pretty=format:"%aN" {0} | sort -u'.format(revision),
                                              shell=True)
    recent_authors.update(decode_authors(recent_authors_out))

def update_authors_with_email(revision):
    authors_with_email_out = subprocess.check_output('git log --pretty=format:"%aN <%aE>" {0} | sort -u'.format(revision),
                                                  shell=True)
    authors_with_email.update(decode_authors(authors_with_email_out))

def remote_repository(remote_spec):
    for line in remote_spec.split('\n'):
        split = line.split()
        try:
            tag_index = split.index('GIT_REPOSITORY')
            return split[tag_index + 1].replace('${git_protocol}', 'https').strip()
        except ValueError:
            continue

    print('GIT_REPOSITORY not found!')
    print('\nRemote spec:')
    print(remote_spec)
    sys.exit(1)

def remote_tag(remote_spec):
    for line in remote_spec.split('\n'):
        split = line.split()
        try:
            tag_index = split.index('GIT_TAG')
            return split[tag_index + 1].strip()
        except ValueError:
            continue

    print('GIT_TAG not found!')
    print('\nRemote spec:')
    print(remote_spec)
    sys.exit(1)

scratch_dir = Path('/tmp/AuthorsChangesSince')
if not scratch_dir.exists():
    os.makedirs(scratch_dir)

changelog_file = scratch_dir / 'Changelog.txt'
with open(changelog_file, 'w') as fp:
    fp.write('')
def write_changelog(repo_name, git_revision):
    log = subprocess.check_output('git shortlog --topo-order --no-merges {0}'.format(git_revision), shell=True).decode('utf-8')
    with open(changelog_file, 'a') as fp:
        fp.write('{0} Changes Since {1}\n'.format(repo_name, revision))
        fp.write('---------------------------------------------\n\n')
        fp.write('```\n')
        fp.write(log)
        fp.write('```\n')
        fp.write('\n\n')

revision_time = subprocess.check_output('git show -s --format="%ci" {0}^{{commit}}'.format(revision), shell=True)
revision_time = revision_time.decode('utf-8').strip()
print('Revision time: ' + revision_time + '\n')
itk_dir = Path(os.path.dirname(os.path.abspath(__file__))) / '..' / '..'

# ITK Repository
update_previous_authors(revision)
update_recent_authors(revision + '..')
update_authors_with_email(revision + '..')
write_changelog('ITK', revision + '..')

# ITKExamples Repository
os.chdir(scratch_dir)
examples_dir = scratch_dir / 'ITKExamples'
if not examples_dir.exists():
    subprocess.check_call('git clone https://github.com/InsightSoftwareConsortium/ITKExamples', shell=True)
os.chdir(examples_dir)
update_previous_authors('--until="{0}"'.format(revision_time))
update_recent_authors('--since="{0}"'.format(revision_time))
update_authors_with_email('--since="{0}"'.format(revision_time))
write_changelog('ITK Examples', '--since="{0}"'.format(revision_time))

# ITKSoftwareGuide Repository
os.chdir(scratch_dir)
examples_dir = scratch_dir / 'ITKSoftwareGuide'
if not examples_dir.exists():
    subprocess.check_call('git clone https://github.com/InsightSoftwareConsortium/ITKSoftwareGuide', shell=True)
os.chdir(examples_dir)
update_previous_authors('--until="{0}"'.format(revision_time))
update_recent_authors('--since="{0}"'.format(revision_time))
update_authors_with_email('--since="{0}"'.format(revision_time))
write_changelog('ITK Software Guide', '--since="{0}"'.format(revision_time))

# Remote modules
os.chdir(itk_dir)
changed_remotes = subprocess.check_output('git diff-index --diff-filter=AM --name-only {0} -- Modules/Remote/'.format(revision), shell=True).decode('utf-8').strip()
with open(changelog_file, 'a') as fp:
    fp.write('Remote Module Changes Since {0}\n'.format(revision))
    fp.write('---------------------------------------------\n\n')
print('Remote module:')
for remote in changed_remotes.split():
    module_name = remote.split('/')[-1].split('.')[0]
    if module_name == 'SphinxExamples' or module_name == 'CMakeLists':
        continue
    print(module_name)
    os.chdir(itk_dir)

    # The remote file could have been added or its name changed. Use the oldest
    # commit since revision with the current name.
    old_commit = subprocess.check_output('git rev-list {0}.. -- {1}'.format(revision, remote),
            shell=True).decode('utf-8').split()[-1]
    try:
        remote_spec = subprocess.check_output('git show {0}^:{1}'.format(old_commit,
            remote), shell=True).decode('utf-8')
    except subprocess.CalledProcessError:
        remote_spec = subprocess.check_output('git show {0}:{1}'.format(old_commit,
            remote), shell=True).decode('utf-8')

    remote_old_tag = remote_tag(remote_spec)

    remote_spec = subprocess.check_output('git show HEAD:{1}'.format(revision,
        remote), shell=True).decode('utf-8')
    remote_new_tag = remote_tag(remote_spec)
    remote_repo = remote_repository(remote_spec)

    os.chdir(scratch_dir)
    remote_dir = scratch_dir / remote_repo.split('/')[-1]
    if not remote_dir.exists():
        subprocess.check_call('git clone {0} {1}'.format(remote_repo, remote_dir),
                shell=True)
    os.chdir(remote_dir)
    # update_previous_authors('..{0}'.format(remote_new_tag))
    update_recent_authors('{0}..{1}'.format(remote_old_tag, remote_new_tag))
    update_authors_with_email('{0}..{1}'.format(remote_old_tag, remote_new_tag))

    log = subprocess.check_output('git shortlog --topo-order --no-merges {0}..{1}'.format(remote_old_tag, remote_new_tag), shell=True).decode('utf-8')
    with open(changelog_file, 'a') as fp:
        fp.write('*{0}*:\n'.format(module_name))
        fp.write('```\n')
        fp.write(log)
        fp.write('```\n')
        fp.write('\n')

os.chdir(scratch_dir)

recent_authors = remove_maintainers(recent_authors)
authors_with_email = remove_maintainers(authors_with_email)
previous_authors = remove_maintainers(previous_authors)
new_authors = recent_authors.difference(previous_authors)

# Print results
print('\n\nPrevious authors: ' + str(len(previous_authors)))
print('Recent authors: ' + str(len(recent_authors)))
print('\nNew authors: ' + str(len(new_authors)))
with open(scratch_dir / 'NewAuthors.txt', 'w') as fp:
    for author in new_authors:
        sys.stdout.write(author + ', ')
        fp.write(author + ', ')

recent_authors_with_email = []
for author in authors_with_email:
    for an in recent_authors:
        if author.find(an) != -1:
            recent_authors_with_email.append(author)
print('\n\nRecent with emails:')
with open(scratch_dir / 'RecentWithEmails.txt', 'w') as fp:
    for author in recent_authors_with_email:
        sys.stdout.write(author + ', ')
        fp.write(author + ', ')

print('\n\nWith emails oneline:')
with open(scratch_dir / 'WithEmailsOneline.txt', 'w') as fp:
    for author in authors_with_email:
        sys.stdout.write(author + '\n')
        fp.write(author + '\n')

print('\n\nNames:')
with open(scratch_dir / 'Names.txt', 'w') as fp:
    for author in authors_with_email:
        sys.stdout.write(author.split('<')[0] + '\n')
        fp.write(author.split('<')[0] + '\n')

print('\n\nEmails:')
with open(scratch_dir / 'Emails.txt', 'w') as fp:
    for author in authors_with_email:
        sys.stdout.write(author.split('<')[1][:-1] + '\n')
        fp.write(author.split('<')[1][:-1] + '\n')
