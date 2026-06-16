#!/usr/bin/python

import os, shutil, sys

DOT_FILES_DIRECTORY = 'dot-files'
COPY_FILES = set(['.gitconfig'])

def fail_if_exists(path):
    if os.path.lexists(path):
        sys.stderr.write('%s already exists\n' % path)
        return True
    return False

def main():
    script_dir = os.path.dirname(os.path.realpath(__file__))
    user_home_dir = os.path.expanduser('~')
    dot_files_dir = os.path.join(script_dir, DOT_FILES_DIRECTORY)
    links = []

    for element in os.listdir(dot_files_dir):
        link_name = os.path.join(user_home_dir, element)
        source = os.path.join(dot_files_dir, element)
        links.append((element, source, link_name))

    if any(fail_if_exists(link_name) for _, _, link_name in links):
        return os.EX_CANTCREAT

    for element, source, link_name in links:
        if element in COPY_FILES:
            shutil.copy2(source, link_name)
        else:
            os.symlink(source, link_name)

    return os.EX_OK

if __name__=="__main__":
    sys.exit(main())
