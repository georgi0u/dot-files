#!/usr/bin/python

import os, shutil, sys

DOT_FILES_DIRECTORY = 'dot-files'

def main():
    script_dir = os.path.dirname(os.path.realpath(__file__))
    user_home_dir = os.path.expanduser('~')

    for element in os.listdir(os.path.join(script_dir, DOT_FILES_DIRECTORY)):
        link_name = os.path.join(user_home_dir, element)
        source = os.path.join(script_dir, DOT_FILES_DIRECTORY, element)

        if os.path.islink(link_name) or os.path.isfile(link_name):
            os.unlink(link_name)
        elif os.path.isdir(link_name):
            shutil.rmtree(link_name)

        os.symlink(source, link_name)

    return os.EX_OK

if __name__=="__main__":
    sys.exit(main())
