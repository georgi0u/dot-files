#!/usr/bin/python

import os, shutil, sys

DOT_FILES_DIRECTORY = 'dot-files'
COPY_FILES = set(['.gitconfig'])
LINK_TARGETS = [
    ('.emacs', '.emacs'),
    ('.emacs.d', '.emacs.d'),
    ('.gitconfig', '.gitconfig'),
    ('.gitignore', '.gitignore'),
    ('.inputrc', '.inputrc'),
    ('.tmux.conf', '.tmux.conf'),
    ('.zshrc', '.zshrc'),
    (
        'Library/Application Support/com.mitchellh.ghostty/config.ghostty',
        'Library/Application Support/com.mitchellh.ghostty/config.ghostty',
    ),
    (
        'Library/Application Support/Code/User/settings.json',
        'Library/Application Support/Code/User/settings.json',
    ),
    (
        'Library/Application Support/Code/User/keybindings.json',
        'Library/Application Support/Code/User/keybindings.json',
    ),
]

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

    for source_name, target_name in LINK_TARGETS:
        link_name = os.path.join(user_home_dir, target_name)
        source = os.path.join(dot_files_dir, source_name)
        links.append((source_name, source, link_name))

    if any(fail_if_exists(link_name) for _, _, link_name in links):
        return os.EX_CANTCREAT

    for source_name, source, link_name in links:
        parent_dir = os.path.dirname(link_name)
        if not os.path.isdir(parent_dir):
            os.makedirs(parent_dir)

        if source_name in COPY_FILES:
            shutil.copy2(source, link_name)
        else:
            os.symlink(source, link_name)

    return os.EX_OK

if __name__=="__main__":
    sys.exit(main())
