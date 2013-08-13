#!/usr/bin/python

# This goes through all the files, looks for a given number and replaces it with another
# 
# run from the comand line using ./set_array_length.py
#
# requires python and the standard libraries

# ############################################################################ #
import os
import sys
import re
import math
from glob import glob

# ############################################################################ #
# general replacement functions

def replace_value (lines,old_str,new_str):
    """
    This replaces the values in each line for a file
    """
    out_lines = []
    changed_lines = []
    no_change = True
    for line in lines:        
        new_line = line.replace(old_str,new_str)
        # if line was changed
        if new_line != line:
            changed_lines.append(new_line.rstrip())
            no_change = False
        out_lines.append(new_line)

    return out_lines,changed_lines, no_change
            
def user_approve (old_file,new_file):
    """
    Give user approval to accept change
    """
    print("-"*60)
    print("Changed  >>> "+new_file.name)
    print("Original >>> "+old_file.name)
    print(" ")

    while True:
        choice = raw_input("Do you want to replace? (enter is yes)\n").lower()
        if choice in ('','y','yes'):
            print("replaced")
            os.system('mv '+new_file.name+" "+old_file.name)
            return

        if choice in ('no','n'):
            print("Didn't replace")
            os.system("rm "+new_file.name)
            return

        elif choice[0] == 'q':
            os.system("rm "+old_file.name)
            print("Did't replace, remove temporary file and exited")
            sys.exit(0)
            
        else:
            print("please press enter, 'n' or 'q' to quit")


def filelist_replace_string (filelist,old_str,new_str):
    if not (isinstance(old_str,str) or isinstance(new_str,str)):
        raise TypeError("Must get value as string")

    for fname in filelist:
        old_file = open(fname,'r')
        lines = old_file.readlines()
        old_file.close()
        out_lines, changed_lines, no_change = replace_value(lines,old_str,new_str)
        if no_change:
            print("No changes within file : "+old_file.name)
            continue
        print("\n==== changed lines =====>>>")        
        for i,line in enumerate(changed_lines):
            print("["+format(i,'3')+"]> "+line)
        new_file = open("TEMPORARY_"+fname,'w')
        for line in out_lines:
            new_file.write(line)
        new_file.close()
        user_approve(old_file,new_file)
        print(" ")

# ############################################################################ #
# SPECTRE specific
def SPECTRE_replace_string (old_str,new_str):
    """
    For all the *.f and *.com files take the old string and replace with 
    the new string. The user will be prompted to accept change
    """
    filelist = glob("*.f") + glob("*.com")
    filelist_replace_string(filelist,old_str,new_str)

def find_current_array_length ():
    """
    Use the SPECTRE file Dataval.com to find the current array length
    """
    dataval = open("Dataval.com")
    lines = dataval.readlines()
    dataval.close()
    array_re = re.compile("\.\ *x\((\d+)\)\,")
    N = None
    for line in lines:
        f = array_re.search(line)
        if f is None: 
            continue
        N = int(f.groups()[0])
    if N is None:
        # TODO: manual input of this
        raise StandardError("couldn't find current array length")
    return N


# ############################################################################ #
if __name__ == '__main__':
    old_str = str(find_current_array_length())
    print("Current length of the array is "+old_str)
    while True:
        new_n = raw_input("Please give new integer length of the array: ")
        
        try:
            new_n = int(new_n)
        except ValueError:
            print("Please give an integer")
            continue
            
        new_str= str(2**int(math.log(new_n)/math.log(2.0)+0.5))
        print("new array length will be "+new_str)
        break
    SPECTRE_replace_string(old_str,new_str)
    print("Done!")
        
    

        
    
