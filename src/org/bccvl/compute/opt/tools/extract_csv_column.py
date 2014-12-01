#!/usr/bin/env python

import sys
import os
import csv

def main():
    from optparse import OptionParser
    parser = OptionParser()
    parser.add_option("--input", dest="input",
                      help="the input csv file", metavar="FILE")
    parser.add_option("--column", dest="column", type=int, default=0, help="The column to extract")
    parser.add_option("--output", dest="output", default=None,
                      help="output file. stdout if not supplied")

    (options, args) = parser.parse_args()
    
    of = sys.stdout if options.output == None else open(options.output, "w")
    with open(options.input, 'r') as csvfile:
        outputs=[vals[options.column] for vals in csv.reader(csvfile)] 
        of.write(",".join(outputs))

    of.close()

if __name__ == '__main__':
    main() 
